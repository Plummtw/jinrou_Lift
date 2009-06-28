package org.plummtw.jinrou.snippet

import scala.xml._
import net.liftweb._
import net.liftweb.mapper._
import http._
import js._
import util._
import S._
import SHtml._
import Helpers._

import org.plummtw.jinrou.model._
import org.plummtw.jinrou.enum._
import org.plummtw.jinrou.util._
import org.plummtw.jinrou.data._

class FrameController {
  def frameset (xhtml : Group) : NodeSeq = {
    val room_no = S.param("room_no").getOrElse("0")
    val room_id : Long =
      try { room_no.toLong } 
      catch { case e: Exception => 0 }    //val room        = Room.get(room_id)
    val room_list   = Room.findAll(By(Room.id, room_id))
    val room        = if (room_list.length == 0) null else room_list(0)
    
    // 參數
    
    if (room == null) {
      S.error(<b>找不到村莊</b>)
      S.redirectTo("main.html")
    }
    //if (room.status.is == RoomStatusEnum.ENDED.toString()) {
    //  S.error(<b>遊戲已經結束</b>)
    //  S.redirectTo("main.html")
    //}
    
    // 檢查是否登入
    var user_entry : UserEntry = null
    var user_role  : RoleData  = RoleNone
    
    val user_entry_no : String = S.getSessionAttribute("user_id").getOrElse("")
    if ((S.getSessionAttribute("room_id").getOrElse("") == room_no) && 
        (user_entry_no != "")){
      val user_entry_list   = UserEntry.findAll(By(UserEntry.id, user_entry_no.toLong), By(UserEntry.room_id, room_id))
      user_entry            = if (user_entry_list.length == 0) null else user_entry_list(0)
    }    

    if (user_entry == null) {
      S.error(<b>找不到對應的玩家</b>)
      S.redirectTo("main.html")
    }
    
    // 是否更新 Session 內容
    val auto_reload = S.param("auto_reload").getOrElse("0")
    if (auto_reload != "0")
      S.setSessionAttribute("auto_reload", auto_reload)

    val play_sound = S.param("play_sound").getOrElse("0")
    if (play_sound != "0")
      S.setSessionAttribute("play_sound", play_sound)

    val list_down = S.param("list_down").getOrElse("0")
    if (list_down != "0")
      S.setSessionAttribute("list_down", list_down)

    val dead_mode = (!user_entry.live.is) && (room.status.is != RoomStatusEnum.ENDED.toString)

    if (dead_mode)
      <frameset rows="80,*,20%" border="2" frameborder="1" framespacing="1" bordercolor="silver" >
        <frame id="up" name="up" src={"up_normal.html?room_no=" + room_no} />
        <frame id="middle" name="middle" src={"game_view.html?room_no=" + room_no} />
        <frame id="bottom" name="bottom" src={"heaven_view.html?room_no=" + room_no} />
        <noframes>
          <h3>不支援無框頁的瀏覽器</h3>
        </noframes>
      </frameset>          
    else if(room.status.is == RoomStatusEnum.ENDED.toString)
      <frameset rows="80,*" border="1" frameborder="1" framespacing="1" >
        <frame id="up" name="up" src={"up_normal.html?room_no=" + room_no} />
        <frame id="bottom" name="bottom" src={"game_end.html?room_no=" + room_no} />
        <noframes>
          <h3>不支援無框頁的瀏覽器</h3>
        </noframes>
      </frameset>
    else
      <frameset rows="80,*" border="1" frameborder="1" framespacing="1" >
        <frame id="up" name="up" src={"up_normal.html?room_no=" + room_no} />
        <frame id="bottom" name="bottom" src={"game_view.html?room_no=" + room_no} />
        <noframes>
          <h3>不支援無框頁的瀏覽器</h3>
        </noframes>
      </frameset>          
  }
}