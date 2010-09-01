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

class HeavenController {
  private def js_force_reload(room_no : String) : String = {
    """function force_reload() {
         top.location.href = 'game_frame.html?room_no=""" + room_no + """'
    }"""
  }

  // 產生 Javascript 字串 onload_string
  private def js_onload_string(force_reload: Boolean) : String = {
    val force_reload_str    = if (force_reload)    "force_reload();" else ""

    """window.onload=function() {
      """ + force_reload_str +  """
    };"""
  }

// 主畫面觀看頁面
  def view (xhtml : Group) : NodeSeq = {
    val room_no = S.param("room_no").getOrElse(S.getSessionAttribute("room_id").getOrElse("0"))
    val room_id : Long =
      try { room_no.toLong } 
      catch { case e: Exception => 0 }    //val room        = Room.get(room_id)
    val room_list   = Room.findAll(By(Room.id, room_id))
    val room        = if (room_list.length == 0) null else room_list(0)
    
    // 參數
    var force_reload = false
    
    if (room == null) {
      S.error(<b>找不到村莊</b>)
      S.redirectTo("main.html")
    }
    if (room.status.is == RoomStatusEnum.ENDED.toString()) {
      //S.error(<b>遊戲已經結束</b>)
      //S.redirectTo("game_view.html?room_no=" + room_no)
      force_reload = true
    }
    
    val room_day_list   = RoomDay.findAll(By(RoomDay.room_id, room_id), OrderBy(RoomDay.day_no, Descending))
    val room_day        = if (room_day_list.length == 0) null else room_day_list(0)
    if (room_day == null) {
      S.error(<b>找不到遊戲日</b>)
      S.redirectTo("main.html")
    }
    
    // 設定顏色
    val (background_color, text_color) = JinrouUtil.color_helper(room, room_day)
    val css_style = JinrouUtil.css_style(background_color, text_color)
    
    // 檢查是否登入
    var message_refresh        = false
    var user_entry : UserEntry = null
    var user_role  : RoleData  = RoleNone
    
    val user_entry_no : String = S.getSessionAttribute("user_id").getOrElse("")
    if ((S.getSessionAttribute("room_id").getOrElse("") == room_no) && 
        (user_entry_no != "")){
      val user_entry_list   = UserEntry.findAll(By(UserEntry.id, user_entry_no.toLong), By(UserEntry.room_id, room_id))
      user_entry            = if (user_entry_list.length == 0) null else user_entry_list(0)
    }
      
    if (user_entry == null) {
      S.error(<b>找不到對應的玩家 {user_entry_no}</b>)
      S.redirectTo("main.html")
    }

    val user_entrys = UserEntry.findAll(By(UserEntry.room_id, room_id))
    //def heaven_messages     = GameProcessCache.get_cache(room_id, "heaven_messages", true,
    //  ()=>MessageHelper.messages_heaven(room, room_day, user_entry, user_entrys))

    val heaven_mode  = (room.status.is == RoomStatusEnum.ENDED.toString) ||
                     ((room.status.is == RoomStatusEnum.PLAYING.toString) &&
                      (room.has_flag(RoomFlagEnum.TEST_MODE))) ||
                     ((user_entry != null) && (!user_entry.live.is) &&
                      (room.has_flag(RoomFlagEnum.DEATH_LOOK)))

    val old_logs = 
      if (!heaven_mode)
        <span></span>
      else
        for (i <- (1 to room_day.day_no.is -1).reverse) yield
        { <a href={"oldlog_view_single.html?room_no=" + room_no + "&roomday_no=" + i} target="_new">{((i+2)/2).toString}{if (i%2==0) "日" else " 夜"}</a> }

    def heaven_messages     =
      MessageHelper.messages_heaven(room, room_day, user_entry, user_entrys)
    
    bind("entry", xhtml,
      "reload_tag"        -> <meta http-equiv="refresh" content={S.getSessionAttribute("auto_reload").getOrElse("")} />,
      "css_style"         -> <style type="text/css">{Unparsed(css_style)}</style>,
      "js_force_reload"   -> <script type="text/javascript">{Unparsed(js_force_reload(room_no))}</script>,
      "js_onload_string"  -> <script type="text/javascript">{Unparsed(js_onload_string(force_reload))}</script>,
      "old_logs"          -> old_logs,
      "heaven_messages"   -> heaven_messages
    )
  }
}
/*
     <table border="0" cellpadding="0" cellspacing="0" style="font-size:12pt;font-family:新細明體;">
       <%
         if (talks != null) { 
           talks.each { talk -> 
             switch(talk.location) {
               case 'H':  %><tr><td width=200 align=left valign=middle style="border-bottom: silver 1px dashed;"><font color=${talk.user_entry.user_icon.color}>◆</font>${talk.user_entry.handle_name}<small>(${talk.user_entry.uname})</small> </td><td><span style="margin:1px;" align=left></span></td><td width=1000 valign=middle style="border-bottom: silver 1px dashed;"><span style="font-size:${talk.font_type}pt;<%if (talk.font_type=='20') { %>font-weight:bold;<% } %>"> ${talk.sentence} </span></td></tr><% break;
             }
           }
         }
       %>     
     <tr>
     </tr>
     </table>
*/