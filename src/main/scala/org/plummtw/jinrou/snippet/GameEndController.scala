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

class GameEndController {
  // 產生 Javascript 字串 force_reload
  private def js_force_reload(room_no : String) : String = {
    """function force_reload() {
         top.location.href = 'game_frame.html?room_no=""" + room_no + """'
    }"""
  }

  private def js_up_force_reload(room_no : String) : String = {
    """function up_force_reload() {
         parent.frames['up'].location.href = 'up_normal.html?room_no=""" + room_no + """'
    }"""
  }

  // 產生 Javascript 字串 onload_string
/*
  private def js_onload_string(force_reload: Boolean, up_force_reload: Boolean) : String = {
    val force_reload_str = if (force_reload) "force_reload();" else ""
    val up_force_reload_str = if (up_force_reload) "up_force_reload();" else ""

    """window.onload=function() {
      """ + force_reload_str + up_force_reload_str + """
      realtime_output();
    };"""
  }
*/
  private def js_onload_string(force_reload: Boolean, up_force_reload: Boolean) : String = {
    val force_reload_str    = if (force_reload)    "force_reload();" else ""
    val up_force_reload_str = if (up_force_reload) "up_force_reload();" else ""
  
    """$(document).ready(function(){
        """ + force_reload_str + up_force_reload_str + """
      });"""
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
    
    if (room == null) {
      S.error(<b>找不到村莊</b>)
      S.redirectTo("main.html")
    }
    if (room.status.is != RoomStatusEnum.ENDED.toString()) {
      S.error(<b>遊戲尚未結束</b>)
      S.redirectTo("game_view.html?room_no=" + room_no)
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
    var force_reload           = false
    var up_force_reload        = false
    var user_entry : UserEntry = null
    var user_role  : RoleData  = RoleNone
    
    val user_entry_no : String = S.getSessionAttribute("user_id").getOrElse("")
    if ((S.getSessionAttribute("room_id").getOrElse("") == room_no) && 
        (user_entry_no != "")){
      val user_entry_list   = UserEntry.findAll(By(UserEntry.id, user_entry_no.toLong), By(UserEntry.room_id, room_id))
      user_entry            = if (user_entry_list.length == 0) null else user_entry_list(0)
      
      if (user_entry == null) {
        S.error(<b>找不到對應的玩家 {user_entry_no}</b>)
        S.redirectTo("main.html")
      }
    }
    
    // 是否更新 Session 內容
    val auto_reload = S.param("auto_reload").getOrElse("0")
    if (auto_reload != "0")
      S.setSessionAttribute("auto_reload", auto_reload)
      
    val link_page = if (user_entry != null) "game_frame.html" else "game_end.html"
    
    val game_title_with_user2 : NodeSeq = 
             Seq(<a href="main.html" target="_top">[離開]</a>)
    val game_title_with_user : NodeSeq =  
             Seq(<a href={"logout.html?&room_no=" + room_no} target="_top">[登出]</a>) ++ game_title_with_user2  
             
    
    val game_title : NodeSeq = Seq(<span style="text-decoration:underline;"><strong style="font-size:15pt;">{room.room_name.is} 村</strong>
         　～{room.room_comment.is}～[{room_no}號村] 
         { 
           if (user_entry != null)
             game_title_with_user 
           else 
             Seq(<a href="main.html" target="_top">[離開]</a>)
         }</span>, <br/>)

    val game_option : NodeSeq = Seq(<small>[自動更新](
         <a href={link_page + "?room_no=" + room_no + "&auto_reload="} target="_top">手動</a>
         <a href={link_page + "?room_no=" + room_no + "&auto_reload=15"} target="_top">15秒</a>
         <a href={link_page + "?room_no=" + room_no + "&auto_reload=20"} target="_top">20秒</a>
         <a href={link_page + "?room_no=" + room_no + "&auto_reload=30"} target="_top">30秒</a>)
         </small>)

    val user_entrys = UserEntry.findAll(By(UserEntry.room_id, room_id))
    val live_pontiff  = user_entrys.filter(x=>(x.current_role == RoleEnum.PONTIFF) && (x.live.is))
    val user_victory  = if (user_entry == null)
                          ""
                        else if (((live_pontiff.length != 0) &&
                                 (user_entry.has_flag(UserEntryFlagEnum.RELIGION))) ||
                                 (user_entry.subrole.is == SubroleEnum.SUBPONTIFF.toString))
                          RoomVictoryEnum.PONTIFF_WIN.toString
                        else if ((user_entry.current_role == RoleEnum.FOX) ||
                                 (user_entry.current_role == RoleEnum.DEMON) ||
                                 (user_entry.current_role == RoleEnum.PONTIFF) ||
                                 (user_entry.current_role == RoleEnum.PENGUIN))
                          RoleEnum.get_role(user_entry.current_role).role_side.toString
                        else if (user_entry.subrole.is == SubroleEnum.WOLFBELIEVER.toString)
                          RoomVictoryEnum.WEREWOLF_WIN.toString
                        else if (user_entry.subrole.is == SubroleEnum.FOXBELIEVER.toString)
                          RoomVictoryEnum.FOX_WIN.toString
                        else if (user_entry.has_flag(UserEntryFlagEnum.BECAME_MOB) && 
                                 (RoleEnum.get_role(user_entry.current_role).role_side == RoomVictoryEnum.VILLAGER_WIN))
                          RoomVictoryEnum.MOB_WIN.toString
                        else if (room.has_flag(RoomFlagEnum.INHERITER_NEUTRAL) &&
                                 (user_entry.current_role == RoleEnum.INHERITER))
                          RoomVictoryEnum.NONE.toString
                        else
                          RoleEnum.get_role(user_entry.current_role).role_side.toString

    val gameend_info : NodeSeq = <table border="0" cellpadding="10" cellspacing="15" width="100%">
       <tr>
        {
          RoomVictoryEnum.valueOf(room.victory.is).getOrElse(RoomVictoryEnum.NONE) match {
            case RoomVictoryEnum.VILLAGER_WIN => <td valign="middle" align="center" width="100%" style="background-color:snow;color:black;font-weight:bold;">[村民勝利] 村民把狼人全部殺死了</td>
            case RoomVictoryEnum.WEREWOLF_WIN => <td valign="middle" align="center" width="100%" style="background-color:#CC0000;color:snow;font-weight:bold;">[人狼・狂人勝利] 幾乎吃掉所有村民</td>
            case RoomVictoryEnum.FOX_WIN      => <td valign="middle" align="center" width="100%" style="background-color:#CC0099;color:snow;font-weight:bold;">[妖狐勝利] 人狼已經被殺光、已經沒有敵人了</td>
            case RoomVictoryEnum.FOX_WIN2     => <td valign="middle" align="center" width="100%" style="background-color:#CC0099;color:snow;font-weight:bold;">[妖狐勝利] 人狼和村民都被騙了</td>
            case RoomVictoryEnum.DEMON_WIN    => <td valign="middle" align="center" width="100%" style="background-color:#666666;color:snow;font-weight:bold;">[惡魔勝利] 儀式完成、村莊毀滅了</td>
            case RoomVictoryEnum.PENGUIN_WIN  => <td valign="middle" align="center" width="100%" style="background-color:#CCFFFF;color:snow;font-weight:bold;">[企鵝勝利] 村莊進入極地氣候</td>
            case RoomVictoryEnum.PONTIFF_WIN  => <td valign="middle" align="center" width="100%" style="background-color:#EEAA55;color:snow;font-weight:bold;">[教主勝利] 村莊納入教派管轄</td>
            case RoomVictoryEnum.MOB_WIN      => <td valign="middle" align="center" width="100%" style="background-color:#AAAAAA;color:snow;font-weight:bold;">[暴民勝利] 獨一無二的暴君誕生了</td>
            case RoomVictoryEnum.MOB_WIN2     => <td valign="middle" align="center" width="100%" style="background-color:#AAAAAA;color:snow;font-weight:bold;">[暴民勝利] 村莊陷入混亂狀態</td>
            case RoomVictoryEnum.ABANDONED    => <td valign="middle" align="center" width="100%" style="background-color:snow;color:black;font-weight:bold;">這個村莊已經廢棄</td>
            case RoomVictoryEnum.DRAW         => <td valign="middle" align="center" width="100%" style="background-color:snow;color:black;font-weight:bold;">投票十次相同平手</td>
            case xs                           => <td valign="middle" align="center" width="100%" style="background-color:snow;color:black;font-weight:bold;">遊戲狀況不明</td>
          }
        }
       </tr>
       <tr>
        {
          if ((room.victory.is == RoomVictoryEnum.ABANDONED.toString) ||
              (room.victory.is == RoomVictoryEnum.DRAW.toString) ||
              (room.victory.is == RoomVictoryEnum.NONE.toString) ||
              (user_victory == ""))
            <td></td>
          else if ((user_victory == room.victory.is) ||
                   ((user_victory == RoomVictoryEnum.FOX_WIN.toString) &&
                    (room.victory.is == RoomVictoryEnum.FOX_WIN2.toString)) ||
                   ((user_victory == RoomVictoryEnum.MOB_WIN.toString) &&
                    (room.victory.is == RoomVictoryEnum.MOB_WIN2.toString)) ||
                   ((room.has_flag(RoomFlagEnum.INHERITER_NEUTRAL)) &&
                    (user_entry != null) && (user_entry.current_role == RoleEnum.INHERITER) &&
                    (user_entry.subrole.is != SubroleEnum.WOLFBELIEVER.toString) &&
                    (user_entry.subrole.is != SubroleEnum.FOXBELIEVER.toString) &&
                    (user_entry.subrole.is != SubroleEnum.SUBPONTIFF.toString) &&
                    (user_entry.live.is)) || 
                   ((user_entry != null) && (user_entry.current_role == RoleEnum.DEMON) &&
                    (room.victory.is == RoomVictoryEnum.DEMON_WIN.toString)) ||
                   ((user_entry != null) && (user_entry.current_role == RoleEnum.PENGUIN) &&
                    (room.victory.is == RoomVictoryEnum.PENGUIN_WIN.toString)))
            <td valign="middle" align="center" width="100%" style="background-color:#FFFF99;color:black;font-weight:bold;">您獲勝了</td>
          else
            <td valign="middle" align="center" width="100%" style="background-color:black;color:#FFFF99;font-weight:bold;">您已經輸了</td>
        }
       </tr>
      </table>
      // <td valign=middle align=center width=100% style="background-color:#FFFF99;color:black;font-weight:bold;">您獲勝了</td>
          
    // 更新 Cache 機制未完成
    // (room_day.day_no.is == 0)
    //def user_table = GameProcessCache.get_cache(room_id, "user_table", true,
    //  ()=>UserEntryHelper.user_table(user_entrys, true))
    def user_table =
      UserEntryHelper.user_table(room, user_entrys, true)

    // 這邊可能靈界和正常看到的不同，要準備兩份 Cache 先不弄    
    // (room_day.day_no.is %2 == 1) || message_refresh
    //def gameend_messages = GameProcessCache.get_cache(room_id, "gameend_messages", true,
    //  ()=>MessageHelper.messages_gameend(room, room_day, user_entry, user_entrys))
    def gameend_messages = 
      MessageHelper.messages_gameend(room, room_day, user_entry, user_entrys)

    if (user_entry != null) {
      if (user_entry.last_day_no.is != room_day.day_no.is) {
        user_entry.last_day_no(room_day.day_no.is)
        user_entry.save()

        if (room.status.is == RoomStatusEnum.ENDED.toString)
          force_reload = true
        else if (user_entry.live.is)
          up_force_reload = true
        else {
         val last_day        = RoomDay.findAll(By(RoomDay.room_id, room.id.is), By(RoomDay.day_no, room_day.day_no.is - 1))(0)
         val death_last_day  = SystemMessage.findAll(By(SystemMessage.roomday_id, last_day.id.is),  By(SystemMessage.actioner_id, user_entry.id.is),
                                                     Like(SystemMessage.mtype, MTypeEnum.DEATH.toString + "%"))
         if (death_last_day.length != 0)
           force_reload = true
        }
      }
    }

    bind("entry", xhtml,
      "reload_tag"        -> <meta http-equiv="refresh" content={S.getSessionAttribute("auto_reload").getOrElse("")} />,
      "css_style"         -> <style type="text/css">{Unparsed(css_style)}</style>,
      "js_force_reload"   -> <script language="JavaScript">{Unparsed(js_force_reload(room_no))}</script>,
      "js_up_force_reload"   -> <script language="JavaScript">{Unparsed(js_up_force_reload(room_no))}</script>,
      "js_onload_string"  -> <script language="JavaScript">{Unparsed(js_onload_string(force_reload,up_force_reload))}</script>,
      "game_title"        -> game_title,
      "game_option"       -> game_option,
      //"game_info"         -> game_info,
      "gameend_info"      -> gameend_info,
      //"day_no"            -> <span>{((room_day.day_no.is+2)/2).toString}</span>,
      "live_player"       -> <span>{user_entrys.count(_.live.is).toString}</span>,
      "alert_message"     -> <span></span>,
      "user_table"        -> user_table,
      "gameend_messages"  -> gameend_messages,
    )
  }
}

