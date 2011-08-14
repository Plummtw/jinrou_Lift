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

class OldlogController {
  def next (xhtml : Group) : NodeSeq = {
    val page_no : Int =
      try { S.param("page_no").getOrElse("0").toInt }
      catch { case e: Exception => 0 }

    <a href={"oldlog_list.html?page_no=" + (page_no+1).toString}>下一頁</a>
  }
  
  def list (xhtml : Group) : NodeSeq = {
    val page_no : Int = 
      try { S.param("page_no").getOrElse("0").toInt }
      catch { case e: Exception => 0 }

    def victory_text(victory: String) = RoomVictoryEnum.valueOf(victory).getOrElse(RoomVictoryEnum.NONE) match
    {
      case RoomVictoryEnum.VILLAGER_WIN => "村"
      case RoomVictoryEnum.WEREWOLF_WIN => "狼"
      case RoomVictoryEnum.FOX_WIN      => "狐"
      case RoomVictoryEnum.FOX_WIN2     => "狐"
      case RoomVictoryEnum.DEMON_WIN    => "惡"
      case RoomVictoryEnum.PENGUIN_WIN  => "企"
      case RoomVictoryEnum.PONTIFF_WIN  => "教"
      case RoomVictoryEnum.MOB_WIN      => "暴"
      case RoomVictoryEnum.MOB_WIN2     => "暴"
      case RoomVictoryEnum.LOVER_WIN    => "戀"
      case RoomVictoryEnum.ABANDONED    => "棄"
      case RoomVictoryEnum.DRAW         => "和"
      case xs                           => "？"
    }

      
    val room_list = Room.findAll(By(Room.status, RoomStatusEnum.ENDED.toString),
                                 OrderBy(Room.id,Descending),
                                 StartAt(page_no * 20),
                                 MaxRows(20))
    val last_page = if (page_no == 0) <span></span>
                    else <a href={"oldlog_list.html?page_no=" + (page_no-1).toString}>上一頁</a>
    val next_page = if (room_list.length != 20) <span></span>
                    else <a href={"oldlog_list.html?page_no=" + (page_no+1).toString}>下一頁</a>
	val last_page_olg = if (page_no == 0) <span></span>
                    else <a href={"oldlog_list_olg.html?page_no=" + (page_no-1).toString}>上一頁</a>
    val next_page_olg = if (room_list.length != 20) <span></span>
                    else <a href={"oldlog_list_olg.html?page_no=" + (page_no+1).toString}>下一頁</a>

    val room_table = <table border="0" cellpadding="0" cellspacing="0">
      <tr>
        <th class="column">村No</th>
        <th class="column">村名</th>
        <th class="column">村莊說明</th>
        <th class="column">人數</th>
        <th class="column">勝</th>
        <th class="column">選項</th>
      </tr>
      { for (room <- room_list) yield 
      <tr> 
        <td align="right" valign="middle" class="row">{room.id.is.toString}</td> 
        <td align="right" valign="middle" class="row">
         <a href={"oldlog_view.html?room_no="+room.id.is.toString}>{room.room_name.is} 村</a>
        </td> 
        <td align="right" valign="middle" class="row"><small>～ {room.room_comment.is.toString} ～</small></td> 
        <td align="center" valign="middle" class="row">[ {room.max_user.is.toString}人用 ]</td> 
        <td align="center" valign="middle" class="row">[{victory_text(room.victory.is.toString)}]</td>
        <td valign="middle" class="row"><small>{room.option_text}</small></td>
      </tr>
      }
    </table> 
    bind("entry", xhtml,
      "last_page"  ->  last_page,
      "next_page"  ->  next_page,
	  "last_page_olg"  ->  last_page_olg,
      "next_page_olg"  ->  next_page_olg,
      "room_table" ->  room_table
    )
  }

  // 過去紀錄觀看頁面
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
      S.redirectTo("main.html")
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
    //var message_refresh        = false
    //var force_reload           = false
    //var up_force_reload        = false
    var user_entry : UserEntry = null
    //var user_role  : RoleData  = RoleNone
    
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
    
    val game_title : NodeSeq = Seq(<span style="text-decoration:underline;"><strong style="font-size:15pt;">{room.room_name.is} 村</strong>
         　～{room.room_comment.is}～[{room_no}號村] <a href="main.html" target="_top">[離開]</a>
         </span>, <br/>)

    val user_entrys = UserEntry.findAll(By(UserEntry.room_id, room_id))

    val gameend_info : NodeSeq = <table border="0" cellpadding="10" cellspacing="15" width="100%">
       <tr>
        {
          RoomVictoryEnum.valueOf(room.victory.is).getOrElse(RoomVictoryEnum.NONE) match {
            case RoomVictoryEnum.VILLAGER_WIN => <td valign="middle" align="center" width="100%" style="background-color:snow;color:black;font-weight:bold;"><img src="icon/hum.gif"/> [村民勝利] 村民把狼人全部殺死了</td>
            case RoomVictoryEnum.WEREWOLF_WIN => <td valign="middle" align="center" width="100%" style="background-color:#CC0000;color:snow;font-weight:bold;"><img src="icon/wlf.gif"/> [人狼・狂人勝利] 幾乎吃掉所有村民</td>
            case RoomVictoryEnum.FOX_WIN      => <td valign="middle" align="center" width="100%" style="background-color:#CC0099;color:snow;font-weight:bold;"><img src="icon/fox.gif"/> [妖狐勝利] 人狼已經被殺光、已經沒有敵人了</td>
            case RoomVictoryEnum.FOX_WIN2     => <td valign="middle" align="center" width="100%" style="background-color:#CC0099;color:snow;font-weight:bold;"><img src="icon/fox.gif"/> [妖狐勝利] 人狼和村民都被騙了</td>
            case RoomVictoryEnum.DEMON_WIN    => <td valign="middle" align="center" width="100%" style="background-color:#666666;color:snow;font-weight:bold;"><img src="icon/mag.gif"/> [惡魔勝利] 儀式完成、村莊毀滅了</td>
            case RoomVictoryEnum.PENGUIN_WIN  => <td valign="middle" align="center" width="100%" style="background-color:#CCFFFF;color:black;font-weight:bold;"><img src="icon/nec.gif"/> [企鵝勝利] 村莊進入極地氣候</td>
            case RoomVictoryEnum.PONTIFF_WIN  => <td valign="middle" align="center" width="100%" style="background-color:#EEAA55;color:snow;font-weight:bold;"><img src="icon/nob.gif"/> [教主勝利] 村莊納入教派管轄</td>
            case RoomVictoryEnum.MOB_WIN      => <td valign="middle" align="center" width="100%" style="background-color:#AAAAAA;color:snow;font-weight:bold;"><img src="icon/spy.gif"/> [暴民勝利] 獨一無二的暴君誕生了</td>
            case RoomVictoryEnum.MOB_WIN2     => <td valign="middle" align="center" width="100%" style="background-color:#AAAAAA;color:snow;font-weight:bold;"><img src="icon/spy.gif"/> [暴民勝利] 村莊陷入混亂狀態</td>
            case RoomVictoryEnum.LOVER_WIN    => <td valign="middle" align="center" width="100%" style="background-color:#FF69B4;color:snow;font-weight:bold;"><img src="icon/fre.gif"/> [戀人勝利] 等這村莊結束之後，我們就要回老家結婚了</td>            case RoomVictoryEnum.ABANDONED    => <td valign="middle" align="center" width="100%" style="background-color:snow;color:black;font-weight:bold;">這個村莊已經廢棄</td>
            case RoomVictoryEnum.DRAW         => <td valign="middle" align="center" width="100%" style="background-color:snow;color:black;font-weight:bold;">投票五次相同平手</td>
            case xs                           => <td valign="middle" align="center" width="100%" style="background-color:snow;color:black;font-weight:bold;">遊戲狀況不明</td>
          }
        }
       </tr>
      </table>
      // <td valign=middle align=center width=100% style="background-color:#FFFF99;color:black;font-weight:bold;">您獲勝了</td>
          
    // 更新 Cache 機制未完成
    // (room_day.day_no.is == 0)
    //def user_table = GameProcessCache.get_cache(room_id, "user_table", true,
    //  ()=>UserEntryHelper.user_table(room, user_entrys, true))
    val user_table = UserEntryHelper.user_table(room, null, user_entrys, true)

    //val room_days = RoomDay.findAll(By(RoomDay.room_id, room_id), OrderBy(RoomDay.day_no, Descending))

    def old_last_words_tag(deaths: List[SystemMessage]) = MiscHelper.get_lastwords_tag(deaths, user_entrys)

    def old_dead_tag(deaths: List[SystemMessage]) = MiscHelper.get_dead_tag(deaths, user_entrys)

    def old_votes_tag(old_room_day:RoomDay) = VoteHelper.get_vote_tag(room, old_room_day, user_entrys)

    def old_message(old_room_day:RoomDay) = 
      MessageHelper.messages_all(room, old_room_day, user_entry, user_entrys)

    def old_logs : NodeSeq = {
      var result : NodeSeq = Seq()
      room_day_list.foreach{old_room_day =>
        val deaths = SystemMessage.findAll(By(SystemMessage.roomday_id, old_room_day.id.is),  Like(SystemMessage.mtype, MTypeEnum.DEATH.toString + "%"),
                                           OrderBy(SystemMessage.actioner_id, Descending))
        result = result ++ Seq(<table border="0" cellpadding="0" cellspacing="0" style="font-family:新細明體;">
          <tr>{old_last_words_tag(deaths)}</tr>
          <tr>{old_dead_tag(deaths)}</tr>
          <tr>{old_votes_tag(old_room_day)}</tr>
        </table>,
        <table border="0" cellpadding="0" cellspacing="0" style="font-family:新細明體;">
          {old_message(old_room_day)}
        </table>)
      }
      result
    }

    bind("entry", xhtml,
      //"css_style"         -> <style type="text/css">{Unparsed(css_style)}</style>,
      "game_title"        -> game_title,
      //"game_option"       -> game_option,
      //"game_info"         -> game_info,
      "gameend_info"      -> gameend_info,
      //"day_no"            -> <span>{((room_day.day_no.is+2)/2).toString}</span>,
      "live_player"       -> <span>{user_entrys.count(_.live.is).toString}</span>,
      "user_table"        -> user_table,
      "old_logs"          -> old_logs
    )
  }

  // 單日觀看頁面
  def view_single (xhtml : Group) : NodeSeq = {
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

    val room_day_no = S.param("roomday_no").getOrElse("0")
    val room_day_no_int : Int =
      try { room_day_no.toInt }
      catch { case e: Exception => 0 }    //val room        = Room.get(room_id)


    val room_day_list   = RoomDay.findAll(By(RoomDay.room_id, room_id), By(RoomDay.day_no, room_day_no_int))
    val room_day        = if (room_day_list.length == 0) null else room_day_list(0)
    if (room_day == null) {
      S.error(<b>找不到遊戲日</b>)
      S.redirectTo("main.html")
    }

    // 設定顏色
    val (background_color, text_color) = JinrouUtil.color_helper(room, room_day)
    val css_style = JinrouUtil.css_style(background_color, text_color)

    // 檢查是否登入
    //var message_refresh        = false
    //var force_reload           = false
    //var up_force_reload        = false
    var user_entry : UserEntry = null
    //var user_role  : RoleData  = RoleNone

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
    val heaven_mode  = (room.status.is == RoomStatusEnum.ENDED.toString) ||
                       ((room.status.is == RoomStatusEnum.PLAYING.toString) &&
                        (room.has_flag(RoomFlagEnum.TEST_MODE))) ||
                       ((user_entry != null) && (!user_entry.live.is) &&
                        (room.has_flag(RoomFlagEnum.DEATH_LOOK)))
    if (!heaven_mode) {
      S.redirectTo("main.html")
    }

    val user_entrys = UserEntry.findAll(By(UserEntry.room_id, room_id))


    def old_last_words_tag(deaths: List[SystemMessage]) = MiscHelper.get_lastwords_tag(deaths, user_entrys)

    def old_dead_tag(deaths: List[SystemMessage]) = MiscHelper.get_dead_tag(deaths, user_entrys)

    def old_votes_tag(old_room_day:RoomDay) = VoteHelper.get_vote_tag(room, old_room_day, user_entrys)

    def old_message(old_room_day:RoomDay) =
      MessageHelper.messages_all(room, old_room_day, user_entry, user_entrys)

    def old_logs : NodeSeq = {
      var result : NodeSeq = Seq()
      val deaths = SystemMessage.findAll(By(SystemMessage.roomday_id, room_day.id.is),  Like(SystemMessage.mtype, MTypeEnum.DEATH.toString + "%"),
                                         OrderBy(SystemMessage.actioner_id, Descending))
      result = result ++ Seq(<table border="0" cellpadding="0" cellspacing="0" style="font-family:新細明體;">
        <tr>{old_last_words_tag(deaths)}</tr>
        <tr>{old_dead_tag(deaths)}</tr>
        <tr>{old_votes_tag(room_day)}</tr>
      </table>,
      <table border="0" cellpadding="0" cellspacing="0" style="font-family:新細明體;">
        {old_message(room_day)}
      </table>)
      result
    }

    bind("entry", xhtml,
      //"css_style"         -> <style type="text/css">{Unparsed(css_style)}</style>,
      //"game_title"        -> game_title,
      //"game_option"       -> game_option,
      //"game_info"         -> game_info,
      //"gameend_info"      -> gameend_info,
      //"day_no"            -> <span>{((room_day.day_no.is+2)/2).toString}</span>,
      //"live_player"       -> <span>{user_entrys.count(_.live.is).toString}</span>,
      //"user_table"        -> user_table,
      "old_logs"          -> old_logs
    )
  }
}

