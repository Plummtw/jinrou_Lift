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

import java.util.{GregorianCalendar, Calendar}

// 村莊處理用 Lock
object GameProcessLock {
  val lock_hash = scala.collection.mutable.Map[Long, Object]()
  
  // 每個 村莊 給一個 Lock
  def get_lock(id: Long) : Object = {
    if (!lock_hash.contains(id)) {
      lock_hash(id) = new Object()
    } 
    
    return lock_hash(id)
  }
  
  def remove_lock(id: Long) = {
    lock_hash -= id
  }
}

// 村莊處理用 Cache
class GameProcessCacheData() {
  
}

object GameProcessCache {
  val room_cache = scala.collection.mutable.Map[Long, scala.collection.mutable.Map[String, NodeSeq]]()
  
  // 每個 村莊 給一個 
  def get_cache(id: Long, type_str: String, refresh: Boolean, generate_method: ()=>NodeSeq) : NodeSeq = {
    if (!room_cache.contains(id)) {
      room_cache(id) = new scala.collection.mutable.HashMap[String, NodeSeq]
    }
    
    val cache = room_cache(id)
    
    if ((refresh == true) || (!cache.contains(type_str))) {
      //println("generate cache " + type_str + " " + id.toString)
      cache(type_str) = generate_method()
      //println(cache(type_str).toString)
    }
    
    return cache(type_str)
  }
  
  def remove_cache(id: Long) = {
    room_cache -= id
  }
}

class GameController {
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

  /*
  // 產生 Javascript 字串 setupbgcolor
  private def js_setupbgcolor(background_color:String, text_color:String) : String = {
    """function setupbgcolor() { 
         parent.frames['up'].document.bgColor    = '""" + background_color + """'; 
         parent.frames['up'].document.fgColor    = '""" + text_color + """'; 
         parent.frames['up'].document.linkColor  = 'blue'; 
         parent.frames['up'].document.vlinkColor = 'blue'; 
         parent.frames['up'].document.alinkColor = 'red'; 
    }"""
  }
  
  // 產生 Javascript 字串 setvotelink
  private def js_setvotelink(room_no : String) : String = {
    ""
    /*"""function setvotelink() {
         for(i=0;i<parent.frames['up'].document.links.length;i++) { 
           if(parent.frames['up'].document.links[i].name == 'vote_link'){ 
             parent.frames['up'].document.links[i].href = 'game_action?room_no=""" + room_no +"""';  
             parent.frames['up'].document.links[i].hash ='game_top'  
           } 
         } 
    }""" */
  }
  */

  // 產生 Javascript 字串 realtime_output
  private def js_realtime_output(room_day: RoomDay, room: Room) : String = {
    def date_string(datetime : GregorianCalendar) : String  = {
      String.format("new Date(%s,%s,%s,%s,%s,%s)",
        datetime.get(Calendar.YEAR).toString, datetime.get(Calendar.MONTH).toString,
        datetime.get(Calendar.DAY_OF_MONTH).toString, datetime.get(Calendar.HOUR_OF_DAY).toString, 
        datetime.get(Calendar.MINUTE).toString, datetime.get(Calendar.SECOND).toString)      
    } 
  
    val s_datetime = new GregorianCalendar()
    val e_datetime = new GregorianCalendar()
    if (room_day.day_no.is == 0) {
       """function realtime_output() { 
         php_now = """ + date_string(s_datetime) + """; 
         local_now = new Date(); 
         diff_sec = Math.floor( (local_now - php_now) / 1000); 
         //document.realtime_form.realtime_output.value = "伺服器與本地時間差：" + diff_sec + "秒";
         $("#realtime_output").val("伺服器與本地時間差：" + diff_sec + "秒");
       }"""
    } else {
      s_datetime.setTime(room_day.created.is)
      e_datetime.setTime(room_day.created.is)
      
      var until_string = ""
      if (room_day.day_no.is % 2 == 0) {
        e_datetime.add(Calendar.MINUTE, room.day_minutes.is)
        until_string = "日落"
      } else {
        e_datetime.add(Calendar.MINUTE, room.night_minutes.is)
        until_string = "早上"
      }      
    
      """starttime = """ + date_string(s_datetime) + """;   
      endtime   = """ + date_string(e_datetime) + """;  
      diffseconds = Math.floor((endtime - starttime)/1000); 
      function realtime_output() { 
        nowtime = new Date(); 
        leftseconds = diffseconds - Math.floor((nowtime - starttime)/1000); 
        lefttime = new Date(0,0,0,0,0,leftseconds); 
        virtual_left_seconds = Math.floor(12*60*60*(leftseconds / diffseconds)); 
        virtual_lefttime = new Date(0,0,0,0,0,virtual_left_seconds); 
        if(leftseconds > 0){ 
          //document.realtime_form.realtime_output.value = "　""" + until_string + """剩餘 " + virtual_lefttime.getHours()+"時間"+virtual_lefttime.getMinutes()+"分 (實際時間 "+lefttime.getMinutes()+"分"+lefttime.getSeconds()+"秒)"; \n\
          $("#realtime_output").val("　""" + until_string + """剩餘 " + virtual_lefttime.getHours()+"時間"+virtual_lefttime.getMinutes()+"分 (實際時間 "+lefttime.getMinutes()+"分"+lefttime.getSeconds()+"秒)");
        } else { 
          overseconds = Math.abs(leftseconds);
          overtime = new Date(0,0,0,0,0,overseconds);
          //document.realtime_form.realtime_output.value = "超過時間 "+overtime.getMinutes()+"分"+overtime.getSeconds()+"秒"; \n\
          $("#realtime_output").val("超過時間 "+overtime.getMinutes()+"分"+overtime.getSeconds()+"秒");
        } 
        tid = setTimeout('realtime_output()', 1000); 
      }"""
    }
  }
  
  // 產生 Javascript 字串 onload_string
  private def js_onload_string(force_reload: Boolean, up_force_reload: Boolean) : String = {
    val force_reload_str    = if (force_reload)    "force_reload();" else ""
    val up_force_reload_str = if (up_force_reload) "up_force_reload();" else ""
  
    /*"""window.onload=function() {
      """ + force_reload_str + up_force_reload_str + """
      realtime_output();
    };"""*/

    """$(document).ready(function(){
        """ + force_reload_str + up_force_reload_str + """
        realtime_output();
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
    if (room.status.is == RoomStatusEnum.ENDED.toString()) {
      //S.error(<b>遊戲已經結束</b>)
      S.redirectTo("game_end.html?room_no=" + room_no)
    }
    
    val room_day_list   = RoomDay.findAll(By(RoomDay.room_id, room_id), OrderBy(RoomDay.day_no, Descending))
    val room_day        = if (room_day_list.length == 0) null else room_day_list(0)
    if (room_day == null) {
      S.error(<b>找不到遊戲日</b>)
      S.redirectTo("main.html")
    }
    //println("dead_line : " + room_day.deadline.is)

    // 設定顏色
    val (background_color, text_color) = JinrouUtil.color_helper(room, room_day)
    val css_style = JinrouUtil.css_style(background_color, text_color)
    
    // 檢查是否登入
    var message_refresh        = false
    var force_reload           = false
    var up_force_reload        = false
    var swf_filename           = ""
    var user_entry : UserEntry = null
    var user_role  : RoleData  = RoleNone
    
    val user_entry_no : String = S.getSessionAttribute("user_id").getOrElse("")
    if ((S.getSessionAttribute("room_id").getOrElse("") == room_no) && 
        (user_entry_no != "")){
      val user_entry_list   = UserEntry.findAll(By(UserEntry.id, user_entry_no.toLong), By(UserEntry.room_id, room_id))
      user_entry            = if (user_entry_list.length == 0) null else user_entry_list(0)
      
      if (user_entry == null) {
        S.setSessionAttribute("room_id", "")
        S.setSessionAttribute("user_id", "")
        S.error(<b>找不到對應的玩家 {user_entry_no}</b>)
        S.redirectTo("main.html")
      }
    }

    var alert_message =
      if (room_day.deadline.is == null)
        <span></span>
      else if (user_entry != null) {
        <span></span>
      } else
        <span></span>


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
    //println("list_down : " + list_down)
      
    val link_page = if (user_entry != null) "game_frame.html" else "game_view.html"
    
    val game_title_with_user2 : NodeSeq = 
             if (room_day.day_no.is == 0) 
                 Seq(<strong><a href={"go_out.html?room_no=" + room_no } target="_top">[自刪]</a></strong>,
                     <a href="main.html" target="_top">[離開]</a>)
             else 
                 //Seq(<a href="main.html" target="_top">[離開]</a>,
                 //    <span>{user_entry.check_voted(room_day,room)}</span>)
                 Seq(<a href="main.html" target="_top">[離開]</a>)
    val game_title_with_user : NodeSeq =  
             Seq(<a href={"logout.html?&room_no=" + room_no} target="_top">[登出]</a>) ++ game_title_with_user2  
             
    
    val game_title : NodeSeq = Seq(<span style="text-decoration:underline;"><strong style="font-size:15pt;">{room.room_name.is} 村</strong>
         　～{room.room_comment.is}～[{room_no}號村] 
         { 
           if (user_entry != null)
             game_title_with_user 
           else 
             Seq(<span></span>) 
         }</span>, <br/>)

    val play_sound_option : scala.xml.Elem =
      if (S.getSessionAttribute("play_sound") == "on")
        <span><a href={link_page + "?room_no=" + room_no + "&play_sound=off"}  target="_top">off</a> on</span>
      else
        <span>off <a href={link_page + "?room_no=" + room_no + "&play_sound=on"}  target="_top">on</a></span>

    val list_down_option : scala.xml.Elem =
      if (S.getSessionAttribute("list_down") == "on")
        <a href={link_page + "?room_no=" + room_no + "&list_down=off"}  target="_top">↑訊息</a>
      else
        <a href={link_page + "?room_no=" + room_no + "&list_down=on"}  target="_top">↓訊息</a>

    val game_option : NodeSeq = Seq(<small>[自動更新](
         <a href={link_page + "?room_no=" + room_no + "&auto_reload="} target="_top">手動</a>
         <a href={link_page + "?room_no=" + room_no + "&auto_reload=15"} target="_top">15秒</a>
         <a href={link_page + "?room_no=" + room_no + "&auto_reload=20"} target="_top">20秒</a>
         <a href={link_page + "?room_no=" + room_no + "&auto_reload=30"} target="_top">30秒</a>)
         [音效通知]({ play_sound_option })
         { list_down_option }
         </small>)

    def sound_object(swf_filename: String) =
      if ((swf_filename == "") || (S.getSessionAttribute("play_sound") != "on"))
        <span></span>
      else
        <object classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000" codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=4,0,0,0" width="0" height="0">
          <param name="movie" value={"swf/" + swf_filename} />
          <param name="quality" value="high" />
          <param name="loop" value="true" />
          <embed src={"swf/" + swf_filename} quality="high" width="0" height="0" loop="true" type="application/x-shockwave-flash" pluginspage="http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash">
          </embed>
        </object>
         
    val game_info : NodeSeq = if ((user_entry != null) && (room_day.day_no == 0)) 
         <table border="0" cellpadding="0" cellspacing="5" width="100%">
          <tr style="background-color:#009900;color:snow;font-weight:bold;">
           <td valign="middle" align="center" width="100%">需要遊戲全體人員投'開始遊戲'才能開始遊戲<small>(完成投票的玩家其名單背景顏色會變粉紅)</small></td>
          </tr>
         </table> else <span></span>
          
    val login_form : NodeSeq = if (user_entry == null) Seq(
         <form action={"login.html?room_no=" + room_no} method="POST">
          <tr>
           <td>
            <strong>帳號<input type="text" name="uname" size="20"/>密碼</strong>
            <input type="password" name="password" size="20" style="txt-align:rignt;ime-mode: disabled;" />
            <input type="submit" value="登入"/>
            { if (room_day.day_no.is == 0) 
                Seq(<a href={"regist?group=1&room_no=" + room_no}><strong>[住民登錄1]</strong></a>,
                    <a href={"regist?group=2&room_no=" + room_no}><strong>[住民登錄2]</strong></a>)
              else scala.xml.NodeSeq.Empty } 
             <a href="main.html"><strong>[離開]</strong></a>              
           </td>
          </tr>
         </form>) else Seq(<span></span>)
        
    val dead_mode    = (room.status.is != RoomStatusEnum.ENDED.toString) &&
                       (user_entry != null) && (!user_entry.live.is)
    val heaven_mode  = (room.status.is == RoomStatusEnum.ENDED.toString) ||
                     ((room.status.is == RoomStatusEnum.PLAYING.toString) && 
                      (room.room_flags.is.indexOf(RoomFlagEnum.TEST_MODE.toString) != -1)) ||
                     ((user_entry != null) && (!user_entry.live.is) && 
                      (room.room_flags.is.indexOf(RoomFlagEnum.DEATH_LOOK.toString) != -1))
    //println("heaven_mode " + heaven_mode.toString)

    // 更新 Cache 機制未完成
    // (room_day.day_no.is == 0)
    val user_entrys = UserEntry.findAll(By(UserEntry.room_id, room_id))

    if (GameProcesser.check_deadline(room, room_day, user_entrys)) {
      S.redirectTo("game_view.html?room_no=" + room_no)
    }


    //def user_table = GameProcessCache.get_cache(room_id, "user_table", true,
    //  ()=>UserEntryHelper.user_table(user_entrys, heaven_mode))
    var user_table_down : NodeSeq = NodeSeq.Empty
    var user_table =
      UserEntryHelper.user_table(room, user_entrys, heaven_mode)
    if (S.getSessionAttribute("list_down") == "on") {
      user_table_down = user_table
      user_table = NodeSeq.Empty
    }


    // 這邊可能靈界和正常看到的不同，要準備兩份 Cache 先不弄    
    // (room_day.day_no.is %2 == 1) || message_refresh
    //def messages     = GameProcessCache.get_cache(room_id, "messages", true,
    //  ()=>MessageHelper.messages_normal(room, room_day, user_entry, heaven_mode, user_entrys))
    val vote_godfat_blind = SystemMessage.findAll(By(SystemMessage.roomday_id, room_day.id.is),
//                                                  By(SystemMessage.actionee_id, user_entry.id.is),
                                                  By(SystemMessage.mtype, MTypeEnum.VOTE_BETRAYER_DISGUISE.toString))
    val is_blinded =
      if (vote_godfat_blind.length == 0)
        false
      else if (user_entry == null)
        true
      else if (((user_entry.test_memoryloss(room, room_day, user_entrys)) && (!(user_entry.test_fake(room_day)))
             && (((user_entry.role.is.substring(0,1) == RoleEnum.FOX.toString) || (user_entry.role.is.substring(0,1) == RoleEnum.BETRAYER.toString) || (user_entry.role.is.substring(0,1) == RoleEnum.GODFAT.toString) ||
                 (user_entry.subrole.is == SubroleEnum.FOXBELIEVER.toString))
             && (user_entry.subrole.is != SubroleEnum.WOLFBELIEVER.toString))) || (heaven_mode))
      //if (vote_godfat_blind.filter(_.actionee_id.is == user_entry.id.is).length == 0)
        false
      else
        true

    var messages     =
      MessageHelper.messages_normal(room, room_day, user_entry, heaven_mode, is_blinded, user_entrys)


    val role_intro = 
      if ((user_entry != null) && (user_entry.live.is) && (room_day.day_no.is != 0))
      {
        val role_data = RoleEnum.get_role(user_entry.role.is.substring(0,1))
        var result = Seq(role_data.generate_role_intro(room, room_day, user_entry, user_entrys),
                         role_data.generate_role_ability(room, room_day, user_entry, user_entrys))
                         
        val subrole_data = SubroleEnum.get_subrole(user_entry.subrole.is)
        //if (user_entry.subrole != SubroleNone)
        //if (user_entry.subrole.is == SubroleEnum.AUTHORITY.toString)
          result = result ++ Seq(subrole_data.subrole_intro)

        // 教徒時互相知道身分
        val live_pontiff = user_entrys.filter(x=>(x.current_role == RoleEnum.PONTIFF) && (x.live.is))

        // 可以得知所有教徒身份的人有三
        // 1. 副教主
        // 2. 非失憶教主
        // 3. 教徒有開教友會，且第七日之後
        val is_pontiff_view = (user_entry.subrole.is == SubroleEnum.SUBPONTIFF.toString) ||
                              ((user_entry.current_role == RoleEnum.PONTIFF)  &&
                               (!user_entry.test_memoryloss(room, room_day, user_entrys)) ||
                              ((live_pontiff.length != 0) &&
                               (user_entry.user_flags.is.indexOf(UserEntryFlagEnum.RELIGION.toString) != -1) &&
                               (room_day.day_no.is > 11) && (room.room_flags.is.indexOf(RoomFlagEnum.PONTIFF_OPTION1.toString) != -1)))

        if (is_pontiff_view) {
          val users_religion = user_entrys.filter(x=>(x.id.is != user_entry.id.is) &&
                                                    ((x.current_role == RoleEnum.PONTIFF) ||
                                                     (x.user_flags.is.indexOf(UserEntryFlagEnum.RELIGION.toString) != -1)))
          val religion_str = users_religion.map(_.handle_name.is).mkString("", "　","")

          result = result ++ Seq(<table cellSpacing="0" cellPadding="0" border="1"><tbody>
            <tr><td>和你同一教派的成員有：</td><td>{religion_str}</td></tr></tbody></table>)
        } else if ((live_pontiff.length != 0) &&
            ((user_entry.user_flags.is.indexOf(UserEntryFlagEnum.RELIGION.toString) != -1))){
          val users_religion = user_entrys.filter(x=>((x.current_role == RoleEnum.PONTIFF) ||
                                                     (x.user_flags.is.indexOf(UserEntryFlagEnum.RELIGION.toString) != -1)))
          val live_users_religion = users_religion.filter(_.live.is)

          result = result ++ Seq(<table cellSpacing="0" cellPadding="0" border="1"><tbody>
            <tr><td>教派成員總數：</td><td>{users_religion.length.toString}</td><td>存活教派成員總數：</td>
                <td>{live_users_religion.length.toString}</td></tr></tbody></table>)
        }

        result
      }
      else
        Seq(<span></span>)

    var vote_tag_d     : NodeSeq = NodeSeq.Empty
    var vote_tag_n     : NodeSeq = NodeSeq.Empty
    var last_words_tag : NodeSeq = NodeSeq.Empty
    var dead_tag       : NodeSeq = NodeSeq.Empty

    if (room_day.day_no.is >= 2)  {
      val last_day  = RoomDay.findAll(By(RoomDay.room_id, room.id.is), By(RoomDay.day_no, room_day.day_no.is - 1))(0)
      val last_day2 = RoomDay.findAll(By(RoomDay.room_id, room.id.is), By(RoomDay.day_no, room_day.day_no.is - 2))(0)

      // 產生投票字串
      val  vote_tag_pair     = VoteHelper.get_vote_tag(room, room_day, user_entrys, heaven_mode, last_day, last_day2)
      vote_tag_d = vote_tag_pair._1
      vote_tag_n = vote_tag_pair._2

      // 產生死亡訊息字串
      val death_last_day  = SystemMessage.findAll(By(SystemMessage.roomday_id, last_day.id.is),  Like(SystemMessage.mtype, MTypeEnum.DEATH.toString + "%"),
                                                  OrderBy(SystemMessage.actioner_id, Ascending))
      val death_last_day2 = SystemMessage.findAll(By(SystemMessage.roomday_id, last_day2.id.is), Like(SystemMessage.mtype, MTypeEnum.DEATH.toString + "%"),
                                                  OrderBy(SystemMessage.actioner_id, Ascending))
      val deaths = death_last_day++death_last_day2

      dead_tag   = MiscHelper.get_dead_tag(deaths, user_entrys)

      // 白天時產生遺言字串
      if (room_day.day_no.is % 2 == 0) {
        last_words_tag = MiscHelper.get_lastwords_tag(deaths, user_entrys)
      } else if (room_day.day_no.is >= 3) {
        // 晚上時產生前一天的遺言字串
        val last_day3 = RoomDay.findAll(By(RoomDay.room_id, room.id.is), By(RoomDay.day_no, room_day.day_no.is - 3))(0)
        val death_last_day3  = SystemMessage.findAll(By(SystemMessage.roomday_id, last_day3.id.is),  Like(SystemMessage.mtype, MTypeEnum.DEATH.toString + "%"),
                                                     OrderBy(SystemMessage.actioner_id, Ascending))
        val deaths_n = death_last_day2++death_last_day3

        last_words_tag   = MiscHelper.get_lastwords_tag(deaths_n, user_entrys)

      }
    }

    val self_lastwords_tag = MiscHelper.get_self_lastwords_tag(user_entry)

    if (user_entry != null) {
      if (user_entry.last_day_no.is != room_day.day_no.is) {
        user_entry.last_day_no(room_day.day_no.is)
        user_entry.save()

        if (room.status.is == RoomStatusEnum.ENDED.toString)
          force_reload = true
        else if (user_entry.live.is) {
          up_force_reload = true
          if (room_day.day_no.is %2 == 0)
             swf_filename = "sound_morning.swf"
        } else {
         val last_day        = RoomDay.findAll(By(RoomDay.room_id, room.id.is), By(RoomDay.day_no, room_day.day_no.is - 1))(0)
         val death_last_day  = SystemMessage.findAll(By(SystemMessage.roomday_id, last_day.id.is),  By(SystemMessage.actioner_id, user_entry.id.is),
                                                     Like(SystemMessage.mtype, MTypeEnum.DEATH.toString + "%"))
         if (death_last_day.length != 0)
           force_reload = true
        }
      }

      if (room_day.deadline.is != null)
        swf_filename = "sound_revote.swf"
    }

    bind("entry", xhtml,
      "reload_tag"        -> <meta http-equiv="refresh" content={S.getSessionAttribute("auto_reload").getOrElse("")} />,
      "css_style"         -> <style type="text/css">{Unparsed(css_style)}</style>,
      "js_force_reload"   -> <script type="text/javascript">{Unparsed(js_force_reload(room_no))}</script>,
      "js_up_force_reload"   -> <script type="text/javascript">{Unparsed(js_up_force_reload(room_no))}</script>,
      //"js_setupbgcolor"   -> <script type="text/javascript">{Unparsed(js_setupbgcolor(background_color, text_color))}</script>,
      //"js_setvotelink"    -> <script type="text/javascript">{Unparsed(js_setvotelink(room_no))}</script>,
      "js_realtimeoutput" -> <script type="text/javascript">{Unparsed(js_realtime_output(room_day, room))}</script>,
      "js_onload_string"  -> <script type="text/javascript">{Unparsed(js_onload_string(force_reload,up_force_reload))}</script>,
      "game_title"        -> game_title,
      "game_option"       -> game_option,
      "sound_object"      -> sound_object(swf_filename),
      "game_info"         -> game_info,
      "login_form"        -> login_form,
      "day_no"            -> <span>{((room_day.day_no.is+2)/2).toString}</span>,
      "weather"           -> <span>{WeatherEnum.get_weather(room_day.weather.is)}</span>,
      "live_player"       -> <span>{user_entrys.count(_.live.is).toString}</span>,
      "alert_message"     -> alert_message,
      "user_table"        -> user_table,
      "role_intro"        -> role_intro,
      "vote_tag_d"        -> vote_tag_d,
      "messages"          -> messages,
      "last_words_tag"    -> last_words_tag,
      "dead_tag"          -> dead_tag,
      "vote_tag_n"        -> vote_tag_n,
      "self_last_words"   -> self_lastwords_tag,
      "user_table_down"   -> user_table_down
    )
  }   

}

