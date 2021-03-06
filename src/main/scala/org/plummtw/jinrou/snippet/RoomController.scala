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

//import org.bone.splurk._
//import org.bone.splurk.constant._



// 創新村莊的 Lock，以免村莊數超過村莊上限
object RoomCreateLock {}

class RoomController {
  //val plurk_username = "YourUserName"
  //val plurk_password = "YourPassword"
  //val plurk_apiKey   = "YourAPIKey"


  def list_normal : NodeSeq = {
    var result : NodeSeq = Seq()
    val room_list  = Room.findAll(By(Room.status, RoomStatusEnum.WAITING.toString), OrderBy(Room.id,Descending)) :::
                     Room.findAll(By(Room.status, RoomStatusEnum.PLAYING.toString), OrderBy(Room.id,Descending))
                     
  
    for (val room <- room_list) result ++= Seq(
      <strong><a href={"login.html?room_no=" + room.id} style="color:#CC3300;">
      {
        if (room.status.is == RoomStatusEnum.WAITING.toString)
          <img src="images/waiting.gif" border="0"/>
        else
          <img src="images/playing.gif" border="0"/>
      }
      <small> [{room.id}號村] </small>{room.room_name}村<br/><small style="margin-left:100px;"><div align="right">～ {room.room_comment} ～
      [時間　白：{room.day_minutes}　夜：{room.night_minutes} ]
      [ {room.max_user}人用 ]
      </div></small></a></strong>, 
      <small>村莊選項：{room.option_text}</small>,
      <br/>, <br/>)
      
    return result
  }
  
  def add (xhtml : Group) : NodeSeq = {
    // 參數
    var room_name         = ""
    var room_comment      = ""
    var max_user          = "22"
    var day_minutes       = "5"
    var night_minutes     = "3"
    var dummy_handle_name = "伊藤誠"
    var dummy_last_words  = ""
    
    // var b                 = false

    // Game Options
    var option_list : List[RoomFlagEnum.Value] = List()
  
    def create_room () {
      val int_day_minutes   = 
        try { day_minutes.toInt } 
        catch { case e: Exception => 5 }

      val int_night_minutes = 
        try { night_minutes.toInt } 
        catch { case e: Exception => 3 }
        
      val int_max_user = 
        try { max_user.toInt } 
        catch { case e: Exception => 22 }

      if (option_list.contains(RoomFlagEnum.CLERIC_OPTION1))
        option_list = option_list.remove(_ == RoomFlagEnum.CLERIC_OPTION2)

      val room_flags : String= option_list.removeDuplicates.map(_.toString).mkString("",",","")
      
      val room = Room.create.room_name(room_name.replace('　',' ').trim()).room_comment(room_comment.replace('　',' ').trim()).max_user(int_max_user)
                     .day_minutes(int_day_minutes).night_minutes(int_night_minutes)
                     .room_flags(room_flags).status(RoomStatusEnum.WAITING.toString).victory("")

      var last_words = dummy_last_words.trim()
      //if (last_words.length > 200)
      //  last_words = dummy_last_words.trim().substring(0, 200)
      last_words = JinrouUtil.encodeHtml(last_words, UserEntry.last_words.maxLen)

      //if (last_words.length > 250)   // 太多控制碼了，程式不比對了，直接空白
      //  last_words = ""
      
      room.validate match {
        case Nil => ;
        case xs  => S.error(xs); return redirectTo("main.html")
      }
      
      // 加入替身君
      val dummy_boy = UserEntry.create.uname("dummy_boy").handle_name(dummy_handle_name.replace('　',' ').trim()).sex("M").user_icon_id(1)
      .password("dummy_boy").last_words(last_words).role(RoleEnum.NONE.toString).subrole("")
                      .user_flags(UserEntryFlagEnum.VOTED.toString)
                      .ip_address(S.request.map{x=>JinrouUtil.getIpAddress(x)}.openOr(""))
                      
      dummy_boy.validate match { 
        case Nil => ;
        case xs  => S.error(xs); return redirectTo("main.html")
      }                
      
      // 加入大廳
      val game_hall = RoomDay.create.day_no(0).vote_time(0).weather(WeatherEnum.SUNNY.toString)
      val room_params = AdminManage.findAll(Like(AdminManage.param_name, "room%"))

      val current_time =  new java.util.GregorianCalendar
      val current_hour =  current_time.get(java.util.Calendar.HOUR_OF_DAY)

      val room_start =
          try { room_params.filter(_.param_name.is == "room_start")(0).param_value.is.toInt }
          catch { case e: Exception => 0}
      val room_end =
          try { room_params.filter(_.param_name.is == "room_end")(0).param_value.is.toInt }
          catch { case e: Exception => 24}

      if ((current_hour >= room_end) || (current_hour< room_start)) {
        S.error((room_end-1).toString + ":00 - " + (room_start.toString) +":00 請不要開村")
        return redirectTo("main.html")
      }

      
      RoomCreateLock.synchronized {
      
        val room_count  = Room.count(By(Room.status, RoomStatusEnum.WAITING.toString)) +
                          Room.count(By(Room.status, RoomStatusEnum.PLAYING.toString))
        val room_count_limit =
          try { room_params.filter(_.param_name.is == "room_count")(0).param_value.is.toInt }
          catch { case e: Exception => 3}

        if (room_count >= room_count_limit) {
          S.error("超過村數上限"); return redirectTo("main.html")
        }
                          
        room.save()

        if (!room.has_flag(RoomFlagEnum.NO_DUMMY)) {
          dummy_boy.room_id(room.id.is)
          dummy_boy.save()
        }
        game_hall.room_id(room.id.is)
        game_hall.save()
      }

      /*
      try {
        val plurk_client = new PlurkClient(plurk_apiKey)     // 建立 SPlurk 物件
        plurk_client.Users.login (plurk_username, plurk_password)  // 登入噗浪

        // 發噗
        plurk_client.Timeline.plurkAdd (
          qualifier = Qualifier.Says,  // 設定噗文前的修飾詞（說、喜歡、正在……等）
          content   = "第" + room.id.is.toString + "號村已建立",  // 噗文的內容
          language  = Some(Language.tr_ch)  // 修飾詞的語言（tr_ch 為中文）
        )
      } catch { case e: Exception =>  S.notice("Plurk 發佈失敗") }
      */

      
      S.notice(room.id.toString() + "號村已建立") 
    }
    
    bind("entry", xhtml,
      "room_name"         -> SHtml.text(room_name,         room_name = _, "size"->"45"),
      "room_comment"      -> SHtml.text(room_comment,      room_comment = _, "size"->"50"),
      "max_user"          -> SHtml.select(Seq(("8","8"),("11","11"),("16","16"),("22","22"),("25","25")),
                             Full("25"), max_user = _, "style"->"background-color:aliceblue;"),
      "day_minutes"       -> SHtml.text(day_minutes,       day_minutes = _,   "size"->"2"),
      "night_minutes"     -> SHtml.text(night_minutes,     night_minutes = _, "size"->"2"),
      "dummy_handle_name" -> SHtml.text(dummy_handle_name, dummy_handle_name = _, "size"->"50"),
      "dummy_last_words"  -> SHtml.textarea(dummy_last_words,  dummy_last_words = _, "rows"->"3","cols"->"70","wrap"->"soft"),
   
      // 這邊有點笨，算了，等我有空再來改
      "test_mode"         -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.TEST_MODE),     "id"->"test_mode"),
      "wish_role"         -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.WISH_ROLE),     "id"->"wish_role"),
      "no_dummy"          -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NO_DUMMY),      "id"->"no_dummy"),
      "dummy_reveal"      -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.DUMMY_REVEAL),  "id"->"dummy_reveal"),
      "vote_reveal"       -> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.VOTE_REVEAL),  "id"->"vote_reveal"),
      //"vote_reveal"       -> SHtml.hidden(() => option_list = option_list ::: List(RoomFlagEnum.VOTE_REVEAL)),
      "death_look"        -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.DEATH_LOOK),    "id"->"death_look"),
      "gemini_talk"       -> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.GEMINI_TALK),    "id"->"gemini_talk"),
      //"gemini_talk"       -> SHtml.hidden(() => option_list = option_list ::: List(RoomFlagEnum.GEMINI_TALK)),
      "auto_vote"         -> SHtml.checkbox(true,  if (_) option_list = option_list ::: List(RoomFlagEnum.AUTO_VOTE),     "id"->"auto_vote"),
      "weather"           -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.WEATHER),       "id"->"weather"),
      "weather1"          -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.WEATHER1),      "id"->"weather1"),
      "item_mode"         -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.ITEM_MODE),     "id"->"item_mode"),
      "item_cubic"       -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.ITEM_CUBIC),     "id"->"item_cubic"),
      "cubic_channel"    -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.CUBIC_CHANNEL),  "id"->"cubic_channel"),
      "cubic_init"       -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.CUBIC_INIT),     "id"->"cubic_init"),
      "cubic_immediate"   -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.CUBIC_IMMEDIATE),"id"->"cubic_immediate"),
      "mob_mode"          -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.MOB_MODE),      "id"->"mob_mode"),
      "mob_mode1"         -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.MOB_MODE1),     "id"->"mob_mode1"),
      
      "role_cleric"       -> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.ROLE_CLERIC),   "id"->"role_cleric"),
      "role_herbalist"    -> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.ROLE_HERBALIST),"id"->"role_herbalist"),
      "role_poisoner"     -> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.ROLE_POISONER), "id"->"role_poison"),
      "role_runner"       -> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.ROLE_RUNNER),   "id"->"role_runner"),
      "role_scholar"      -> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.ROLE_SCHOLAR),  "id"->"role_scholar"),
      "role_aughunter"    -> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.ROLE_AUGHUNTER),"id"->"role_aughunter"),
      "role_archmage"     -> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.ROLE_ARCHMAGE), "id"->"role_archmage"),

      "role_sorceror"     -> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.ROLE_SORCEROR), "id"->"role_sorceror"),
      "role_wolfcub"      -> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.ROLE_WOLFCUB),  "id"->"role_wolfcub"),

      "role_betrayer"     -> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.ROLE_BETRAYER), "id"->"role_betrayer"),
      "role_godfat"       -> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.ROLE_GODFAT),   "id"->"role_godfat"),
      
      "role_demon"        -> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.ROLE_DEMON),    "id"->"role_demon"),
      "role_fallen_angel" -> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.ROLE_FALLEN_ANGEL), "id"->"role_fallen_angel"),

      "role_pontiff"      -> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.ROLE_PONTIFF),  "id"->"role_pontiff"),
      "role_penguin"      -> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.ROLE_PENGUIN),  "id"->"role_penguin"),
      "role_inheriter"    -> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.ROLE_INHERITER),"id"->"role_inheriter"),
      "role_shifter"      -> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.ROLE_SHIFTER),  "id"->"role_shifter"),
      "role_hermit"       -> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.ROLE_HERMIT),   "id"->"role_hermit"),
      "role_cardmaster"   -> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.ROLE_CARDMASTER),"id"->"role_cardmaster"),

      "subrole_memoryloss4"  -> SHtml.checkbox(true, if (_) option_list = option_list ::: List(RoomFlagEnum.SUBROLE_MEMORYLOSS4), "id"->"subrole_memoryloss4"),
      "subrole_memoryloss4_2"-> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.SUBROLE_MEMORYLOSS4_2), "id"->"subrole_memoryloss4_2"),
      "subrole_memoryloss6"  -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.SUBROLE_MEMORYLOSS6), "id"->"subrole_memoryloss6"),
      "subrole_memoryloss8"  -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.SUBROLE_MEMORYLOSS8), "id"->"subrole_memoryloss8"),
      "subrole_fakeaugurer"  -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.SUBROLE_FAKEAUGURER), "id"->"subrole_fakeaugurer"),
      "subrole_suddendeath"  -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.SUBROLE_SUDDENDEATH), "id"->"subrole_suddendeath"),
      "subrole_avenger"      -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.SUBROLE_AVENGER), "id"->"subrole_avenger"),
      "subrole_wolfbeliever" -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.SUBROLE_WOLFBELIEVER), "id"->"subrole_wolfbeliever"),
      "subrole_alphawolf"    -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.SUBROLE_ALPHAWOLF),    "id"->"subrole_alphfwolf"),
      "subrole_wisewolf"     -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.SUBROLE_WISEWOLF),     "id"->"subrole_wisewolf"),
      "subrole_wolfstamp"    -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.SUBROLE_WOLFSTAMP),    "id"->"subrole_wolfstamp"),
      "subrole_subpontiff"   -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.SUBROLE_SUBPONTIFF), "id"->"subrole_subpontiff"),
      "subrole_hashihime"    -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.SUBROLE_HASHIHIME),  "id"->"subrole_hashihime"),
      "subrole_plus"         -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.SUBROLE_PLUS),         "id"->"subrole_plus"),

      // Role Adjustment
      "villager_detect"   -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.VILLAGER_DETECT), "id"->"villager_detect"),
      "necromancer_option1"-> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.NECROMANCER_OPTION1), "id"->"necromancer_option1"),
      "hunter_option1"    -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.HUNTER_OPTION1), "id"->"hunter_option1"),
      "hunter_option2"    -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.HUNTER_OPTION2), "id"->"hunter_option2"),
      "gemini_daytalk"    -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.GEMINI_DAYTALK), "id"->"gemini_daytalk"),
      "gemini_balance"    -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.GEMINI_BALANCE), "id"->"gemini_balance"),
      "gemini_legacy"     -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.GEMINI_LEGACY),  "id"->"gemini_legacy"),
      "cleric_option1"    -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.CLERIC_OPTION1), "id"->"cleric_option1"),
      "cleric_option2"    -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.CLERIC_OPTION2), "id"->"cleric_option2"),
      "herbalist_mix"     -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.HERBALIST_MIX), "id"->"herbalist_mix"),
      "herbalist_drop"    -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.HERBALIST_DROP), "id"->"herbalist_drop"),
      "scholar_option1"   -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.SCHOLAR_OPTION1), "id"->"scholar_option1"),
      "scholar_option2"   -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.SCHOLAR_OPTION2), "id"->"scholar_option2"),
      "scholar_option3"   -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.SCHOLAR_OPTION3), "id"->"scholar_option3"),
      "scholar_option4"   -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.SCHOLAR_OPTION4), "id"->"scholar_option4"),
      "runner_option1"    -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.RUNNER_OPTION1), "id"->"runner_option1"),
      "runner_option2"    -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.RUNNER_OPTION2), "id"->"runner_option2"),
      "runner_option3"    -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.RUNNER_OPTION3), "id"->"runner_option3"),
      "runner_option4"    -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.RUNNER_OPTION4), "id"->"runner_option4"),
      "archmage_option1"  -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.ARCHMAGE_OPTION1), "id"->"archmage_option1"),
      "werewolf_option1"  -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.WEREWOLF_OPTION1), "id"->"werewolf_option1"),
      "wolfcub_option1"   -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.WOLFCUB_OPTION1), "id"->"wolfcub_option1"),
      "madman_knowledge"  -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.MADMAN_KNOWLEDGE), "id"->"madman_knowledge"),
      "madman_suicide"    -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.MADMAN_SUICIDE),  "id"->"madman_suicide"),
      "madman_stun"       -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.MADMAN_STUN),     "id"->"madman_stun"),
      "madman_duel"       -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.MADMAN_DUEL),     "id"->"madman_duel"),
      "sorceror_believe"  -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.SORCEROR_BELIEVE), "id"->"sorceror_believe"),
      "sorceror_whisper1" -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.SORCEROR_WHISPER1), "id"->"sorceror_whisper1"),
      "sorceror_shout1"   -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.SORCEROR_SHOUT1), "id"->"sorceror_shout1"),
      "sorceror_sear"     -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.SORCEROR_SEAR),   "id"->"sorceror_sear"),
      "sorceror_summon"   -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.SORCEROR_SUMMON), "id"->"sorceror_summon"),
      "fox_option1"       -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.FOX_OPTION1),      "id"->"fox_option1"),
      "fox_option2"       -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.FOX_OPTION2),      "id"->"fox_option2"),
      "fox_option3"       -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.FOX_OPTION3),      "id"->"fox_option3"),
      "fox_option4"       -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.FOX_OPTION4),      "id"->"fox_option4"),
      "betrayer_option1"  -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.BETRAYER_OPTION1), "id"->"betrayer_option1"),
      "betrayer_option2"  -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.BETRAYER_OPTION2), "id"->"betrayer_option2"),
      "betrayer_option3"  -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.BETRAYER_OPTION3), "id"->"betrayer_option3"),
      "godfat_special1"   -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.GODFAT_SPECIAL1),  "id"->"godfat_special1"),
      "godfat_special2"   -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.GODFAT_SPECIAL2),  "id"->"godfat_special2"),
      "godfat_special3"   -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.GODFAT_SPECIAL3),  "id"->"godfat_special3"),
      "godfat_special4"   -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.GODFAT_SPECIAL4),  "id"->"godfat_special4"),
      "demon_option1"     -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.DEMON_OPTION1),    "id"->"demon_option1"),
      "demon_option2"     -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.DEMON_OPTION2),    "id"->"demon_option2"),
      "demon_option3"     -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.DEMON_OPTION3),    "id"->"demon_option3"),
      "penguin_option1"   -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.PENGUIN_OPTION1),  "id"->"penguin_option1"),
      "penguin_option2"   -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.PENGUIN_OPTION2),  "id"->"penguin_option2"),
      "penguin_option3"   -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.PENGUIN_OPTION3),  "id"->"penguin_option3"),
      "pontiff_option1"   -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.PONTIFF_OPTION1), "id"->"pontiff_option1"),
      "pontiff_option2"   -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.PONTIFF_OPTION2), "id"->"pontiff_option2"),
      "pontiff_option3"   -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.PONTIFF_OPTION3), "id"->"pontiff_option3"),
      "inheriter_reveal"  -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.INHERITER_REVEAL), "id"->"inheriter_reveal"),
      "inheriter_neutral" -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.INHERITER_NEUTRAL), "id"->"inheriter_neutral"),
      "shifter_reveal"    -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.SHIFTER_REVEAL), "id"->"shifter_reveal"),
      "shifter_links"     -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.SHIFTER_LINKS), "id"->"shifter_links"),
      "cardmaster_option1" -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.CARDMASTER_OPTION1), "id"->"cardmaster_option1"),

      "gm_penguin1"         -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.GM_PENGUIN1), "id"->"gm_penguin1"),
      "gm_hermit1"          -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.GM_HERMIT1), "id"->"gm_hermit1"),
      "gm_pontiff1"         -> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.GM_PONTIFF1), "id"->"gm_pontiff1"),

      "submit"            -> SHtml.submit(" 建  立 ",  create_room)      
    )
  }   

}

