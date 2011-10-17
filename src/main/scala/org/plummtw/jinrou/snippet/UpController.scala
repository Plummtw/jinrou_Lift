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

class UpController {
  
  //<form name="reloadsend" method="POST" action="game_play.php?room_no=4600&auto_reload=15&play_sound=off&dead_mode=&heaven_mode=&list_down=#game_top" target="bottom"></form>
  //<form name="send" method="POST" action="game_play.php?room_no=4600&auto_reload=15&play_sound=off&dead_mode=&heaven_mode=&list_down=#game_top" target="bottom" onSubmit="setfocus();  ">
  /*
  private def js_reload_game(reload_middle: Boolean) = {
    """function reloadgame(){
          document.forms['reloadsend'].submit();
    """ + (if (reload_middle) "parent.frames['middle'].document.forms['middle_reloadform'].submit();"
          else "") +
    "}"
  }
  */

 private def js_reload_game(reload_middle: Boolean) = {
    """
    // <![CDATA[
    function reloadgame(){
      //document.forms['reloadsend'].submit();
    $('#reloadsend').submit();
    """ + (if (reload_middle) "$('#reloadmiddle').submit();" //"parent.frames['middle'].${'#reloadsend').submit();"
          else "") +
    """}
    // ]]>
    """
}
  
  def say_form (xhtml : Group) : NodeSeq = {
    val room_no = S.param("room_no").getOrElse(S.getSessionAttribute("room_id").getOrElse("0"))
    val room_id : Long =
      try { room_no.toLong } 
      catch { case e: Exception => 0 }
    val room_list   = Room.findAll(By(Room.id, room_id))
    val room        = if (room_list.length == 0) null else room_list(0)

    if (room == null) {
      S.error(<b>找不到村莊</b>)
      S.redirectTo("up_error.html")
    }
    
    val room_day_list   = RoomDay.findAll(By(RoomDay.room_id, room_id), OrderBy(RoomDay.day_no, Descending))
    var room_day        = if (room_day_list.length == 0) null else room_day_list(0)
    if (room_day == null) {
      S.error(<b>找不到遊戲日</b>)
      S.redirectTo("up_error.html")
    }
    
    var user_entry : UserEntry = null
    val user_entry_no : String = S.getSessionAttribute("user_id").getOrElse("")
    if ((S.getSessionAttribute("room_id").getOrElse("") == room_no) && 
        (user_entry_no != "")){
      val user_entry_list   = UserEntry.findAll(By(UserEntry.id, user_entry_no.toLong), By(UserEntry.room_id, room_id))
      user_entry            = if (user_entry_list.length == 0) null else user_entry_list(0)
    }
    
    if (user_entry == null) {
      S.error(<b>找不到對應的玩家 {user_entry_no}</b>)
      S.redirectTo("up_error.html")
    }
    val user_entrys     = UserEntry.findAll(By(UserEntry.room_id, room_id))
    
    // 是否發言
    var say_data   = S.param("say").getOrElse("").trim()
    val say_gemini = S.param("say_gemini").getOrElse("")
    val say_lover  = S.param("say_lover").getOrElse("")
    val say_betrayer = S.param("say_betrayer").getOrElse("")

    val font_type  = S.param("font_type").getOrElse("")
    val day_no = 
      try { S.param("day_no").getOrElse("0").toLong }
      catch { case e: Exception => 0 } 
      
    //if (say_data.length > 550)
    //  say_data = S.param("say").getOrElse("").trim().substring(0, 550)
    say_data = 
      //if ((new Random()).nextInt(100) < 15) JinrouUtil.encodeHtml_ap(say_data)
      //else
      JinrouUtil.encodeHtml(say_data, Talk.message.maxLen)

    //if (say_data.length > 600)   // 太多控制碼了，程式不比對了，直接空白
    //  say_data = ""

    //println("say_data : [" + say_data + "]")
    //println("say_font : [" + font_type + "]")
    //println("check room_day.deadline.is == null : " + (room_day.deadline.is == null)  )
    //println("check user_entry.last_talk.is != say_data : " + (user_entry.last_talk.is != say_data))
    //println("check say_day_no == room_day.day_no.is : " + (day_no == room_day.day_no.is))
    //println("check room.status.is == RoomStatusEnum.ENDED.toString : " + (room.status.is == RoomStatusEnum.ENDED.toString))
    //println("check  !user_entry.live.is : " + (!user_entry.live.is))
    val system_messages        = SystemMessage.findAll(By(SystemMessage.roomday_id, room_day.id.is))
    val vote_betrayer_disguise = system_messages.filter(_.mtype.is == MTypeEnum.VOTE_BETRAYER_DISGUISE.toString)
    val vote_betrayer_fog = system_messages.filter(_.mtype.is == MTypeEnum.VOTE_BETRAYER_FOG.toString)
    val vote_sorceror_whisper = system_messages.filter(x => (x.mtype.is == MTypeEnum.VOTE_SORCEROR_WHISPER.toString) &&
                                                            (x.actioner_id.is == user_entry.id.is))
    val vote_ventriloquist = system_messages.filter(x => (x.mtype.is == MTypeEnum.ITEM_VENTRILOQUIST.toString) &&
                                                         (x.actioner_id.is == user_entry.id.is))
//*    val vote_betrayer_disguise = SystemMessage.findAll(By(SystemMessage.roomday_id, room_day.id.is),
//                                                       By(SystemMessage.actioner_id, user_entry.id.is),
//*                                                       By(SystemMessage.mtype, MTypeEnum.VOTE_BETRAYER_DISGUISE.toString))
//*    val vote_betrayer_fog      = SystemMessage.findAll(By(SystemMessage.roomday_id, room_day.id.is),
//*                                                       By(SystemMessage.mtype, MTypeEnum.VOTE_BETRAYER_FOG.toString))
//*    val vote_sorceror_whisper = SystemMessage.findAll(By(SystemMessage.roomday_id, room_day.id.is),
//*                                                      By(SystemMessage.actioner_id, user_entry.id.is),
//*                                                      By(SystemMessage.mtype, MTypeEnum.VOTE_SORCEROR_WHISPER.toString))


    if (((say_data != "") && List("20","16","12","8","S","V").contains(font_type)) ||
        (font_type == "L")) {
      if (font_type == "L") {
        // 遺言
        if (user_entry.live.is) {
          user_entry.last_words(say_data)
          user_entry.validate match {
            case Nil => user_entry.save()
            case xs  => Log.warn(xs); S.error(xs)
          }
        }
      } else if (((room_day.deadline.is == null) &&
//                  (user_entry.last_talk.is != say_data) &&
//      這個先不擋
//                    (!is_redundant) &&
                  (day_no == room_day.day_no.is)) ||
                 (room.status.is == RoomStatusEnum.ENDED.toString) ||
                 (!user_entry.live.is)) {
        // 一般發言
        // 超過時間的話，或是發言重複 就不能發言了
        val mtype : String =
          if (room.status.is == RoomStatusEnum.ENDED.toString)
            MTypeEnum.TALK_END.toString
          else if (!user_entry.live.is)
            MTypeEnum.TALK_HEAVEN.toString
          else if (room_day.day_no.is % 2 == 0) {
            if ((font_type == "S") && (vote_sorceror_whisper.length != 0))
              MTypeEnum.TALK_SECRET.toString
            else if ((font_type == "V") && (vote_ventriloquist.length != 0))
              MTypeEnum.TALK_VENTRILOQUIST.toString
            else if ((say_gemini == "on") && (user_entry.current_role == RoleEnum.GEMINI) &&
                     (!user_entry.test_memoryloss(room, room_day, user_entrys)) &&
                     (!user_entry.test_fake(room_day)) &&
                     (room.has_flag(RoomFlagEnum.GEMINI_DAYTALK)))
              MTypeEnum.TALK_GEMINI_DAY.toString
            else if ((say_betrayer == "on") && (vote_betrayer_disguise.length != 0)
                     && (vote_betrayer_disguise.map(_.actioner_id.is).contains(user_entry.id.is)))
              MTypeEnum.TALK_DISGUISED.toString
            else if ((vote_betrayer_disguise.length != 0)
                     && (vote_betrayer_disguise.map(_.actionee_id.is).contains(user_entry.id.is))) {
              var is_sealed = false
              vote_betrayer_disguise.foreach { vote_betrayer =>
                val betrayer_list = user_entrys.filter(x=>(x.id.is == vote_betrayer.actioner_id.is) && (x.live.is))
                if ((betrayer_list.length != 0) || (vote_betrayer.actioner_id.is == 0))
                   is_sealed = true
              }
              if (is_sealed)
                MTypeEnum.TALK_SEALED.toString
              else if (vote_betrayer_fog.length != 0)
                MTypeEnum.TALK_DAY_FOG.toString
              else
                MTypeEnum.TALK_DAY.toString
            }
            else if (vote_betrayer_fog.length != 0)
              MTypeEnum.TALK_DAY_FOG.toString
            else
              MTypeEnum.TALK_DAY.toString
          }
          else if ((say_lover == "on") &&
                   (user_entry.has_flag(UserEntryFlagEnum.LOVER)) &&
                   (room.has_flag(RoomFlagEnum.CUBIC_CHANNEL)))
              MTypeEnum.TALK_LOVER.toString
          else if ((user_entry.test_memoryloss(room, room_day, user_entrys)) ||
                   (user_entry.test_fake(room_day)))
            MTypeEnum.TALK_NIGHT.toString
          else if (user_entry.role.is.substring(0,1) == RoleEnum.WEREWOLF.toString)
            MTypeEnum.TALK_WEREWOLF.toString
          else if (user_entry.role.is.substring(0,1) == RoleEnum.GEMINI.toString)
            MTypeEnum.TALK_GEMINI.toString
          else if (user_entry.role.is.substring(0,1) == RoleEnum.FOX.toString)
            MTypeEnum.TALK_FOX.toString
          else if (user_entry.role.is.substring(0,1) == RoleEnum.PONTIFF.toString)
            MTypeEnum.TALK_PONTIFF.toString
          else
            MTypeEnum.TALK_NIGHT.toString
          
      //GameProcessLock.get_lock(room_id).synchronized {
      //算了，發言不擋了，擋行動就好了

        //println("Creating Talk")
        val talk = Talk.create.roomday_id(room_day.id.is).actioner_id(user_entry.id.is)
                   .font_type(font_type).message(say_data).mtype(mtype)
        if (user_entry.has_flag(UserEntryFlagEnum.BECAME_MOB)) {
          val random_int = new Random().nextInt(10)
          if (random_int == 0) {
            talk.font_type("20")
          }
        }

        if (talk.mtype.is == MTypeEnum.TALK_DISGUISED.toString) {
           val actionee = vote_betrayer_disguise.filter(_.actioner_id.is == user_entry.id.is)(0)
           talk.actionee_id(actionee.actionee_id.is)
        } else
           talk.actionee_id(0)

        UserSayLock.get_lock(user_entry.id.is).synchronized {
          var user_entry_new = UserEntry.findAll(By(UserEntry.id, user_entry.id.is))(0)

          val last_say = new java.util.GregorianCalendar()
          last_say.setTime(user_entry_new.updated.is)
          last_say.add(java.util.Calendar.SECOND, 5)
          val current_time = new java.util.Date()
          val is_redundant = (user_entry_new.last_talk.is == say_data) &&
                             (current_time.before(last_say.getTime()))
          /* if (is_redundant) {
            Log.warn("redundant : UserEntry : " + user_entry.id.is.toString + " say_data : " + say_data)
          } else if (user_entry_new.last_talk.is == say_data) {
            Log.warn("same last_talk : UserEntry : " + user_entry.id.is.toString + " say_data : " + say_data)
            Log.warn("current_time : " + current_time.toString + " last_say : " + last_say.getTime().toString)
          } */
          if (!is_redundant) {
            talk.validate match {
              case Nil => talk.save(); //println("Talk Saved")
              case xs  => println(xs); Log.warn(xs); S.error(xs)
            }
            
            user_entry.last_talk(say_data).updated(new java.util.Date())
            user_entry.save()

            // 遊戲大廳時更新 room 的時間，用來判斷廢村用
            if (room_day.day_no.is == 0) {
              room.updated(new java.util.Date())
              room.save
            }

          }
        }
        //}
        //message_refresh = true
      }
    }

    // 投票次數
    val vote_time_data     = S.param("vote_time").getOrElse("").trim()
    val vote_time          =
      try { vote_time_data.toInt }
      catch { case e: Exception => 0 }

    // 是否行動
    val command_data       = S.param("command").getOrElse("").trim()
    //println("Command : " + command_data)

    val command_target_no  = 
      try { S.param("target").getOrElse("0").toLong }
      catch { case e: Exception => 0}

    var vote_list = Vote.findAll(By(Vote.roomday_id, room_day.id.is), By(Vote.vote_time, room_day.vote_time.is))

    if (command_data.startsWith("item_")) {
      val user_item = ItemEnum.get_item(user_entry.item_flags.is)
      if (user_item.command_name != command_data) {
        Log.warn("Item Error " + command_data + " UserEntry: " + user_entry.id.is)
      } else {
        var command_target : UserEntry = null
        if (user_item.targetable) {
          val targetable_users = user_item.targetable_users(room, room_day, user_entry, user_entrys)
          val command_target_list = user_entrys.filter(_.id.is == command_target_no)
          command_target = if (command_target_list.length != 0) command_target_list(0) else null

          if ((command_target == null) || (!targetable_users.contains(command_target))) {
            Log.warn("Item Target Error " + command_data + " UserEntry: " + user_entry.id.is + " " + command_target)
            S.error(<b>目標錯誤 : {command_target}</b>)
            S.redirectTo("up_error.html")
          }
        }

        // 寫入道具使用
        val item_vote = ItemVote.create.roomday_id(room_day.id.is).actioner_id(user_entry.id.is)
                        .mtype(user_item.action_enum.toString.toUpperCase)
        if (user_item.targetable)
          item_vote.actionee_id(command_target.id.is)
        else
          item_vote.actionee_id(Empty)

        val item_option_key =
          if (user_item.isInstanceOf[ItemOption])
            S.param("item_option").getOrElse("")
          else ""

        val item_option_val =
          if (user_item.isInstanceOf[ItemOption])
            user_item.asInstanceOf[ItemOption].option_map.get(item_option_key).getOrElse("")
          else ""

        item_vote.vote_flags(item_option_key)

        item_vote.validate match {
          case Nil => item_vote.save()
          case xs  => Log.warn(xs)
        }

        // 寫入行動字串
        val talk = Talk.create.roomday_id(room_day.id.is).actioner_id(user_entry.id.is)
                  .mtype(user_item.action_enum.toString)

        if (user_item.targetable)
          talk.actionee_id(command_target.id.is)
        else
          talk.actionee_id(Empty)

        talk.message(item_option_val)

        talk.validate match {
          case Nil => talk.save()
          case xs  => Log.warn(xs)
        }
      }

    } else if (command_data != "") {
      val actions = user_entry.get_action_list(room, room_day, user_entrys, vote_list)
      val actions_filtered = actions.filter(_.command_name == command_data)
      val action  = 
        if   (actions_filtered.length == 0) null
        else  actions_filtered(0)
        
      if (action == null) {
        Log.warn("Action Error " + command_data + " UserEntry: " + user_entry.id.is)
      } else {
        if (action == ActionStartGame) { // 開始遊戲，這個要特殊處理

          GameProcessLock.get_lock(room_id).synchronized {
            // 重新取一次 Roomday
            room_day = RoomDay.findAll(By(RoomDay.room_id, room_id), OrderBy(RoomDay.day_no, Descending))(0)
            if (room_day.day_no.is == 0) {
              user_entry.user_flags(UserEntryFlagEnum.VOTED.toString)
              user_entry.save()

              val users_voted = UserEntry.findAll(By(UserEntry.room_id, room.id.is),
                                                  By(UserEntry.user_flags, UserEntryFlagEnum.VOTED.toString))
              if ((users_voted.length == user_entrys.length) && (user_entrys.length >= 8)) {
                  // 全員投 開始遊戲完成 正式開始遊戲
                  GameProcesser.process_start_game(room, user_entrys)

                  // TODO: 換日時更新 Cache
              }

            }
          }
          
        } else if (action == ActionKick) { // 踢人，這個要特殊處理
          val command_target_list = user_entrys.filter(_.id.is == command_target_no)
          val command_target = if (command_target_list.length != 0) command_target_list(0) else null
          if (command_target != null) {
            val votes = Vote.findAll(By(Vote.actioner_id, user_entry.id.is), By(Vote.actionee_id, command_target.id.is),
                                     By(Vote.mtype, MTypeEnum.VOTE_KICK.toString))
            if ((room_day.day_no == 0) && (votes.length == 0)) { // 沒踢過才可以踢
              // 寫入投票
              val vote = Vote.create.roomday_id(room_day.id.is).actioner_id(user_entry.id.is)
                         .vote_time(vote_time).mtype(action.action_enum.toString)
                         .actionee_id(command_target.id.is)

              vote.save()

              // 寫入行動字串
              val talk = Talk.create.roomday_id(room_day.id.is).actioner_id(user_entry.id.is)
                         .mtype(action.action_enum.toString).actionee_id(command_target.id.is)
                         .message(user_entry.handle_name.is + " 對 " + command_target.handle_name.is + " 投票踢出")
              talk.save()

              val voted_list = Vote.findAll(By(Vote.actionee_id, command_target.id.is),
                                            By(Vote.mtype, MTypeEnum.VOTE_KICK.toString))
              
              if (voted_list.length >= 5) {
                // 使用者被踢出去
                command_target.room_id(0)
                command_target.save()

                val talk2 = Talk.create.roomday_id(room_day.id.is).actioner_id(command_target.id.is)
                                .mtype(MTypeEnum.MESSAGE_KICKED.toString).message(command_target.handle_name.is)
                talk2.save()

                // 投票重新開始
                Vote.bulkDelete_!!(By(Vote.roomday_id, room_day.id.is))
                //ItemVote.bulkDelete_!!(By(ItemVote.roomday_id, room_day.id.is))
                SpecialVote.bulkDelete_!!(By(SpecialVote.roomday_id, room_day.id.is))

                // Update 使用者狀態
                DB.use(DefaultConnectionIdentifier) { conn =>
                  DB.prepareStatement("update UserEntry set user_flags = '' where room_id = ? and uname != 'dummy_boy'", conn) { stmt =>
                    stmt.setLong(1, room.id.is)
                    stmt.executeUpdate()
                  }
                }

                val talk3 = Talk.create.roomday_id(room_day.id.is)
                                .mtype(MTypeEnum.MESSAGE_REVOTE0.toString)
                talk3.save()
              }
            }
          }
        } else { // 其他正常行動
          
          var command_target : UserEntry = null
          if (action.targetable) {
            val targetable_users = action.targetable_users(room, room_day, user_entry, user_entrys)
            val command_target_list = user_entrys.filter(_.id.is == command_target_no)
            command_target = if (command_target_list.length != 0) command_target_list(0) else null

            if ((command_target == null) || (!targetable_users.contains(command_target))) {
              Log.warn("Target Error " + command_data + " UserEntry: " + user_entry.id.is + " " + command_target)
              S.error(<b>目標錯誤 : {command_target}</b>)
              S.redirectTo("up_error.html")
            }
          } 
          
          GameProcessLock.get_lock(room_id).synchronized {
            // 重新取一次 Roomday
            room_day = RoomDay.findAll(By(RoomDay.room_id, room_id), OrderBy(RoomDay.day_no, Descending))(0)
            //println("room_day.day_no == day_no : " + (room_day.day_no.is == day_no).toString)
            //println("room_day.vote_time == vote_time : " + (room_day.vote_time.is == vote_time).toString)
            //println("room.status.is != RoomStatusEnum.ENDED.toString : " + (room.status.is != RoomStatusEnum.ENDED.toString).toString)
            //println("user_entry.live.is : " + (user_entry.live.is).toString)
            if ((room_day.day_no.is == day_no) &&
                (room_day.vote_time.is == vote_time) &&
                (room.status.is != RoomStatusEnum.ENDED.toString) &&
                (user_entry.live.is)) {

              // 寫入投票
              val vote = Vote.create.roomday_id(room_day.id.is).actioner_id(user_entry.id.is)
                         .vote_time(vote_time).mtype(action.action_enum.toString.toUpperCase)
              if (room.has_flag(RoomFlagEnum.ITEM_MODE) && (action == ActionVote)) {
                val auction_number =
                  try { Math.max(0, Math.min(S.param("auction").getOrElse("0").toInt, user_entry.cash.is)) }
                  catch { case e: Exception => 0}
                vote.auc_number(auction_number)
              }

              if (action.targetable) {
                // 暴民投票為亂數
                if (user_entry.has_flag(UserEntryFlagEnum.BECAME_MOB) && (action == ActionVote)) {
                  val live_users   = user_entrys.filter(x=>(x != user_entry) && (x.live.is))
                  val random_user = live_users((new Random()).nextInt(live_users.length))
                  vote.actionee_id(random_user.id.is)
                } else
                  vote.actionee_id(command_target.id.is)
              } else
                vote.actionee_id(Empty)

              vote.validate match {
                case Nil => vote.save()
                case xs  => Log.warn(xs)
              }

              // 寫入行動字串
              val talk = Talk.create.roomday_id(room_day.id.is).actioner_id(user_entry.id.is)
                         .mtype(action.action_enum.toString)

              if (action.targetable) {
                talk.actionee_id(command_target.id.is)
              } else
                talk.actionee_id(Empty)

              talk.validate match {
                case Nil => talk.save()
                case xs  => Log.warn(xs)
              }

                            
              vote_list = Vote.findAll(By(Vote.roomday_id, room_day.id.is), By(Vote.vote_time, room_day.vote_time.is))
              // 檢查是否所有人都行動過了
              if (user_entrys.forall{user => user.get_action_list(room, room_day, user_entrys, vote_list).length == 0}) {
                  GameProcesser.process_phase(room, room_day, user_entrys, vote_list)
                 
                 // 重新取一次 room_day 和 vote_list
                 room_day = RoomDay.findAll(By(RoomDay.room_id, room_id), OrderBy(RoomDay.day_no, Descending))(0)
                 vote_list = Vote.findAll(By(Vote.roomday_id, room_day.id.is), By(Vote.vote_time, room_day.vote_time.is))
              }
              
            }
          }


        }
      }
    }
    
    // 設定顏色
    val (background_color, text_color) = JinrouUtil.color_helper(room, room_day)
    val css_style = JinrouUtil.css_style(background_color, text_color)

    val dead_mode           = (room.status.is != RoomStatusEnum.ENDED.toString) &&
                              (user_entry != null) && (!user_entry.live.is)

    val js_reload_game_str  = js_reload_game(dead_mode)
    val item_bar            = (if (room.has_flag(RoomFlagEnum.ITEM_MODE) && (room_day.day_no.is % 2 == 1))
                                 user_entry.get_item_tag(room, room_day, user_entrys, ItemVote.findAll(By(ItemVote.roomday_id, room_day.id.is)))
                               else
                                 <span></span>)
    val action_bar          = user_entry.get_action_tag(room, room_day, user_entrys, vote_list)
    val say_option          = (if (vote_sorceror_whisper.length != 0) Seq(<option value="S">隱密發言</option>)
                              else NodeSeq.Empty) ++
                              (if (vote_ventriloquist.length != 0) Seq(<option value="V">腹語發言</option>)
                              else NodeSeq.Empty)
    val say_checkbox        = if ((room.status.is != RoomStatusEnum.ENDED.toString) &&
                                  (room_day.day_no.is != 0) && (room_day.day_no.is %2 == 0) &&
                                  (user_entry.live.is)) {
                                if ((user_entry.current_role == RoleEnum.GEMINI) && (!user_entry.test_memoryloss(room, room_day, user_entrys)) &&
                                    (!user_entry.test_fake(room_day)) &&
                                    (room.has_flag(RoomFlagEnum.GEMINI_DAYTALK)))
                                  Seq(<input type="checkbox" id="say_gemini" name="say_gemini"/>, <span>共生頻道</span>, <br/>)
                                else if ((vote_betrayer_disguise.length != 0) &&
                                         (vote_betrayer_disguise.map(_.actioner_id.is).contains(user_entry.id.is)))
                                  Seq(<input type="checkbox" id="say_betrayer" name="say_betrayer"/>, <span>偽裝頻道</span>, <br/>)
                                else
                                  NodeSeq.Empty
                              } else if ((room.status.is != RoomStatusEnum.ENDED.toString) &&
                                  (room_day.day_no.is %2 == 1) &&
                                  (user_entry.has_flag(UserEntryFlagEnum.LOVER)) &&
                                  (room.has_flag(RoomFlagEnum.CUBIC_CHANNEL)) &&
                                  (user_entry.live.is))
                                Seq(<input type="checkbox" id="say_lover" name="say_lover"/>, <span>戀人頻道</span>, <br/>)
                              else NodeSeq.Empty
    val item_option         = if ((room.status.is != RoomStatusEnum.ENDED.toString) &&
                                  (room_day.day_no.is %2 == 1) &&
                                  (user_entry.live.is)) {
                                val user_item = ItemEnum.get_item(user_entry.item_flags.is)
                                if (user_item.isInstanceOf[ItemOption]) {
                                  val option_map = user_item.asInstanceOf[ItemOption].option_map
                                  var option_map_option : NodeSeq = NodeSeq.Empty
                                  option_map.keys.foreach { option_key =>
                                    if (option_map_option == NodeSeq.Empty)
                                      option_map_option ++= <option value={option_key} selected="selected">{option_map.get(option_key).getOrElse("")}</option>
                                    else
                                      option_map_option ++= <option value={option_key}>{option_map.get(option_key).getOrElse("")}</option>
                                  }
                                  Seq(<select style="width:100px;" name="item_option">{option_map_option}</select>)
                                } else NodeSeq.Empty
                              } else NodeSeq.Empty

    val reload_page         = if (room.status.is == RoomStatusEnum.ENDED.toString) "game_end"
                              else if (dead_mode) "heaven_view"
                              else "game_view"
    
    bind("entry", xhtml,
      "css_style"            -> <style type="text/css">{Unparsed(css_style)}</style>,
      "js_reload_game"       -> <script type="text/javascript">{Unparsed(js_reload_game_str)}</script>,
      "room_no_hidden_field" -> <input type="hidden" name="room_no" value={room_no} />,
      "day_no_hidden_field"  -> <input type="hidden" name="day_no"  value={room_day.day_no.is.toString} />,
      "vote_time_hidden_field" -> <input type="hidden" name="vote_time" value={room_day.vote_time.is.toString} />,
      "reload_form"          -> <form id="reloadsend" name="reloadsend" method="POST" action={reload_page + ".html"} target="bottom">
                                  <input type="hidden" name="room_no" value={room_no} />
                                  <input type="hidden" name="day_no"  value={room_day.day_no.is.toString} />
                                </form>,
      "reload_middle_form"   -> <form id="reloadmiddle" name="reloadmiddle" method="POST" action="game_view.html" target="middle">
                                  <input type="hidden" name="room_no" value={room_no} />
                                  <input type="hidden" name="day_no"  value={room_day.day_no.is.toString} />
                                </form>,
      "say_option"           -> say_option,
      "say_checkbox"         -> say_checkbox,
      "item_option"          -> item_option,
      "item_bar"             -> item_bar,
      "action_bar"           -> action_bar
    )
  }
  
  // 產生 Javascript 字串 set_radio
  private def js_set_radio(only_one : Boolean) : String = {
    """function setRadio() {
          if (self.document.action) {
            """ + (if (only_one) "self.document.action.target.checked = true;"
                   else          "self.document.action.target[0].checked = true;") +
          """}
    }"""
  }
  
  def action_form (xhtml : Group) : NodeSeq = {
    val room_no = S.param("room_no").getOrElse("0")
    val room_id : Long =
      try { room_no.toLong } 
      catch { case e: Exception => 0 }
    val room_list   = Room.findAll(By(Room.id, room_id))
    val room        = if (room_list.length == 0) null else room_list(0)

    if (room == null) {
      S.error(<b>找不到村莊</b>)
      S.redirectTo("up_error.html")
    }
    
    val room_day_list   = RoomDay.findAll(By(RoomDay.room_id, room_id), OrderBy(RoomDay.day_no, Descending))
    val room_day        = if (room_day_list.length == 0) null else room_day_list(0)
    if (room_day == null) {
      S.error(<b>找不到遊戲日</b>)
      S.redirectTo("up_error.html")
    }
    
    // 設定顏色
    val (background_color, text_color) = JinrouUtil.color_helper(room, room_day)
    val css_style = JinrouUtil.css_style(background_color, text_color)
    
    var user_entry : UserEntry = null
    val user_entry_no : String = S.getSessionAttribute("user_id").getOrElse("")
    if ((S.getSessionAttribute("room_id").getOrElse("") == room_no) && 
        (user_entry_no != "")){
      val user_entry_list   = UserEntry.findAll(By(UserEntry.id, user_entry_no.toLong), By(UserEntry.room_id, room_id))
      user_entry            = if (user_entry_list.length == 0) null else user_entry_list(0)
    }
    
    if (user_entry == null) {
      S.error(<b>找不到對應的玩家 {user_entry_no}</b>)
      S.redirectTo("up_error.html")
    }
    
    val user_entrys     = UserEntry.findAll(By(UserEntry.room_id, room_id))
    val user_role       = if (room_day.day_no.is == 0) "0"
                          else user_entry.role.is.substring(0,1)
    val user_role_enum  = RoleEnum.valueOf(user_role).getOrElse(RoleEnum.NONE)
    val user_role_data  = RoleEnum.get_role(user_role_enum)

    var targetable_users : List[UserEntry] = null
    var tag_string       : String          = ""
    
    // 是否行動
    val command_data       = JinrouUtil.encodeHtml(S.param("command").getOrElse("").trim(), 100)

    if (command_data.startsWith("item_")) {
      val user_item = ItemEnum.get_item(user_entry.item_flags.is)
      if (user_item.command_name != command_data) {
        Log.warn("Item Error " + command_data + " UserEntry: " + user_entry.id.is)
      } else if (!user_item.targetable) {
        Log.warn("Item No Target " + command_data + " UserEntry: " + user_entry.id.is)
        S.error(<b>無可指定對象 {command_data}</b>)
        S.redirectTo("up_normal.html?room_no=" + room_no)
      } else {
        targetable_users = user_item.targetable_users(room, room_day, user_entry, user_entrys)
        tag_string = user_item.tag_string
      }
    } else {
      if (command_data == "") {
        Log.warn("No Command " + command_data + " UserEntry: " + user_entry.id.is)
        S.error(<b>找不到對應行動</b>)
        S.redirectTo("up_normal.html?room_no=" + room_no)
      }

      val vote_list = Vote.findAll(By(Vote.roomday_id, room_day.id.is), By(Vote.vote_time, room_day.vote_time.is))
      val actions = user_role_data.check_action_list(room, room_day, user_entry, user_entrys, vote_list)
      val actions_filtered = actions.filter(_.command_name == command_data)
      val action  =
        if   (actions_filtered.length == 0) null
        else  actions_filtered(0)

      if (action == null) {
        Log.warn("Cannot Command " + command_data + " UserEntry: " + user_entry.id.is)
        S.error(<b>你無法進行這項行動 {command_data}</b>)
        S.redirectTo("up_normal.html?room_no=" + room_no)
       }

      targetable_users = action.targetable_users(room, room_day, user_entry, user_entrys)
      tag_string = action.tag_string
    }

    if (targetable_users.length == 0) {
      Log.warn("No Target " + command_data + " UserEntry: " + user_entry.id.is)
      S.error(<b>無可指定對象 {command_data}</b>)
      S.redirectTo("up_normal.html?room_no=" + room_no)
    }

    val js_set_radio_text = js_set_radio(targetable_users.length == 1)

    def action_table = UserEntryHelper.user_select_table(user_entrys, targetable_users)
    
    bind("entry", xhtml,
      "css_style" -> <style type="text/css">{Unparsed(css_style)}</style>,
      "js_set_radio"   -> <script type="text/javascript">{Unparsed(js_set_radio_text)}</script>,
      "room_no_hidden_field"   -> <input type="hidden" name="room_no"   value={room_no} />,
      "day_no_hidden_field"    -> <input type="hidden" name="day_no"    value={room_day.day_no.is.toString} />,
      "command_hidden_field"   -> <input type="hidden" name="command"   value={command_data} />,
      "vote_time_hidden_field" -> <input type="hidden" name="vote_time" value={room_day.vote_time.is.toString} />,
      "action_table"           -> action_table,
      "auction"                -> (if (room.has_flag(RoomFlagEnum.ITEM_MODE) && (room_day.day_no.is % 2 == 0) && (room_day.day_no.is != 0))
                                     <span>競標金額：<input type="text" name="auction" size="3" maxlength="3" value="0"/></span>
                                   else
                                     <span></span>),
      "submit"                 -> <input type="submit" name="submit"  value={"　" + tag_string + "　"} />

      //"action_bar"           -> action_bar
    )
  }
}