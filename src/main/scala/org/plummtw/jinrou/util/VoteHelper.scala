package org.plummtw.jinrou.util

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
import org.plummtw.jinrou.data._

object VoteHelper {
  /*
  public static getNotVoted(room) {
    def result = []
    def room_day = room.room_days.last()
    if (room_day.day_no == 0) {
      return []
    } else if (room_day.day_no == 1) {
      // 第一天晚上
      room.user_entrys.each {
        if ((it.live) && (it.uname != 'dummy_boy') && (['A','I','S','W'].contains(it.role[0])))
          result.add(it)
      }
      def vote = room_day.votes.findAll{it.vote_time == room_day.vote_time}.collect{it.voter}
      result.removeAll(vote)
      if (vote.find{it.role[0]=='W'} != null) {
        result.removeAll(result.findAll{it.role[0]=='W'})
      }
    
    } else if (room_day.day_no %2 == 0) {
      // 白天
      room.user_entrys.each {
        if (it.live)
          result.add(it)
      }
      def vote = room_day.votes.findAll{it.vote_time == room_day.vote_time}.collect{it.voter}
      result.removeAll(vote)
    } else {
      // 晚上
      room.user_entrys.each {
        if (it.live) {
          if (['A','C','H','I','R','S','W'].contains(it.role[0]))
            result.add(it)
          else if ((room_day.day_no==7) && (room.room_flags =~ /VD/) && (it.role[0]=='V'))
            result.add(it)
        }
      }
      
      def vote = room_day.votes.findAll{it.vote_time == room_day.vote_time}.collect{it.voter}
      result.removeAll(vote)
      if (vote.find{it.role[0]=='W'} != null) {
        result.removeAll(result.findAll{it.role[0]=='W'})
      }
    }
    return result
  }
  
  public static getVoteString(room, user_entry) {
    def result = []
    def room_day = room.room_days.last()
    if (room_day.day_no == 0) {
      return ''
    } else if (room_day.day_no == 1) {
      // 第一天晚上
      room.user_entrys.each {
        if (it.live && (it.uname != 'dummy_boy') && (['A','I','S','W'].contains(it.role[0])))
          result.add(it)
      }
      if (!result.contains(user_entry))
        return '你無法行動'
      
      def vote = room_day.votes.findAll{it.vote_time == room_day.vote_time}.collect{it.voter}
      result.removeAll(vote)
      if (vote.find{it.role[0]=='W'} != null) {
        result.removeAll(result.findAll{it.role[0]=='W'})
      }
    
    } else if (room_day.day_no %2 == 0) {
      // 白天
      room.user_entrys.each {
        if (it.live)
          result.add(it)
      }
      if (!result.contains(user_entry))
        return '你無法投票'
      
      def vote = room_day.votes.findAll{it.vote_time == room_day.vote_time}.collect{it.voter}
      result.removeAll(vote)
    } else {
      // 晚上
      room.user_entrys.each {
        if (it.live) {
          if (['A','C','H','I','R','S','W'].contains(it.role[0]))
            result.add(it)
          else if ((room_day.day_no==7) && (room.room_flags =~ /VD/) && (it.role[0]=='V'))
            result.add(it)
        }
      }
      if (!result.contains(user_entry))
        return '你無法行動'
      
      def vote = room_day.votes.findAll{it.vote_time == room_day.vote_time}.collect{it.voter}
      result.removeAll(vote)
      if (vote.find{it.role[0]=='W'} != null) {
        result.removeAll(result.findAll{it.role[0]=='W'})
      }
    }
    
    if (result.contains(user_entry))
      return '你還沒投過票'
    return '你已經投過票了'
  }
  */  
  
  def check_vote_hang(room_day: RoomDay, user_entrys: List[UserEntry], vote_list: List[Vote]) : UserEntry = {
    //val votes = Vote.findAll(By(Vote.roomday_id, room_day.id.is), By(Vote.vote_time, room_day.vote_time.is),
    //                         By(Vote.mtype, MTypeEnum.VOTE_HANG.toString))

    // 處理惡魔支配術
    val dominates = SystemMessage.findAll(By(SystemMessage.roomday_id, room_day.id.is),
                                          By(SystemMessage.mtype, MTypeEnum.VOTE_DEMON_DOMINATE.toString))
    // 處理教主指令投票
    val pontiff_commands = SystemMessage.findAll(By(SystemMessage.roomday_id, room_day.id.is),
                                                 By(SystemMessage.mtype, MTypeEnum.VOTE_PONTIFF_COMMAND.toString))
    var pontiff_target    : Long            = 0
    var pontiff_affecters : List[Long]      = List()

    if (pontiff_commands.length != 0) {
      val pontiff_actioner = user_entrys.filter(_.id.is == pontiff_commands(0).actioner_id.is)(0)
      val pontiff_actionee = user_entrys.filter(_.id.is == pontiff_commands(0).actionee_id.is)(0)

      pontiff_target = pontiff_actionee.id.is
      if (pontiff_actioner.live.is && pontiff_actionee.live.is && (room_day.weather.is != WeatherEnum.SNOWY.toString))
        pontiff_affecters = user_entrys.filter(x=>(x.subrole.is == SubroleEnum.SUBPONTIFF.toString) ||
                                                  (x.current_role == RoleEnum.PONTIFF)  ||
                                                  (x.user_flags.is.indexOf(UserEntryFlagEnum.RELIGION.toString) != -1)).map(_.id.is)
    }


    var votes = vote_list.filter(_.mtype.is == MTypeEnum.VOTE_HANG.toString)
    var archmage : UserEntry = null
    var vote_archmage : Vote = null

    votes.foreach { vote => 
      vote.vote_number(0) 
      vote.vote_flags("")

      // 惡魔支配術
      val vote_dominate = dominates.filter(_.actionee_id.is == vote.actioner_id.is)
      if (vote_dominate.length != 0) {
        val dominate_actioner_list = user_entrys.filter(_.id.is == vote_dominate(0).actioner_id.is).filter(_.live.is)
        if (dominate_actioner_list.length != 0) {
          vote.actionee_id(vote_dominate(0).actioner_id.is).vote_flags(VoteFlagEnum.DOMINATE.toString)
          vote
        }
      }

      // 教主指定投票
      if (pontiff_affecters.contains(vote.actioner_id.is)) {
        vote.actionee_id(pontiff_target).vote_flags(VoteFlagEnum.COMMAND.toString)
        vote
      }

      val actioner      = user_entrys.filter(_.id.is    == vote.actioner_id.is)(0)
      if (actioner.current_role == RoleEnum.ARCHMAGE) {
        archmage = actioner   // 找出大魔導
        vote_archmage = vote  // 找出大魔導的投票
      }
    }

    // 投給大魔導的轉給水元素
    if ((archmage != null) && (archmage.user_flags.is.indexOf(UserEntryFlagEnum.WATER_ELEM_USED.toString) == -1)) {
      val vote_water_elem = Vote.create.roomday_id(room_day.id.is).actioner_id(0)
                            .actionee_id(vote_archmage.actionee_id.is).vote_time(room_day.vote_time.is)
                            .mtype(MTypeEnum.VOTE_HANG.toString)
      vote_water_elem.save()
      votes = votes ::: List(vote_water_elem)

      votes.foreach { vote =>
        if ((vote.actioner_id.is != archmage.id.is) &&
            (vote.actionee_id.is == archmage.id.is)) {
          vote.actionee_id(0)
          vote
        }
      }
    }
    
    // 處理狂巫鼓舞術
    val shouted = SystemMessage.findAll(By(SystemMessage.roomday_id, room_day.id.is), 
                                        By(SystemMessage.mtype, MTypeEnum.VOTE_SORCEROR_SHOUT.toString))
    val werewolf_side = 
      if ((room_day.weather.is != WeatherEnum.SNOWY.toString) && (shouted.length != 0)) {
        var shout_enabled = false
        shouted.foreach {shout =>
          val shout_actioner_list = user_entrys.filter(_.id.is    == shout.actioner_id.is).filter(_.live.is)
          if (shout_actioner_list.length != 0)
            shout_enabled = true
        }
        if (shout_enabled)
          user_entrys.filter(x=> (x.current_role == RoleEnum.WEREWOLF) ||
                                 (x.current_role == RoleEnum.WOLFCUB) ||
                                 (x.current_role == RoleEnum.MADMAN) ||
                                 (x.current_role == RoleEnum.SORCEROR))
        else
          List()
      }
      else List()

    // 計算得票數
    votes.foreach { vote =>
      val actioner_list = user_entrys.filter(_.id.is    == vote.actioner_id.is)
      val actioner      = 
        if (actioner_list.length != 0)
          actioner_list(0)
        else
          null
      val vote_actionee = votes.filter(_.actioner_id.is == vote.actionee_id.is)(0)
    
      if ((actioner != null) && (vote.actioner_id.is == vote.actionee_id.is))  { // 自投時固定 1 票
        //vote_actionee.vote_number(vote_actionee.vote_number.is + 2) // 自投時改成 0 票
        vote_actionee.vote_flags(VoteFlagEnum.AUTO.toString + vote.vote_flags.is)
      }
      else if ((room_day.weather.is != WeatherEnum.CLOUDY.toString) && (actioner != null) && (actioner.subrole.is == SubroleEnum.AUTHORITY.toString))
        vote_actionee.vote_number(vote_actionee.vote_number.is + 4)
      else if ((room_day.weather.is != WeatherEnum.CLOUDY.toString) && (actioner != null) && (actioner.subrole.is == SubroleEnum.DECIDER.toString))
        vote_actionee.vote_number(vote_actionee.vote_number.is + 3)
      else 
        vote_actionee.vote_number(vote_actionee.vote_number.is + 2)
        
      if ((actioner != null) && (werewolf_side.contains(actioner))) {
        vote_actionee.vote_number(vote_actionee.vote_number.is + 2)
        vote.vote_flags(vote.vote_flags.is + VoteFlagEnum.SHOUTED.toString)
      }
    }

    // 處理惡魔詛咒術
    val curses = SystemMessage.findAll(By(SystemMessage.roomday_id, room_day.id.is),
                                       By(SystemMessage.mtype, MTypeEnum.VOTE_DEMON_CURSE.toString)) :::
                 SystemMessage.findAll(By(SystemMessage.roomday_id, room_day.id.is),
                                       By(SystemMessage.mtype, MTypeEnum.VOTE_DEMON_CURSE2.toString))
    if ((room_day.weather.is != WeatherEnum.SNOWY.toString) && (curses.length != 0)) {
      curses.foreach{ curse =>
        val curser = user_entrys.filter(_.id.is == curse.actioner_id.is)(0)
        val cursed_target =
          if (curse.actionee_id.is == 0)
            curse.actioner_id.is
          else
            curse.actionee_id.is

        val vote_cursed_list = votes.filter(_.actioner_id.is == cursed_target)

        if (vote_cursed_list.length != 0) {
          val vote_cursed = vote_cursed_list(0)

          if ((curser.live.is) && (vote_cursed.vote_flags.is.indexOf(VoteFlagEnum.CURSED.toString) == -1)) {
            vote_cursed.vote_flags(vote_cursed.vote_flags.is + VoteFlagEnum.CURSED.toString)
            vote_cursed.vote_number(vote_cursed.vote_number.is + 6)
          }
        }
      }
    }

    // 處理哥德法七彩噴射
    val colorsprays = SystemMessage.findAll(By(SystemMessage.roomday_id, room_day.id.is),
                                       By(SystemMessage.mtype, MTypeEnum.VOTE_GODFAT_COLORSPRAY.toString))
    if ((room_day.weather.is != WeatherEnum.SNOWY.toString) && (colorsprays.length != 0)) {
      colorsprays.foreach{ colorspray =>
        val actioner = user_entrys.filter(_.id.is == colorspray.actioner_id.is)(0)
        val colorsprays_target = colorspray.actionee_id.is
        val vote_colorspray_list = votes.filter(_.actioner_id.is == colorsprays_target)

        if (vote_colorspray_list.length != 0) {
          val vote_colorspray = vote_colorspray_list(0)

          if ((actioner.live.is) && (vote_colorspray.vote_flags.is.indexOf(VoteFlagEnum.COLORSPRAY.toString) == -1)) {
            vote_colorspray.vote_flags(vote_colorspray.vote_flags.is + VoteFlagEnum.COLORSPRAY.toString)
            vote_colorspray.vote_number(vote_colorspray.vote_number.is + 2 + (RoleGodfat.combo(user_entrys) * 3))
          }
        }
      }
    }


    // 處理牧師祝福術
    val blesseds = SystemMessage.findAll(By(SystemMessage.roomday_id, room_day.id.is), 
                                       By(SystemMessage.mtype, MTypeEnum.VOTE_CLERIC_BLESS.toString))
    if ((room_day.weather.is != WeatherEnum.SNOWY.toString) && (blesseds.length != 0)) {
      blesseds.foreach{ blessed =>
        val blesser = user_entrys.filter(_.id.is == blessed.actioner_id.is)(0)
        val vote_blessed_list = votes.filter(_.actioner_id.is == blessed.actionee_id.is)

        if (vote_blessed_list.length != 0) {
          val vote_blessed = vote_blessed_list(0)
        
          if ((blesser.live.is) && (vote_blessed.vote_flags.is.indexOf(VoteFlagEnum.BLESSED.toString) == -1)) {
            vote_blessed.vote_flags(vote_blessed.vote_flags.is + VoteFlagEnum.BLESSED.toString)
            vote_blessed.vote_number(Math.max(0, vote_blessed.vote_number.is - 2))
          }
        }
      }
    }

    // 處理惡魔斗轉
    val vortexes = SystemMessage.findAll(By(SystemMessage.roomday_id, room_day.id.is),
                                         By(SystemMessage.mtype, MTypeEnum.VOTE_DEMON_VORTEX.toString))
    if ((room_day.weather.is != WeatherEnum.SNOWY.toString) && (vortexes.length != 0)) {
      vortexes.foreach{ vortex =>
        val vortexer = user_entrys.filter(_.id.is == vortex.actioner_id.is)(0)
        val vote_vortexed_list = votes.filter(_.actioner_id.is == vortex.actionee_id.is)

        if ((vortexer.live.is) && (vote_vortexed_list.length != 0)) {
          val vote_vortexed = vote_vortexed_list(0)

          val vortexer_vote = votes.filter(_.actioner_id.is == vortex.actioner_id.is)(0)
          val vortexer_vote_target = votes.filter(_.actioner_id.is == vortexer_vote.actionee_id.is)(0)

          if (vote_vortexed.vote_flags.is.indexOf(VoteFlagEnum.VORTEX.toString) == -1) {
            vote_vortexed.vote_flags(vote_vortexed.vote_flags.is + VoteFlagEnum.VORTEX.toString)
          }

          val vote_number = vortexer_vote_target.vote_number.is
          vortexer_vote_target.vote_number(0)
          vote_vortexed.vote_number(vote_vortexed.vote_number.is + vote_number)
        }
      }
    }

    // 儲存
    votes.foreach { vote => vote.save }
    
    val votes_sorted = votes.sort(_.vote_number.is > _.vote_number.is)
    
    if ( votes_sorted(0).vote_number.is != votes_sorted(1).vote_number.is) {
      val result_list = user_entrys.filter(_.id.is == votes_sorted(0).actioner_id.is)
      if (result_list.length == 0)
        return WaterElemental
      else
        return result_list(0)
    }
    else
      return null
  }
  
  def generate_vote_tag(room: Room, room_day: RoomDay, vote_time: Int, heaven_mode: Boolean, user_entrys: List[UserEntry]) : scala.xml.Elem = {
    val votes = Vote.findAll(By(Vote.roomday_id, room_day.id.is), By(Vote.vote_time, vote_time),
                             By(Vote.mtype, MTypeEnum.VOTE_HANG.toString),
                             OrderBy(Vote.actioner_id, Ascending)).sort((x,y)=>(x.actioner_id.is<y.actioner_id.is)&&(x.actioner_id.is!=0))
    
    def vote_flags_str(vote:Vote)   : String = {
      val auto_str    = if (vote.vote_flags.is.indexOf(VoteFlagEnum.AUTO.toString) != -1 )    "(自投)" else ""
      val blessed_str = if (vote.vote_flags.is.indexOf(VoteFlagEnum.BLESSED.toString) != -1 ) "(祝福)" else ""
      val cursed_str  = if (vote.vote_flags.is.indexOf(VoteFlagEnum.CURSED.toString) != -1 )  "(詛咒)" else ""
      val shouted_str = if (vote.vote_flags.is.indexOf(VoteFlagEnum.SHOUTED.toString) != -1 ) "(鼓舞)" else ""
      val vortex_str = if (vote.vote_flags.is.indexOf(VoteFlagEnum.VORTEX.toString) != -1 ) "(斗轉)" else ""
      val colored_str = if (vote.vote_flags.is.indexOf(VoteFlagEnum.COLORSPRAY.toString) != -1 ) "(七彩)" else ""
    
      return auto_str + blessed_str + cursed_str + shouted_str + vortex_str + colored_str
    }
    def vote_no(room_day:RoomDay, user:UserEntry, vote_reveal:Boolean, vote_flags:String): String =
      if (!vote_reveal) ""
      else {
        val result = 
          (if (vote_flags.indexOf(VoteFlagEnum.AUTO.toString) == -1) 0
           else if (user==null) 1
           else if ((user.subrole.is == SubroleEnum.AUTHORITY.toString) &&
               (room_day.weather.is != WeatherEnum.CLOUDY.toString)) 2
           else 1) +
          (if (vote_flags.indexOf(VoteFlagEnum.SHOUTED.toString) != -1) 1 else 0)

        result.toString
      }
    
    def vote_tag(vote:Vote, vote_reveal:Boolean)  : scala.xml.Elem = {
      val actioner_list = user_entrys.filter(_.id.is == vote.actioner_id.is)
      val actioner = 
        if (actioner_list.length != 0)
          actioner_list(0)
        else
          null
      val actioner_name =
        if (actioner_list.length != 0)
          <span>{actioner_list(0).handle_name.is}</span>
        else
          <font color="#1E90FF">水元素</font>
      val actionee_list = user_entrys.filter(_.id.is == vote.actionee_id.is)
      val actionee_name =
        if ((room_day.weather.is == WeatherEnum.MISTY.toString) && (! heaven_mode))
          <span></span>
        else if (actionee_list.length != 0)
          <span>{actionee_list(0).handle_name.is}</span>
        else
          <font color="#1E90FF">水元素</font>
    
      return <tr>
         <td align="left"><strong>{actioner_name}</strong></td>
         <td>{(vote.vote_number.is/2).toString+" 票"}</td>
         <td>{"投票給 " + vote_no(room_day, actioner, vote_reveal, vote.vote_flags.is) +" 票 →"}</td>
         <td><strong>{actionee_name}</strong></td>
         <td> {vote_flags_str(vote)}</td>
        </tr>
    }
    val vote_reveal = (room.room_flags.is.indexOf(RoomFlagEnum.VOTE_REVEAL.toString) != -1)

    if (votes.length == 0)
      return <span></span>

    return <table border="1" cellspacing="0" cellpadding="2" style="font-size:12pt;">
      <td colspan="5" align="center">{"第 " + ((room_day.day_no+2)/2).toString + "日 (第 " + vote_time.toString +" 回)"}</td> {
         for (vote <- votes) yield vote_tag(vote, vote_reveal)
      }
    </table>
  }

  def get_vote_tag(room: Room, room_day: RoomDay, user_entrys: List[UserEntry]) = {
    var vote_tag : NodeSeq = NodeSeq.Empty
    (1 to room_day.vote_time.is).toList.reverse.foreach { index =>
      vote_tag = vote_tag ++ Seq(VoteHelper.generate_vote_tag(room, room_day, index, true, user_entrys))
    }
    vote_tag
  }

  def get_vote_tag(room: Room, room_day: RoomDay, user_entrys: List[UserEntry], heaven_mode:Boolean, last_day:RoomDay, last_day2: RoomDay) = {
    var vote_tag_d : NodeSeq = NodeSeq.Empty
    var vote_tag_n : NodeSeq = NodeSeq.Empty
    if (room_day.day_no.is % 2 == 0)  {
      if (room_day.vote_time.is > 1) {
        (1 to room_day.vote_time.is-1).toList.reverse.foreach { index =>
          vote_tag_d = vote_tag_d ++ Seq(VoteHelper.generate_vote_tag(room, room_day, index, heaven_mode, user_entrys))
        }
      }
      if (room_day.day_no.is >= 4) {
        (1 to last_day2.vote_time.is).toList.reverse.foreach { index =>
            vote_tag_n = vote_tag_n ++ Seq(VoteHelper.generate_vote_tag(room, last_day2, index, heaven_mode, user_entrys))
        }
      }
    } else  {
      (1 to last_day.vote_time.is).toList.reverse.foreach { index =>
        vote_tag_n = vote_tag_n ++ Seq(VoteHelper.generate_vote_tag(room, last_day, index, heaven_mode, user_entrys))
      }
    }
    (vote_tag_d, vote_tag_n)
  }
}