package org.plummtw.jinrou.data

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


class RoleData(role: RoleEnum.Value, name: String, color : String, side: RoomVictoryEnum.Value, actions: List[ActionData]) {
  def role_enum         = role
  def role_name         = name 
  def role_color        = color
  def role_side         = side
  def role_actions      = actions
  def role_pic          = <img src="images/rolepic_no.gif" />
  
  def role_intro : scala.xml.Elem = null
  def role_enabled = true
  
  def ctext        = <font color={role_color}>[{role_name}]</font>
  def simple_ctext = <font color={role_color}>[{role_name(0)}]</font>
  def role_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = <span></span>
  def day_actions : List[ActionData] = List(ActionVote)
  
  // 測試是否投過票(行動過)
  def check_voted(room_day: RoomDay, user:UserEntry, vote_list:List[Vote]) : Boolean = {
    if (role_enum != RoleEnum.NONE) {
      val voted = 
        if ((role_enum == RoleEnum.WEREWOLF) && (room_day.day_no.is % 2 == 1))
          vote_list.filter(_.mtype.is == MTypeEnum.VOTE_WEREWOLF.toString)
        else
          vote_list.filter(_.actioner_id.is == user.id.is)
          
      (voted.length !=0)          
    } else false
  }

  // 測試是否投過票(行動過)
  /*
  def check_voted(room_day: RoomDay, user:UserEntry) : Boolean = {
    if (role_enum != RoleEnum.NONE) {
      val voted =
        if ((role_enum == RoleEnum.WEREWOLF) && (room_day.day_no.is % 2 == 1))
          Vote.findAll(By(Vote.roomday_id, room_day.id.is), By(Vote.mtype, MTypeEnum.VOTE_WEREWOLF.toString))
        else
          Vote.findAll(By(Vote.roomday_id, room_day.id.is), By(Vote.actioner_id, user.id.is), By(Vote.vote_time, room_day.vote_time.is))

      (voted.length !=0)
    } else false
  } */

  // 產生角色介紹 Tag
  def generate_role_intro(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = {
    if (user.test_memoryloss(room, room_day, user_entrys))
      RoleVillager.role_intro
    else if (user.test_fake(room_day))
      RoleAugurer.role_intro
    else
      role_intro
  }
  
  // 產生角色能力 Tag
  def generate_role_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = {
    if (user.test_memoryloss(room, room_day, user_entrys))
      RoleVillager.role_ability(room, room_day, user, user_entrys)
    else if (user.test_fake(room_day))
      RoleAugurer.role_ability(room, room_day, user, user_entrys)
    else 
      role_ability(room, room_day, user, user_entrys)
  }
  
  // 產生角色行動 Tag
  def generate_action_tag(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry], vote_list:List[Vote]) : NodeSeq = {
    if (user.test_memoryloss(room, room_day, user_entrys))
      RoleVillager.action_tag(room, room_day, user, user_entrys, vote_list)
    else if (user.test_fake(room_day))
      RoleAugurer.action_tag(room, room_day, user, user_entrys, vote_list)
    else 
      action_tag(room, room_day, user, user_entrys, vote_list)
  }
  
  // 針對角色 列出可以的行動
  def list_actions_enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[ActionData] = {
    val actions = if ((room_day.day_no.is %2 == 0) &&
                      (room_day.day_no.is != 0))
                    day_actions
                  else
                    role_actions        
    
    val actions_enabled = actions.filter(_.enabled(room, room_day, user, user_entrys))
    
    if ((actions_enabled.length == 1) && (actions_enabled(0).isInstanceOf[NoActionTrait]))
      return List()
    else if ((room_day.day_no.is %2 == 1) && (room_day.weather.is == WeatherEnum.TYPHOON.toString) && (user.role.is.substring(0,1) != RoleEnum.WEREWOLF.toString))
      return List()
    //else if ((actions_enabled.length == 1) && (actions_enabled(0) == ActionNoAction2))
    //  return List()
    else
      return actions_enabled
  }
  
  // 產生 Action Tag
  def action_tag(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry], vote_list:List[Vote]) : NodeSeq = {
    if (room.status.is == RoomStatusEnum.ENDED.toString)
      return Seq(<span>＜遊戲已經結束＞</span>)
    else if (!user.live.is)
      return Seq(<span>＜已死亡無法行動＞</span>)

    val is_memoryloss = user.test_memoryloss(room, room_day, user_entrys)
    val is_voted =
      if (is_memoryloss)
        RoleVillager.check_voted(room_day, user, vote_list)
      else if (user.test_fake(room_day))
        RoleAugurer.check_voted(room_day, user, vote_list)
      else
        check_voted(room_day, user, vote_list)

    if (is_voted) {
      if (room_day.day_no.is % 2 == 1)
        return Seq(<span>＜你已經行動過了＞</span>)
      else
        return Seq(<span>＜你已經投票過了＞</span>)
    }
    
    val actions_enabled =
      if (is_memoryloss)
        RoleVillager.list_actions_enabled(room, room_day, user, user_entrys)
      else if (user.test_fake(room_day))
        RoleAugurer.list_actions_enabled(room, room_day, user, user_entrys)
      else
        list_actions_enabled(room, room_day, user, user_entrys)
    
    if (actions_enabled.length == 0)
      return Seq(<span>＜你不能行動＞</span>)
      
    var result : NodeSeq = Seq()
    actions_enabled.foreach( action =>
      if (action.targetable)
        result = result ++ Seq(<a href={"up_action.html?room_no=" + room.id.is.toString + "&command=" + action.command_name}>{action.toString}</a>)
      else
        //result = result ++ Seq(<a href="#" onClick={"self.document.send.say.value='';self.document.send.command.value='" + action.command_name +
        //           "';self.document.send.submit();return false;"}>{action.toString}</a>)
        result = result ++ Seq(<a href={"javascript:submit_action('" + action.command_name + "')"}>{action.toString}</a>)
    )
    return result
  }
  
  // 產生可以行動的 行動 List
  def check_action_list(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry], vote_list:List[Vote]) : List[ActionData] = {
    if (room.status.is == RoomStatusEnum.ENDED.toString)
      return List()
    else if (!user.live.is)
      return List()

    val is_memoryloss = user.test_memoryloss(room, room_day, user_entrys)
    val is_voted =
      if (is_memoryloss)
        RoleVillager.check_voted(room_day, user, vote_list)
      else if (user.test_fake(room_day))
        RoleAugurer.check_voted(room_day, user, vote_list)
      else
        check_voted(room_day, user, vote_list)

    if (is_voted)
      return List()
    
    val actions_enabled = 
      if (is_memoryloss)
        RoleVillager.list_actions_enabled(room, room_day, user, user_entrys)
      else if (user.test_fake(room_day))
        RoleAugurer.list_actions_enabled(room, room_day, user, user_entrys)
      else 
        list_actions_enabled(room, room_day, user, user_entrys)
    
    return actions_enabled
  }  
    
  override def toString(): String = role_name
}

object RoleNone        extends RoleData(RoleEnum.NONE,        "不指定", "", RoomVictoryEnum.VILLAGER_WIN, List(ActionKick, ActionStartGame)) {
  override def role_enabled = false
}

object RoleVillager    extends RoleData(RoleEnum.VILLAGER,    "村民", "", RoomVictoryEnum.VILLAGER_WIN, List(ActionVillagerDetect, ActionBecomeMob ,ActionNoAction)) {
  override def role_intro = <img src="images/role_human.gif"/>
  override def role_pic   = <img src="images/rolepic_human.gif" />

  override def ctext        = <span>[村民]</span>
  override def simple_ctext = <span>[村]</span>

  override def role_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = {
    if  ((room.has_flag(RoomFlagEnum.VILLAGER_DETECT)) &&
         (room_day.day_no.is == 8) ) {
      val  system_message = SystemMessage.findAll(By(SystemMessage.roomday_id,  room_day.id.is),
                                                  By(SystemMessage.actioner_id, user.id.is),
                                                  By(SystemMessage.mtype,       MTypeEnum.VOTE_VILLAGER.toString))
      if   (system_message.length == 0)
        <span></span>
      else {
        //val actionee   = UserEntry.findAll(By(UserEntry.id, system_message(0).actionee_id.is))(0)
        val actionee   = user_entrys.filter(_.id.is == system_message(0).actionee_id.is)(0)
        
        val yes_no_pic = (if (actionee.role.is.substring(0,1) == RoleEnum.VILLAGER.toString)  <img src="images/yes.gif" />
                         else <img src="images/no.gif" />)
          
        <table cellSpacing="0" cellPadding="0" border="1"><tbody>
        <tr><td><img src="images/role_human_detect.gif" /></td><td>{actionee.handle_name.is}{yes_no_pic}<img src="images/rolepic_human.gif" /></td></tr></tbody></table>
      } 
    } else
      <span></span>   
  }
}

object RoleHermit    extends RoleData(RoleEnum.HERMIT,    "隱士", "#888888", RoomVictoryEnum.VILLAGER_WIN, List(ActionHide, ActionReverseVote ,ActionNoAction)) {
  override def role_intro = <img src="images/role_hermit.gif"/>
  override def role_pic   = <img src="images/rolepic_hermit.gif" />
}



object RoleAugurer     extends RoleData(RoleEnum.AUGURER,     "占卜師", "#9933FF", RoomVictoryEnum.VILLAGER_WIN, List(ActionAugure, ActionBecomeMob)) {
  override def role_intro = <img src="images/role_mage.gif"/>
  override def role_pic   = <img src="images/rolepic_mage.gif" />
  
  def augurer_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry], system_message0:SystemMessage) = {
      
    var  color_tag  : scala.xml.Elem = null
    var  result_tag : scala.xml.Elem = null  
    val actionee   = user_entrys.filter(_.id.is == system_message0.actionee_id.is)(0)
    if (((actionee.role.is.substring(0,1) == RoleEnum.WEREWOLF.toString) ||
         (actionee.role.is.substring(0,1) == RoleEnum.WOLFCUB.toString)  ||
         (actionee.subrole.is == SubroleEnum.WOLFSTAMP.toString)  ||
         ((actionee.role.is.substring(0,1) == RoleEnum.DEMON.toString)  && (room.has_flag(RoomFlagEnum.DEMON_OPTION1)))) &&
         (system_message0.message.is.indexOf(VoteFlagEnum.FAKE.toString) == -1)) {
      color_tag  = <font color="red">{actionee.handle_name.is}</font>
      result_tag = <img src="images/role_result_wolf.gif"/> 
    } else {
      color_tag =  <span>{actionee.handle_name.is}</span>
      result_tag = <img src="images/role_result_human.gif"/>
    }

    if (user.test_fake(room_day)) {
      if (actionee.user_no.is %5 == 0) {
        color_tag  = <font color="red">{actionee.handle_name.is}</font>
        result_tag = <img src="images/role_result_wolf.gif"/>
      } else {
        color_tag =  <span>{actionee.handle_name.is}</span>
        result_tag = <img src="images/role_result_human.gif"/>
      }
    }
           
    <table cellSpacing="0" cellPadding="0" border="1"><tbody>
     <tr><td><img src="images/role_mage_result.gif"/></td>
      <td>{color_tag}</td>
      <td>{result_tag}</td></tr></tbody></table>    
  }

  override def role_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = {
    if ((room_day.day_no.is != 0) && (room_day.day_no.is %2 == 0)) {
      val  system_message = SystemMessage.findAll(By(SystemMessage.roomday_id,  room_day.id.is),
                                                  By(SystemMessage.actioner_id, user.id.is),
                                                  By(SystemMessage.mtype,       MTypeEnum.VOTE_AUGURER.toString))

          
      if   (system_message.length == 0)
        <span></span>
      else {
        //val actionee = UserEntry.findAll(By(UserEntry.id, system_message(0).actionee_id.is))(0)
        augurer_ability(room, room_day, user, user_entrys, system_message(0))
      }
    } else
      <span></span>
  }
}

object RoleNecromancer extends RoleData(RoleEnum.NECROMANCER, "靈能者", "#009900", RoomVictoryEnum.VILLAGER_WIN, List(ActionBecomeMob ,ActionNoAction)) {
  override def role_intro = <img src="images/role_necromancer.gif"/>
  override def role_pic   = <img src="images/rolepic_necromancer.gif" />
  
  override def role_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = {
    if ((room_day.day_no.is != 0) && (room_day.day_no.is %2 == 0)) {
      val  room_days      = RoomDay.findAll(By(RoomDay.room_id, room.id.is), OrderBy(RoomDay.day_no, Descending))
      val  system_messages = SystemMessage.findAll(By(SystemMessage.roomday_id,  room_days(2).id.is),
                                                  By(SystemMessage.mtype,       MTypeEnum.DEATH_HANGED.toString)) ::: 
                            SystemMessage.findAll(By(SystemMessage.roomday_id,  room_days(1).id.is),
                                                  By(SystemMessage.mtype,       MTypeEnum.DEATH_SUDDEN.toString)) :::
                            SystemMessage.findAll(By(SystemMessage.roomday_id,  room_days(2).id.is),
                                                  By(SystemMessage.mtype,       MTypeEnum.DEATH_SUDDEN.toString))
    
      def  color_tag(handle_name : String, role_str : String)   =
        if (role_str == RoleEnum.WEREWOLF.toString)
          <font color="red">{handle_name}</font>
        else if ((role_str == RoleEnum.DEMON.toString) || (role_str == RoleEnum.FALLEN_ANGEL.toString))
          <font color="#666666">{handle_name}</font>
        else if ((role_str == RoleEnum.FOX.toString) && (room.has_flag(RoomFlagEnum.NECROMANCER_OPTION1)))
          <font color="#CC0099">{handle_name}</font>
        else 
          <span>{handle_name}</span>
       
      def  result_tag(role_str : String)  =
        if (role_str == RoleEnum.WEREWOLF.toString)
          <img src="images/role_result_wolf.gif"/> 
        else if (role_str == RoleEnum.DEMON.toString) 
          <img src="images/role_result_demon.gif"/>
        else if ((role_str == RoleEnum.FOX.toString) && (room.has_flag(RoomFlagEnum.NECROMANCER_OPTION1)))
          <img src="images/role_result_fox.gif"/>
        else
          <img src="images/role_result_human.gif"/>
          
      def  necromancer_tag(message : SystemMessage) : NodeSeq = {
        if (message.actioner_id.is == 0)
          return NodeSeq.Empty

        val actioner = UserEntry.findAll(By(UserEntry.id, message.actioner_id.is))(0)
        val role_string = if (actioner.role.is.substring(0,1) == RoleEnum.WOLFCUB.toString) RoleEnum.WEREWOLF.toString
                          else if (actioner.subrole.is == SubroleEnum.WOLFSTAMP.toString) RoleEnum.WEREWOLF.toString
                          else actioner.role.is.substring(0,1)

        Seq(<tr><td><img src="images/role_necromancer_result.gif"/></td>
         <td>{color_tag(actioner.handle_name.is, role_string)}</td>
         <td>{result_tag(role_string)}</td></tr>)
      }
      
      if   (system_messages.length == 0)
        <span></span>
      else 
        <table cellSpacing="0" cellPadding="0" border="1"><tbody>
        { for (message <- system_messages) yield
            necromancer_tag(message)  
        } </tbody></table>
    } else 
      <span></span>
  }
}

object RoleHunter      extends RoleData(RoleEnum.HUNTER,      "獵人",   "#3399FF", RoomVictoryEnum.VILLAGER_WIN, List(ActionGuard, ActionBecomeMob)) {
  override def role_intro = <img src="images/role_guard.gif"/>
  override def role_pic   = <img src="images/rolepic_guard.gif" />
 
  def hunter_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry], system_message0:SystemMessage) = {
    val  system_message = SystemMessage.findAll(By(SystemMessage.roomday_id,  room_day.id.is),
                                                By(SystemMessage.mtype,       MTypeEnum.VOTE_WEREWOLF.toString))
    val actionee   = user_entrys.filter(_.id.is == system_message0.actionee_id.is)(0)
    if (system_message(0).actionee_id.is == actionee.id.is) 
      <table cellSpacing="0" cellPadding="0" border="1"><tbody>
        <tr><td>{actionee.handle_name.is}</td><td><img src="images/role_guard_success.gif"/></td>
        </tr></tbody></table>
    else
      <span></span>
  }

  override def role_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = {
    if ((room_day.day_no.is != 0) && (room_day.day_no.is %2 == 0)) {
      val  system_message_hunter = SystemMessage.findAll(By(SystemMessage.roomday_id,  room_day.id.is),
                                                         By(SystemMessage.actioner_id, user.id.is),
                                                         By(SystemMessage.mtype,       MTypeEnum.VOTE_HUNTER.toString))                                                  
    
      if   (system_message_hunter.length == 0)
        <span></span>
      else {
        //val actionee = UserEntry.findAll(By(UserEntry.id, system_message_hunter(0).actionee_id.is))(0)
        hunter_ability(room, room_day, user, user_entrys, system_message_hunter(0))
      }
    }
    else
      <span></span>
  }
}

object RoleGemini      extends RoleData(RoleEnum.GEMINI,      "共有者", "#CC9966", RoomVictoryEnum.VILLAGER_WIN, List(ActionBecomeMob ,ActionNoAction)) {
  override def role_intro = <img src="images/role_common.gif"/>
  override def role_pic   = <img src="images/rolepic_common.gif" />
  
  override def role_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = {
    val gemini = UserEntry.findAll(By(UserEntry.room_id, room.id.is),
                                   NotBy(UserEntry.id, user.id.is),
                                   Like(UserEntry.role, RoleEnum.GEMINI.toString+"%"))
                                   
    val gemini_str = gemini.map(_.handle_name.is).mkString("　","　　","")
    //val gemini_str = ""
    <table cellSpacing="0" cellPadding="0" border="1"><tbody>
      <tr><td><img src="images/role_common_partner.gif"/></td>
      <td>{gemini_str}</td></tr></tbody></table>
                
  }
}

object RoleCleric      extends RoleData(RoleEnum.CLERIC,      "牧師",   "#CCDD00", RoomVictoryEnum.VILLAGER_WIN, List(ActionClericBless, ActionClericSancture, ActionNoAction)) {
  //override def role_intro = <span>[角色]<br/>　　您所扮演的角色是牧師，您可以施行祝福術給予祝福，施行聖術犧牲自己拯救村民，並可察覺惡魔儀式以警告村民。</span>
  override def role_intro = <img src="images/role_cleric.gif"/>
  override def role_pic   = <img src="images/rolepic_cleric.gif" />
  
  override def role_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = {
    if ((room_day.day_no.is != 0) && (room_day.day_no.is %2 == 0)) {
      val system_message = SystemMessage.findAll(By(SystemMessage.roomday_id,  room_day.id.is),
                                                 By(SystemMessage.mtype,       MTypeEnum.VOTE_WEREWOLF.toString))
      //val actionee       = UserEntry.findAll(By(UserEntry.id, system_message(0).actionee_id.is))(0)
      val actionee   = user_entrys.filter(_.id.is == system_message(0).actionee_id.is)(0)
      val system_message_hunter = SystemMessage.findAll(By(SystemMessage.roomday_id,  room_day.id.is),
                                                        By(SystemMessage.actionee_id, actionee.id.is),
                                                        By(SystemMessage.mtype,       MTypeEnum.VOTE_HUNTER.toString))

      val demon_tag =
        if (actionee.role.is.substring(0,1) == RoleEnum.DEMON.toString) {
          if (system_message(0).message.is.indexOf(VoteFlagEnum.POWER.toString) != -1)
            Seq(<tr><td><img src="images/role_cleric_demon_dead.gif" /></td></tr>)
          else if (system_message_hunter.length == 0)
            Seq(<tr><td><img src="images/role_cleric_demon_attacks.gif" /></td></tr>)
          else
            NodeSeq.Empty
        } else NodeSeq.Empty

      val betrayer_tag =
        if (room.has_flag(RoomFlagEnum.CLERIC_OPTION2)) {
          val system_message_betrayer = SystemMessage.findAll(By(SystemMessage.roomday_id,  room_day.id.is),
                                                              By(SystemMessage.mtype, MTypeEnum.VOTE_BETRAYER_DISGUISE.toString))
          if (system_message_betrayer.length != 0)
            Seq(<tr><td><img src="images/role_cleric_camouflague.gif" /></td></tr>)
          else
            NodeSeq.Empty
        } else NodeSeq.Empty


      <table cellSpacing="0" cellPadding="0" border="1"><tbody>{demon_tag}{betrayer_tag}</tbody></table>
    } else
      <span></span>
  }
}

object RoleHerbalist   extends RoleData(RoleEnum.HERBALIST,   "藥師",   "#8FCECE", RoomVictoryEnum.VILLAGER_WIN, List(ActionHerbalistElixir, ActionHerbalistPoison, ActionHerbalistMix,ActionHerbalistDrop, ActionBecomeMob, ActionNoAction)) {
  //override def role_intro = <span>[角色]<br/>　　您所扮演的角色是藥師，您可以使用治療藥和毒藥來幫助村民。</span>
  override def role_intro = <img src="images/role_herbalist.gif"/>
  override def role_pic   = <img src="images/rolepic_herbalist.gif" />
  
  override def role_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = {
    var herbs : NodeSeq = Seq()
    //val herbs_elixir = if (user.hasnt_flag(UserEntryFlagEnum.ELIXIR_USED)) List("治療藥") else List("")
    //val herbs_poison = if (user.hasnt_flag(UserEntryFlagEnum.POISON_USED)) List("毒藥")   else List("")
    //val herbs = herbs_elixir ::: herbs_poison
    if (user.hasnt_flag(UserEntryFlagEnum.ELIXIR_USED)) {herbs ++= <img src="images/role_herbalist_elixir.gif" /> }
    if (user.hasnt_flag(UserEntryFlagEnum.POISON_USED)) {herbs ++= <img src="images/role_herbalist_poison.gif" /> }
  
    <table cellSpacing="0" cellPadding="0" border="1"><tbody>
      <tr><td><img src="images/role_herbalist_drug.gif" /></td><td>{herbs}</td></tr></tbody></table>
  }
}

object RoleAlchemist   extends RoleData(RoleEnum.ALCHEMIST,   "鍊金術士",   "#8FCECE", RoomVictoryEnum.VILLAGER_WIN, List(ActionAlchemistElixir, ActionAlchemistPoison, ActionBecomeMob, ActionNoAction)) {
  //override def role_intro = <span>[角色]<br/>　　您所扮演的角色是藥師，您可以使用治療藥和毒藥來幫助村民。</span>
  override def role_intro = <img src="images/role_alchemist.gif"/>
  override def role_pic   = <img src="images/rolepic_alchemist.gif" />

  override def role_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = {
    var herbs : NodeSeq = Seq()
    //val herbs_elixir = if (user.hasnt_flag(UserEntryFlagEnum.ELIXIR_USED)) List("治療藥") else List("")
    //val herbs_poison = if (user.hasnt_flag(UserEntryFlagEnum.POISON_USED)) List("毒藥")   else List("")
    //val herbs = herbs_elixir ::: herbs_poison
    if (user.has_flag(UserEntryFlagEnum.EARTH)) {herbs ++= <span>地</span> }
    if (user.has_flag(UserEntryFlagEnum.WATER)) {herbs ++= <span>水</span> }
    if (user.has_flag(UserEntryFlagEnum.AIR))   {herbs ++= <span>風</span> }
    if (user.has_flag(UserEntryFlagEnum.FIRE))  {herbs ++= <span>火</span> }

    <table cellSpacing="0" cellPadding="0" border="1"><tbody>
      <tr><td><img src="images/role_alchemist_drug.gif" /></td><td>{herbs}</td></tr></tbody></table>
  }
}

object RolePoisoner    extends RoleData(RoleEnum.POISONER,    "埋毒者", "#006633", RoomVictoryEnum.VILLAGER_WIN, List(ActionBecomeMob, ActionNoAction)) {
  override def role_intro = <img src="images/role_poison.gif"/>
  override def role_pic   = <img src="images/rolepic_poison.gif" />
}

object RoleRunner      extends RoleData(RoleEnum.RUNNER,      "逃亡者", "#009999", RoomVictoryEnum.VILLAGER_WIN, List(ActionRun)) {
  //override def role_intro = <span>[角色]<br/>　　您所扮演的角色是逃亡者，逃亡到不同的人家以躲避人狼的襲擊，由於您忙於逃亡所以沒時間準備遺書。</span>
  override def role_intro = <img src="images/role_runner.gif"/>
  override def role_pic   = <img src="images/rolepic_runner.gif" />

  def runner_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry], system_message0:SystemMessage) = {
    val actionee   = user_entrys.filter(_.id.is == system_message0.actionee_id.is)(0)
    val subrole_data = SubroleEnum.get_subrole(actionee.subrole.is).toString
    if (actionee.current_role == RoleEnum.PONTIFF)
      <table cellSpacing="0" cellPadding="0" border="1"><tbody>
       <tr><td><img src="images/role_runner_result.gif" /></td><td>{actionee.handle_name.is}</td>
       <td><img src="images/role_result_pontiff.gif"/></td></tr></tbody></table>
    else <span></span>
  }

  override def role_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = {
    if ((room_day.day_no.is != 0) && (room_day.day_no.is %2 == 0) &&
        (room.has_flag(RoomFlagEnum.RUNNER_OPTION3))) {
      val  system_message = SystemMessage.findAll(By(SystemMessage.roomday_id,  room_day.id.is),
                                                  By(SystemMessage.actioner_id, user.id.is),
                                                  By(SystemMessage.mtype,      MTypeEnum.VOTE_RUNNER.toString))
      if (system_message.length != 0) {
        runner_ability(room, room_day, user, user_entrys, system_message(0))
      } else <span></span>
    } else <span></span>
  }
}

object RoleAugHunter   extends RoleData(RoleEnum.AUGHUNTER,   "占卜獵人", "#9933FF", RoomVictoryEnum.VILLAGER_WIN, List(ActionAugHunterAugure, ActionAugHunterGuard, ActionBecomeMob)) {
  //override def role_intro = <span>[角色]<br/>　　您所扮演的角色是占卜獵人。您在奇數日晚上可如同占卜師般占卜、偶數日晚上可如同獵人般護衛。</span>
  override def role_intro = <img src="images/role_aughunter.gif"/>
  override def role_pic   = <img src="images/rolepic_aughunter.gif" />

  def role_color2  = "#3399FF"

  override def ctext = <span><font color="#9933FF">[占卜</font><font color="#3399FF">獵人]</font></span>
  override def role_enabled = false
  
  override def role_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = {
    if ((room_day.day_no.is != 0) && (room_day.day_no.is %2 == 0)) {
      if (room_day.day_no.is %4 == 2)
        RoleAugurer.role_ability(room, room_day, user, user_entrys)
      else
        RoleHunter.role_ability(room, room_day, user, user_entrys)
    }
    else
      <span></span>
  }
}

object RoleScholar     extends RoleData(RoleEnum.SCHOLAR,     "學者",   "#3CB371", RoomVictoryEnum.VILLAGER_WIN, List(ActionScholarExamine,ActionScholarExamine2,ActionScholarAnalyze,ActionScholarReport,ActionBecomeMob)) {
  //override def role_intro = <span>[角色]<br/>　　您所扮演的角色是學者。你可以針對調查一個人的副職業及是否是教徒，以及分析事件一次及瞭解現況一次。</span>
  override def role_intro = <img src="images/role_scholar.gif"/>
  override def role_pic   = <img src="images/rolepic_scholar.gif" />

  def scholar_examine(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry], system_message0: SystemMessage) = {
    // 個案調查
    val actionee   = user_entrys.filter(_.id.is == system_message0.actionee_id.is)(0)
    val subrole_data =
      //if (room.has_flag(RoomFlagEnum.SCHOLAR_OPTION4) && (actionee.sex.is == user.sex.is))
      //  SubroleEnum.get_subrole(actionee.subrole.is).subrole_pic_true //subrole_name
      //else
        SubroleEnum.get_subrole(actionee.subrole.is).subrole_pic      //toString
    val is_religion =
     if (actionee.current_role == RoleEnum.PONTIFF) {
       if (room.has_flag(RoomFlagEnum.SCHOLAR_OPTION1))
         Seq(<img src="images/yes.gif"/>, <img src="images/rolepic_pontiff.gif"/>) //"是教主"
       else
         Seq(<img src="images/yes.gif"/>, <img src="images/religion.gif"/>) //"是教徒"
     }
     else Seq(if (actionee.has_flag(UserEntryFlagEnum.RELIGION)) <img src="images/yes.gif"/> else <img src="images/no.gif"/>, <img src="images/religion.gif"/>)

    <table cellSpacing="0" cellPadding="0" border="1"><tbody>
      <tr><td><img src="images/role_scholar_examine.gif" /></td><td>{actionee.handle_name.is}</td>
      <td>{subrole_data}</td><td>{is_religion}</td></tr></tbody></table>
  }

  def scholar_examine2(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry], system_message0: SystemMessage) = {
    // 個案調查
    val actionee   = user_entrys.filter(_.id.is == system_message0.actionee_id.is)(0)
    val subrole_data = if ((actionee.subrole.is == SubroleEnum.WOLFSTAMP.toString) &&
                           (room.has_flag(RoomFlagEnum.SUBROLE_WISEWOLF)))
        SubroleWiseWolf.subrole_pic
      else if (actionee.get_werewolf_special != RoleSpecialEnum.WHITE.toString)
        SubroleEnum.get_subrole(actionee.subrole.is).subrole_pic_true //subrole_name
      else
        SubroleEnum.get_subrole(actionee.subrole.is).subrole_pic      //toString
    val is_religion =
     if (actionee.current_role == RoleEnum.PONTIFF) {
       if (room.has_flag(RoomFlagEnum.SCHOLAR_OPTION1))
         Seq(<img src="images/yes.gif"/>, <img src="images/rolepic_pontiff.gif"/>) //"是教主"
       else
         Seq(<img src="images/yes.gif"/>, <img src="images/religion.gif"/>) //"是教徒"
     }
     else Seq(if (actionee.has_flag(UserEntryFlagEnum.RELIGION)) <img src="images/yes.gif"/> else <img src="images/no.gif"/>, <img src="images/religion.gif"/>)

    <table cellSpacing="0" cellPadding="0" border="1"><tbody>
      <tr><td><img src="images/role_scholar_examine.gif" /></td><td>{actionee.handle_name.is}</td>
      <td>{subrole_data}</td><td>{is_religion}</td></tr></tbody></table>
  }

  override def role_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = {
    if ((room_day.day_no.is != 0) && (room_day.day_no.is %2 == 0)) {
      val  system_message = SystemMessage.findAll(By(SystemMessage.roomday_id,  room_day.id.is),
                                                  By(SystemMessage.actioner_id, user.id.is),
                                                Like(SystemMessage.mtype,      MTypeEnum.VOTE.toString+"%"))
      if (system_message.length != 0) {
        val system_message0 = system_message(0)
        if (system_message0.mtype.is == MTypeEnum.VOTE_SCHOLAR_EXAMINE.toString) {
          scholar_examine(room, room_day, user, user_entrys, system_message0)
        } else if (system_message0.mtype.is == MTypeEnum.VOTE_SCHOLAR_EXAMINE2.toString) {
          scholar_examine2(room, room_day, user, user_entrys, system_message0)
        } else if (system_message0.mtype.is == MTypeEnum.VOTE_SCHOLAR_ANALYZE.toString) {
          val last_day3         = RoomDay.findAll(By(RoomDay.room_id, room.id.is), By(RoomDay.day_no, room_day.day_no.is - 3))(0)
          val vote_last_day3    = Vote.findAll(By(Vote.roomday_id, last_day3.id.is))
          val death_last_day3  = SystemMessage.findAll(By(SystemMessage.roomday_id, last_day3.id.is))

          val werewolf_votes           = vote_last_day3.filter(_.mtype.is == MTypeEnum.VOTE_WEREWOLF.toString)
          val hunter_votes             = vote_last_day3.filter(_.mtype.is == MTypeEnum.VOTE_HUNTER.toString)
          val archmage_dispell_votes   = vote_last_day3.filter(_.mtype.is == MTypeEnum.VOTE_ARCHMAGE_DISPELL.toString)
          val herbalist_elixir_votes   = vote_last_day3.filter(_.mtype.is == MTypeEnum.VOTE_HERBALIST_ELIXIR.toString)
          val alchemist_elixir_votes   = vote_last_day3.filter(_.mtype.is == MTypeEnum.VOTE_ALCHEMIST_ELIXIR.toString)
          val cleric_sancture_votes    = vote_last_day3.filter(_.mtype.is == MTypeEnum.VOTE_CLERIC_SANCTURE.toString)
          val runner_votes             = vote_last_day3.filter(_.mtype.is == MTypeEnum.VOTE_RUNNER.toString)
          val werewolf_biter           = user_entrys.filter(_.id.is == werewolf_votes(0).actioner_id.is)(0)
          val werewolf_target          = user_entrys.filter(_.id.is == werewolf_votes(0).actionee_id.is)(0)

          var werewolf_result          = "role_scholar_failure_unknown.gif" //"失敗(？？)"
          if (death_last_day3.filter(_.mtype.is == MTypeEnum.DEATH_EATEN.toString).length != 0)
            werewolf_result = "role_scholar_success.gif" //"成功"
          else if ((werewolf_target.current_role == RoleEnum.RUNNER) && (room_day.day_no.is == 4))
            werewolf_result = "role_scholar_failure_runner.gif" //"失敗(逃亡者)"
          else if (hunter_votes.filter(_.actionee_id.is == werewolf_target.id.is).length != 0)
            werewolf_result = "role_scholar_failure_hunter.gif" //"失敗(獵人)"
          else if (hunter_votes.filter(_.actionee_id.is == werewolf_biter.id.is).length != 0)
            werewolf_result = "role_scholar_failure_archmage.gif" //"失敗(大魔導)"
          else if (werewolf_target.current_role == RoleEnum.FOX)
            werewolf_result = "role_scholar_failure_fox.gif" //"失敗(妖狐)"
          else if (werewolf_target.current_role == RoleEnum.DEMON)
            werewolf_result = "role_scholar_failure_demon.gif" //"失敗(惡魔)"
          else if (werewolf_target.current_role == RoleEnum.DEMON)
            werewolf_result = "role_scholar_failure_penguin.gif" //"失敗(企鵝)"
          else if (werewolf_target.current_role == RoleEnum.WOLFCUB)
            werewolf_result = "role_scholar_failure_wolfcub.gif" //"失敗(幼狼)"
          else if (herbalist_elixir_votes.filter(_.actionee_id.is == werewolf_target.id.is).length != 0)
            werewolf_result = "role_scholar_failure_herbalist.gif" //"失敗(藥師)"
          else if (alchemist_elixir_votes.filter(_.actionee_id.is == werewolf_target.id.is).length != 0)
            werewolf_result = "role_scholar_failure_alchemtst.gif" //"失敗(鍊金術士)"
          else if (cleric_sancture_votes.length != 0)
            werewolf_result = "role_scholar_failure_cleric.gif" //"失敗(牧師)"

          def death_analyze(death: SystemMessage) = {
            val actioner_list   = user_entrys.filter(_.id.is == death.actioner_id.is)
            val actioner_name   =
              if (actioner_list.length != 0)
                <span>{actioner_list(0).handle_name.is}</span>
              else
                <font color="#1E90FF">水元素</font>
            //val death_text = MTypeEnum.get_death_text(death.mtype.is)
            val death_gif = MTypeEnum.get_death_gif(death.mtype.is)

            <tr><td>{actioner_name}</td><td><img src={"images/" + death_gif} /></td></tr>
          }

          <table cellSpacing="0" cellPadding="0" border="1"><tbody>
           <tr><td colspan="2"><img src="images/role_scholar_analysis.gif"/></td></tr>
           <tr><td><img src="images/role_scholar_analysis_wolf.gif"/></td><td><img src={"images/" + werewolf_result} /></td></tr>
           { for (death <- death_last_day3) yield
               death_analyze(death)}
           </tbody></table>
        } else if (system_message0.mtype.is == MTypeEnum.VOTE_SCHOLAR_REPORT.toString) {
          val live_users = user_entrys.filter(_.live.is)

          // 產生人數字串
          //var role_text = new StringBuffer("")
          var role_nodeseq : NodeSeq = Seq()

          if (room.room_flags.is.indexOf(RoomFlagEnum.SCHOLAR_OPTION2.toString) == -1) {
            val side_list = List(RoomVictoryEnum.VILLAGER_WIN, RoomVictoryEnum.WEREWOLF_WIN,
                                 RoomVictoryEnum.FOX_WIN, RoomVictoryEnum.DEMON_WIN, RoomVictoryEnum.PONTIFF_WIN)
            side_list.foreach{side =>
              var side_number = live_users.filter{x=>RoleEnum.get_role(x.role.is.substring(0,1)).role_side == side}.length

              if (side_number > 0) {
                //role_text.append("　")
                val side_name = side match {
                  case RoomVictoryEnum.VILLAGER_WIN => "side_villager.gif" //"人側職業"
                  case RoomVictoryEnum.WEREWOLF_WIN => "side_wolf.gif" //"狼側職業"
                  case RoomVictoryEnum.FOX_WIN      => "side_fox.gif" //"狐側職業"
                  case RoomVictoryEnum.DEMON_WIN    => "side_demon.gif" //"惡魔"
                  case RoomVictoryEnum.PONTIFF_WIN  => "side_pontiff.gif" //"教主"
                  case xs                           => "side_unknown.gif" //"？？"
                }

                //role_text.append(side_name)
                //role_text.append(" ")
                role_nodeseq ++= <img src={"images/"+ side_name} />

                //role_text.append(side_number.toString)
                role_nodeseq ++= <span>{side_number.toString}</span>
              }
            }

          } else {
            val role_list = RoleEnum.ROLE_MAP.keys.toList.filter(x => (x != RoleNone) && (RoleEnum.get_role(x).role_side != RoomVictoryEnum.VILLAGER_WIN))
            role_list.foreach{role =>
              var role_number = live_users.filter(_.current_role == role).length

              if (role_number > 0) {
                //role_text.append("　")
                //role_text.append(RoleEnum.get_role(role).role_name)
                role_nodeseq ++=  RoleEnum.get_role(role).role_pic 
                //role_text.append(" ")
                //role_text.append(role_number.toString)
                role_nodeseq ++= <span>{role_number.toString}</span>
              }
            }
          }

          <table cellSpacing="0" cellPadding="0" border="1"><tbody>
           <tr><td><img src="images/role_scholar_report.gif" /></td><td>{role_nodeseq}</td></tr></tbody></table>
        } else
        <span></span>
      } else
      <span></span>
    } else
      <span></span>
  }

}

object RoleArchmage    extends RoleData(RoleEnum.ARCHMAGE,    "大魔導", "#7B68EE", RoomVictoryEnum.VILLAGER_WIN, List(ActionDispell, ActionSummon, ActionNoAction)) {
  //override def role_intro = <span>[角色]<br/>　　您所扮演的角色是大魔導。恭禧您，在經過漫長的修練，終於轉職成大魔導了。</span>
  override def role_intro = <img src="images/role_archmage.gif"/>
  override def role_pic   = <img src="images/rolepic_archmage.gif" />

  override def role_enabled = false

  
  override def role_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = {
    val action_point_str = user.action_point.is.toString + "/10"
    //val spell_barrier = if (user.hasnt_flag(UserEntryFlagEnum.BARRIER_USED)) List("魔法護盾") else List("")
    val spell_water_elem = if (user.hasnt_flag(UserEntryFlagEnum.WATER_ELEM_USED)) <span>水元素</span>   else <span />
    //val spells = spell_barrier ::: spell_water_elem

    <table cellSpacing="0" cellPadding="0" border="1"><tbody>
      <tr><td><img src="images/mana.gif" /></td><td>{action_point_str}</td></tr>
      <tr><td><img src="images/magic_in_effect.gif" /></td><td>{spell_water_elem}</td></tr></tbody></table>
  }
}

object RoleWerewolf    extends RoleData(RoleEnum.WEREWOLF,    "人狼",   "#FF0000", RoomVictoryEnum.WEREWOLF_WIN, List(ActionWerewolf)) {
  override def role_intro = <img src="images/role_wolf.gif"/>
  override def role_pic   = <img src="images/rolepic_wolf.gif" />
  
  override def role_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = {
    val werewolf = UserEntry.findAll(By(UserEntry.room_id, room.id.is),
                                     NotBy(UserEntry.id, user.id.is),
                                     Like(UserEntry.role, RoleEnum.WEREWOLF.toString+"%"))
                                   
    val werewolf_str = werewolf.map(_.handle_name.is).mkString("　","　　","")
    var werewolf_tag : scala.xml.Elem = <tr><td></td></tr> 
    
    if ((room_day.day_no.is != 0) && (room_day.day_no.is %2 == 0)) {
      val system_message = SystemMessage.findAll(By(SystemMessage.roomday_id,  room_day.id.is),
                                                 By(SystemMessage.mtype,       MTypeEnum.VOTE_WEREWOLF.toString))
      //val actioner       = UserEntry.findAll(By(UserEntry.id, system_message(0).actioner_id.is))(0)                                            
      val actioner   = user_entrys.filter(_.id.is == system_message(0).actioner_id.is)(0)
      //val actionee       = UserEntry.findAll(By(UserEntry.id, system_message(0).actionee_id.is))(0)
      val actionee   = user_entrys.filter(_.id.is == system_message(0).actionee_id.is)(0)
      val actionee_role = RoleEnum.get_role(actionee.role.is.substring(0,1))
      val actionee_role_str =
        if (room_day.day_no.is == 2)
          actionee_role.role_pic
        else if ((actionee_role == RoleFox) || (actionee_role == RoleDemon) ||
            (actionee.subrole.is == "") || (actionee.subrole.is == SubroleEnum.FAKEAUGURER.toString))
          <img src="images/side_unknown.gif"/> //？？
        else
          actionee_role.role_pic
      val actionee_str =
        if (user.subrole.is == SubroleEnum.WISEWOLF.toString)
          <span><img src="images/parenthesis_left.gif"/><span>{actionee_role_str}</span>
          <img src="images/parenthesis_right.gif"/><img src="images/parenthesis_left.gif"/><span>{SubroleEnum.get_subrole(actionee.subrole.is).subrole_pic}</span><img src="images/parenthesis_right.gif"/></span>
        else if (actionee.current_role == RoleEnum.WOLFCUB) <img src="images/attacks_wolfcub.gif"/> else <span></span>

      if (system_message(0).message.is.indexOf(VoteFlagEnum.VICTIM.toString) != -1)
        werewolf_tag = <tr><td colspan="2"><img src="images/yesterday_wolf.gif"/><span>{actioner.handle_name.is}</span><img src="images/attacks_transfer.gif"/></td></tr>
      else
        werewolf_tag = <tr><td colspan="2"><img src="images/yesterday_wolf.gif"/><span>{actioner.handle_name.is}</span><img src="images/wolf_attacks.gif"/><span>{actionee.handle_name.is}</span><span>{actionee_str}</span></td></tr>
    }
    
    <table cellSpacing="0" cellPadding="0" border="1"><tbody>
      <tr><td><img src="images/role_wolf_partner.gif"/></td>
      <td>{werewolf_str}</td></tr>{werewolf_tag}</tbody></table>
  }
}

object RoleWolfcub     extends RoleData(RoleEnum.WOLFCUB,     "幼狼",   "#EE0000", RoomVictoryEnum.WEREWOLF_WIN, List(ActionWolfcub)) {
  //override def role_intro = <span>[角色]<br/>　　您所扮演的角色是幼狼。您無法咬人也不知道也和狼互不知道，當您被吊死時當晚狼群會狂暴。</span>
  override def role_intro = <img src="images/role_wolfcub.gif"/>
  override def role_pic   = <img src="images/rolepic_wolfcub.gif" />
  //override def role_enabled = false
  
  override def role_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = {
    if ((room_day.day_no.is != 0) && (room_day.day_no.is %2 == 0)) {
      val system_message = SystemMessage.findAll(By(SystemMessage.roomday_id,  room_day.id.is),
                                                 By(SystemMessage.mtype,       MTypeEnum.VOTE_WEREWOLF.toString))
      //val actioner       = UserEntry.findAll(By(UserEntry.id, system_message(0).actioner_id.is))(0)
      //val actioner   = user_entrys.filter(_.id.is == system_message(0).actioner_id.is)(0)
      //val actionee       = UserEntry.findAll(By(UserEntry.id, system_message(0).actionee_id.is))(0)
      val actionee   = user_entrys.filter(_.id.is == system_message(0).actionee_id.is)(0)

      <table cellSpacing="0" cellPadding="0" border="1"><tbody>
      <tr><td colspan="2"><img src="images/yesterday_wolf_attacks.gif" />{actionee.handle_name.is}</td></tr></tbody></table>
    } else
      <span></span>
  }
}

object RoleMadman      extends RoleData(RoleEnum.MADMAN,      "狂人",   "#DD0000", RoomVictoryEnum.WEREWOLF_WIN, List(ActionMadmanStun1, ActionMadmanStun3, ActionMadmanStun, ActionMadmanSuicide, ActionMadmanDuel, ActionNoAction)) {
  override def role_intro = <img src="images/role_mad.gif"/>
  override def role_pic   = <img src="images/rolepic_mad.gif" />
  
  override def role_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = {
    val werewolves_tag : NodeSeq =
      if  ((room.has_flag(RoomFlagEnum.MADMAN_KNOWLEDGE)) && (room_day.day_no.is >= 4) ) {
        val werewolves =
            user_entrys.filter(_.role.is == RoleEnum.WEREWOLF.toString).sort(_.user_no.is > _.user_no.is) ++
            user_entrys.filter(x=>(x.role.is.length == 2) && (x.role.is.substring(0,1) == RoleEnum.WEREWOLF.toString)).sort(_.user_no.is > _.user_no.is) ++
            user_entrys.filter(x=>(x.role.is.length == 3) && (x.role.is.substring(0,1) == RoleEnum.WEREWOLF.toString)).sort(_.user_no.is > _.user_no.is)
        val known_no   = Math.min(werewolves.length, (room_day.day_no.is + 2) / 6)
        val werewolves_str = 
          if (user.subrole.is == SubroleEnum.FOXBELIEVER.toString)
            <img src="images/role_mad_fox.gif"/>
          else
            <span>{werewolves.slice(0, known_no).map(_.handle_name.is).mkString("　", "　　","")}</span>

        Seq(<tr><td><img src="images/role_mad_partner.gif" /></td><td>{werewolves_str}</td></tr>)
     } else
        NodeSeq.Empty

    val action_point_tag : NodeSeq =
      if (room.has_flag(RoomFlagEnum.MADMAN_STUN))
        Seq(<tr><td><img src="images/role_mad_anger.gif" /></td><td>{user.action_point.is.toString}</td></tr>)
      else
        NodeSeq.Empty

    <table cellSpacing="0" cellPadding="0" border="1"><tbody>{werewolves_tag}{action_point_tag}</tbody></table>
  }
}

object RoleSorceror    extends RoleData(RoleEnum.SORCEROR,    "狂巫",   "#CC0000", RoomVictoryEnum.WEREWOLF_WIN, List(ActionSorcerorAugure, ActionSorcerorWhisper, ActionSorcerorConjure, ActionSorcerorShout, ActionSorcerorBelieve, ActionSorcerorSear, ActionSorcerorSummon, ActionNoAction)) {
  //override def role_intro = <span>[角色]<br/>　　您所扮演的角色是狂巫。您雖為人但屬於狼側，您有多樣的法術可以施行。</span>
  override def role_intro = <img src="images/role_sorceror.gif"/>
  override def role_pic   = <img src="images/rolepic_sorceror.gif" />

  override def role_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = {
    val action_point_str = user.action_point.is.toString + "/10"
    
    val  system_message = SystemMessage.findAll(By(SystemMessage.roomday_id,  room_day.id.is),
                                                By(SystemMessage.actioner_id, user.id.is),
                                                By(SystemMessage.mtype,       MTypeEnum.VOTE_SORCEROR_AUGURE.toString))
    
    val  result_augure : NodeSeq = if (system_message.length != 0) {
      val actionee   = user_entrys.filter(_.id.is == system_message(0).actionee_id.is)(0)
      val actionee_role = RoleEnum.get_role(actionee.role.is.substring(0,1))
      val actionee_role_str = 
        if (actionee_role.role_side == RoomVictoryEnum.VILLAGER_WIN)
          actionee_role.role_pic
        else
          <img src="images/role_result_inhuman.gif" />
      
      Seq(<tr><td><img src="images/role_mage_result.gif"/></td>
        <td><span>{actionee.handle_name.is}</span></td>
        <td><span>{actionee_role_str}</span></td>
        <td>{SubroleEnum.get_subrole(actionee.subrole.is).subrole_pic}</td>
      </tr>)
    } else NodeSeq.Empty

  
    <table cellSpacing="0" cellPadding="0" border="1"><tbody>
      <tr><td><img src="images/mana.gif" /></td><td>{action_point_str}</td></tr>
      { result_augure }</tbody></table>
  }
}

object RoleFox         extends RoleData(RoleEnum.FOX,         "妖狐",   "#CC0099", RoomVictoryEnum.FOX_WIN, List(ActionFox, ActionFox1, ActionFox2, ActionFoxDisguise, ActionNoAction2)) {
  def betrayer_mimic(user_entrys: List[UserEntry]) : Boolean = {
    val betrayers = user_entrys.filter(x => (x.current_role == RoleEnum.BETRAYER) ||
                                             (x.subrole.is == SubroleEnum.FOXBELIEVER.toString))
    if (betrayers.length == 0)
      return false

    val live_betrayers = betrayers.filter(_.live.is)
    if (live_betrayers.length != 0)
      return false

    return true
  }
  
  def godfat_mimic(user_entrys: List[UserEntry]) : Boolean = {
    val godfats = user_entrys.filter(x => (x.current_role == RoleEnum.GODFAT))
    if (godfats.length == 0)
      return false

    val live_godfats = godfats.filter(_.live.is)
    if (live_godfats.length != 0)
      return false

    return true
  }

  def combo(room : Room, room_day : RoomDay, user_entrys: List[UserEntry]) = {
    val lives = user_entrys.map(x=>((x.live.is == false) || x.test_foxside(room, room_day, user_entrys)))
    //lives.foreach(x=> println(x.toString))

    val combo =
        (if  (lives(0) && lives(1) && lives(2) && lives(3) && lives(4)) 1 else 0) +
        (if  (lives(5) && lives(6) && lives(7) && lives(8) && lives(9)) 1 else 0) +
        (if  (lives(10) && lives(11) && lives(12) && lives(13) && lives(14)) 1 else 0) +
        (if  (lives(15) && lives(16) && lives(17) && lives(18) && lives(19)) 1 else 0) +
        (if  (lives(20) && lives(21) && lives(22) && lives(23) && lives(24)) 1 else 0) +
        (if  (lives(0) && lives(5) && lives(10) && lives(15) && lives(20)) 1 else 0) +
        (if  (lives(1) && lives(6) && lives(11) && lives(16) && lives(21)) 1 else 0) +
        (if  (lives(2) && lives(7) && lives(12) && lives(17) && lives(22)) 1 else 0) +
        (if  (lives(3) && lives(8) && lives(13) && lives(18) && lives(23)) 1 else 0) +
        (if  (lives(4) && lives(9) && lives(14) && lives(19) && lives(24)) 1 else 0) +
        (if  (lives(0) && lives(6) && lives(12) && lives(18) && lives(24)) 1 else 0) +
        (if  (lives(4) && lives(8) && lives(12) && lives(16) && lives(20)) 1 else 0)
      val max_combo = Math.min(combo, 5)
      max_combo
  }

  def godfat_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = {
    val lives = user_entrys.map(_.live.is == false)
    //lives.foreach(x=> println(x.toString))

    val max_combo = combo(room, room_day, user_entrys)
    val combo_str = max_combo.toString + "/5"
    val combos    = Math.max(0, max_combo - 1)

    val user_index = user_entrys.indexOf(user)
    val user_x     = (user_index % 5)
    val user_y     = (user_index / 5)

    val user_up     = ((user_y+4)%5)*5 + user_x
    val user_down   = ((user_y+1)%5)*5 + user_x
    val user_y2     = ((user_y+2)%5)*5 + user_x
    val user_y3     = ((user_y+3)%5)*5 + user_x

    val user_left   = user_y*5 + ((user_x+4)%5)
    val user_right  = user_y*5 + ((user_x+1)%5)
    val user_x2     = user_y*5 + ((user_x+2)%5)
    val user_x3     = user_y*5 + ((user_x+3)%5)

    val user_up_left  = ((user_y+4)%5)*5 + ((user_x+4)%5)
    val user_up_right = ((user_y+4)%5)*5 + ((user_x+1)%5)
    val user_down_left = ((user_y+1)%5)*5 + ((user_x+4)%5)
    val user_down_right = ((user_y+1)%5)*5 + ((user_x+1)%5)

    val user_connected = List(user_entrys(user_up), user_entrys(user_down),
                              user_entrys(user_left), user_entrys(user_right),
                              user_entrys(user_up_left), user_entrys(user_up_right),
                              user_entrys(user_down_left), user_entrys(user_down_right),
                              user_entrys(user_x2), user_entrys(user_x3),
                              user_entrys(user_y2), user_entrys(user_y3)
                             ).sort(_.user_no.is > _.user_no.is)
    val user_known     = user_connected.slice(0, combos*3)
    val user_groups     = JinrouUtil.zipListBySize(3)(user_known)

    <table cellSpacing="0" cellPadding="0" border="1"><tbody>
    <tr><td><img src="images/role_godfat_connection.gif"/></td><td>{combo_str}</td></tr>
    {
      for (user_group <- user_groups ) yield
        <tr>
         { for (actionee <- user_group ) yield
         <td>{actionee.handle_name.is}</td>
         <td>{RoleEnum.get_role(actionee.current_role).role_pic}</td>
         <td>{SubroleEnum.get_subrole(actionee.subrole.is).subrole_pic}</td>
         } </tr>
    }</tbody></table>
  }

  override def role_intro = <img src="images/role_fox.gif"/>
  override def role_pic   = <img src="images/rolepic_fox.gif" />
  
  override def role_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = {
    if ((room_day.day_no.is != 0) && (room_day.day_no.is %2 == 0)) {
       val system_message = SystemMessage.findAll(By(SystemMessage.roomday_id,  room_day.id.is),
                                                 By(SystemMessage.mtype,       MTypeEnum.VOTE_WEREWOLF.toString))
       val system_message_hunter = SystemMessage.findAll(By(SystemMessage.roomday_id,  room_day.id.is),
                                                          By(SystemMessage.actionee_id, user.id.is),
                                                          By(SystemMessage.mtype,       MTypeEnum.VOTE_HUNTER.toString))

       val actionee   = user_entrys.filter(_.id.is == system_message(0).actionee_id.is)(0)

       val werewolf_tag =
         if (room.has_flag(RoomFlagEnum.FOX_OPTION2))
           Seq(<tr><td colspan="2"><img src="images/yesterday_wolf_attacks.gif"/><span>{actionee.handle_name.is}</span></td></tr>)
         else
           NodeSeq.Empty

       val fox_targeted_tag =
         if   ((actionee.id.is == user.id.is) &&
               (system_message_hunter.length == 0))
           Seq(<tr><td colspan="2"><img src="images/role_fox_targeted.gif"/></td></tr>)
         else
           NodeSeq.Empty

        val betrayer_tag =
          if ((room.has_flag(RoomFlagEnum.FOX_OPTION4)) &&
              (betrayer_mimic(user_entrys)))
            Seq(<tr><td><img src="images/role_cult_tickets.gif"/></td><td>{user.action_point.is.toString}</td></tr>)
          else
            NodeSeq.Empty

        val godfat_tag =
          if ((room.has_flag(RoomFlagEnum.FOX_OPTION4)) &&
              (godfat_mimic(user_entrys)))
            //RoleGodfat.role_ability(room, room_day, user, user_entrys) \\ "tr"
            godfat_ability(room, room_day, user, user_entrys) \\ "tr"
          else
            NodeSeq.Empty

        <table cellSpacing="0" cellPadding="0" border="1"><tbody>
          {werewolf_tag}{fox_targeted_tag}{betrayer_tag}{godfat_tag}</tbody></table>
    }
    else
      <span></span>
  }
}

object RoleBetrayer    extends RoleData(RoleEnum.BETRAYER,    "背德",   "#DD0088", RoomVictoryEnum.FOX_WIN, List(ActionBetrayerDisguise, ActionBetrayerChange, ActionBetrayerFog, ActionNoAction)) {
  override def role_intro = <img src="images/role_cult.gif"/>
  override def role_pic   = <img src="images/rolepic_cult.gif" />
  
  override def role_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = {
    //val fox = UserEntry.findAll(By(UserEntry.room_id, room.id.is),
    //                            Like(UserEntry.role, RoleEnum.FOX.toString+"%"))
    val fox = user_entrys.filter(_.current_role == RoleEnum.FOX)
                                   
    val fox_str = if (user.subrole.is == SubroleEnum.WOLFBELIEVER.toString)
        <img src="images/role_cult_wold.gif" /> //"背德狼信者不會顯示妖狐是誰"
      else
        <span>{fox.map(_.handle_name.is).mkString("　","　　","")}</span>
    var fox_tag : scala.xml.Elem = <span></span>
    val action_point_tag =
        if ((room.has_flag(RoomFlagEnum.BETRAYER_OPTION1)) ||
            (room.has_flag(RoomFlagEnum.BETRAYER_OPTION2)) ||
            (room.has_flag(RoomFlagEnum.BETRAYER_OPTION3)))
          <tr><td><img src="images/role_cult_tickets.gif"/></td><td>{user.action_point.is.toString}</td></tr>
        else
          <tr><td></td><td></td></tr>
    
    if ((room_day.day_no.is != 0) && (room_day.day_no.is %2 == 0)) {
      val system_message = SystemMessage.findAll(By(SystemMessage.roomday_id,  room_day.id.is),
                                                 By(SystemMessage.mtype,       MTypeEnum.VOTE_WEREWOLF.toString))
      //val actionee       = UserEntry.findAll(By(UserEntry.id, system_message(0).actionee_id.is))(0)
      val actionee   = user_entrys.filter(_.id.is == system_message(0).actionee_id.is)(0)
      val  system_message_hunter = SystemMessage.findAll(By(SystemMessage.roomday_id,  room_day.id.is),
                                                         By(SystemMessage.actionee_id, actionee.id.is),
                                                         By(SystemMessage.mtype,       MTypeEnum.VOTE_HUNTER.toString))
            
      //if ((actionee.role.is.substring(0,1) == RoleEnum.FOX.toString) && (system_message_hunter.length == 0))
      //  fox_tag = <tr><td colspan="2">昨天晚上 你的主人 似乎被襲擊了 </td></tr>
      if ((user.subrole.is != SubroleEnum.WOLFBELIEVER.toString) && (actionee.role.is.substring(0,1) == RoleEnum.FOX.toString)) {
        if (system_message(0).message.is == VoteFlagEnum.POWER.toString)
          fox_tag = <table cellSpacing="0" cellPadding="0" border="1"><tbody>
            <tr><td colspan="2"><img src="images/role_cult_master_dead.gif" /></td></tr></tbody></table>
        else if (system_message_hunter.length == 0)
          fox_tag = <table cellSpacing="0" cellPadding="0" border="1"><tbody>
            <tr><td colspan="2"><img src="images/role_cult_master_attacks.gif" /></td></tr></tbody></table>
      }
    }
    
    <table cellSpacing="0" cellPadding="0" border="1"><tbody>
      <tr><td><img src="images/role_cult_partner.gif"/></td>
      <td>{fox_str}</td></tr>
      {action_point_tag}
      <tr><td colspan="2">{fox_tag}</td></tr></tbody></table>
  }
}

object RoleGodfat      extends RoleData(RoleEnum.GODFAT,      "哥德法", "#BB00AA", RoomVictoryEnum.FOX_WIN, 
                                        List(ActionGodfatSpecial1, ActionGodfatSpecial2, ActionGodfatSpecial3, ActionGodfatSpecial4, ActionGodfatDeathGaze, ActionGodfatHellword, ActionGodfatColorSpray, ActionGodfatBlind2, ActionGodfatExchange,
                                             ActionGodfatNecromancer, ActionGodfatHunter, ActionGodfatHerbalist, ActionGodfatPoisoner, ActionGodfatScholar, ActionNoAction)) {
  //override def role_intro = <span>[角色]<br/>　　您所扮演的角色是哥德法。您不知道妖狐是誰，但是占卜師或狂巫占卜到您，就會被逆咒殺。</span>
  override def role_intro = <img src="images/role_godfat.gif"/>
  override def role_pic   = <img src="images/rolepic_godfat.gif" />

  def combo(room : Room, room_day : RoomDay, user_entrys: List[UserEntry]) = {
    val lives = user_entrys.map(x=>((x.live.is == false) || x.test_foxside(room, room_day, user_entrys) ||
                                    (x.user_flags.is.indexOf(UserEntryFlagEnum.GODFAT_TARGETED.toString)!= -1 )))
    //lives.foreach(x=> println(x.toString))

    val combo =
        (if  (lives(0) && lives(1) && lives(2) && lives(3) && lives(4)) 1 else 0) +
        (if  (lives(5) && lives(6) && lives(7) && lives(8) && lives(9)) 1 else 0) +
        (if  (lives(10) && lives(11) && lives(12) && lives(13) && lives(14)) 1 else 0) +
        (if  (lives(15) && lives(16) && lives(17) && lives(18) && lives(19)) 1 else 0) +
        (if  (lives(20) && lives(21) && lives(22) && lives(23) && lives(24)) 1 else 0) +
        (if  (lives(0) && lives(5) && lives(10) && lives(15) && lives(20)) 1 else 0) +
        (if  (lives(1) && lives(6) && lives(11) && lives(16) && lives(21)) 1 else 0) +
        (if  (lives(2) && lives(7) && lives(12) && lives(17) && lives(22)) 1 else 0) +
        (if  (lives(3) && lives(8) && lives(13) && lives(18) && lives(23)) 1 else 0) +
        (if  (lives(4) && lives(9) && lives(14) && lives(19) && lives(24)) 1 else 0) +
        (if  (lives(0) && lives(6) && lives(12) && lives(18) && lives(24)) 1 else 0) +
        (if  (lives(4) && lives(8) && lives(12) && lives(16) && lives(20)) 1 else 0)
      val max_combo = Math.min(combo, 4)
      max_combo
  }

  override def role_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = {
    if ((user_entrys.length == 25) &&
        (user.hasnt_flag(UserEntryFlagEnum.GODFAT_SPECIAL1)) &&
        (user.hasnt_flag(UserEntryFlagEnum.GODFAT_SPECIAL3)) &&
        (user.hasnt_flag(UserEntryFlagEnum.GODFAT_SPECIAL4))){
      val lives = user_entrys.map(_.live.is == false)
      //lives.foreach(x=> println(x.toString))

      val max_combo = combo(room, room_day, user_entrys)
      val combo_str = max_combo.toString + "/4"

      val user_index = user_entrys.indexOf(user)
      val user_x     = (user_index % 5)
      val user_y     = (user_index / 5)

      val user_up     = ((user_y+4)%5)*5 + user_x
      val user_down   = ((user_y+1)%5)*5 + user_x
      val user_y2     = ((user_y+2)%5)*5 + user_x
      val user_y3     = ((user_y+3)%5)*5 + user_x

      val user_left   = user_y*5 + ((user_x+4)%5)
      val user_right  = user_y*5 + ((user_x+1)%5)
      val user_x2     = user_y*5 + ((user_x+2)%5)
      val user_x3     = user_y*5 + ((user_x+3)%5)

      val user_up_left  = ((user_y+4)%5)*5 + ((user_x+4)%5)
      val user_up_right = ((user_y+4)%5)*5 + ((user_x+1)%5)
      val user_down_left = ((user_y+1)%5)*5 + ((user_x+4)%5)
      val user_down_right = ((user_y+1)%5)*5 + ((user_x+1)%5)

      val user_connected = List(user_entrys(user_up), user_entrys(user_down),
                                user_entrys(user_left), user_entrys(user_right),
                                user_entrys(user_up_left), user_entrys(user_up_right),
                                user_entrys(user_down_left), user_entrys(user_down_right),
                                user_entrys(user_x2), user_entrys(user_x3),
                                user_entrys(user_y2), user_entrys(user_y3)
                               ).sort(_.user_no.is > _.user_no.is)
      val user_known     = user_connected.slice(0, max_combo*3)
      val user_groups     = JinrouUtil.zipListBySize(3)(user_known)

      <table cellSpacing="0" cellPadding="0" border="1"><tbody>
      <tr><td><img src="images/role_godfat_connection.gif" /></td><td>{combo_str}</td></tr>
      {
        for (user_group <- user_groups ) yield
          <tr>
           { for (actionee <- user_group ) yield
           <td>{actionee.handle_name.is}</td>
           <td>{RoleEnum.get_role(actionee.current_role).role_pic}</td>
           <td>{SubroleEnum.get_subrole(actionee.subrole.is).subrole_pic}</td>
           } </tr>
      }</tbody></table>
    } else if (user.has_flag(UserEntryFlagEnum.GODFAT_SPECIAL4)) {
      val godfat_predicts = user_entrys.filter( x =>
                             (x.current_role == RoleEnum.NECROMANCER) ||
                             (x.current_role == RoleEnum.HUNTER) ||
                             (x.current_role == RoleEnum.HERBALIST) ||
                             (x.current_role == RoleEnum.ALCHEMIST) ||
                             (x.current_role == RoleEnum.POISONER) ||
                             (x.current_role == RoleEnum.SCHOLAR))
                         
      val godfat_predicts_tag : NodeSeq = if ((room_day.day_no.is >= 12) && (room_day.day_no.is % 2 == 0))
                                            Seq(<tr><td>靈獵藥毒學人數(存活/總數)：</td><td>{godfat_predicts.filter(_.live.is).length.toString}/{godfat_predicts.length.toString}</td></tr>)
                                          else
                                            NodeSeq.Empty

      <table cellSpacing="0" cellPadding="0" border="1"><tbody>
      <tr><td><img src="images/role_godfat_prophecy.gif" /></td><td>{ user.action_point.is.toString + "/4"}</td></tr>{godfat_predicts_tag}</tbody></table>
    } else
    <span></span>
  }
}

object RoleDemon       extends RoleData(RoleEnum.DEMON,       "惡魔",   "#666666", RoomVictoryEnum.DEMON_WIN, List(ActionDemonChaos, ActionDemonDominate, ActionDemonCurse, ActionDemonCurse2, ActionDemonVortex, ActionNoAction)) {
  //override def role_intro = <span>[角色]<br/>　　您所扮演的角色是惡魔，您必須完成惡魔儀式以毀滅整個村子。(當你被咬後你可以有一次機會詛咒自己，使隔日自己票數多 2)。</span>
  override def role_intro = <img src="images/role_demon.gif"/>
  override def role_pic   = <img src="images/rolepic_demon.gif" />
  
  override def role_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = {
    val action_point_tag : NodeSeq =
      Seq(<tr><td><img src="images/role_demon_soulload.gif" /></td><td>{(user.action_point.is + user_entrys.filter(x=> !x.live.is).length).toString}/35</td></tr>)
    var bited_tag    : NodeSeq = NodeSeq.Empty
    var targeted_tag : NodeSeq  = NodeSeq.Empty

    if ((room_day.day_no.is != 0) && (room_day.day_no.is %2 == 0)) {
      val  system_message = SystemMessage.findAll(By(SystemMessage.roomday_id,  room_day.id.is),
                                                  By(SystemMessage.mtype,       MTypeEnum.VOTE_WEREWOLF.toString),
                                                  By(SystemMessage.actionee_id, user.id.is))
      val  system_message_hunter = SystemMessage.findAll(By(SystemMessage.roomday_id,  room_day.id.is),
                                                         By(SystemMessage.actionee_id, user.id.is),
                                                         By(SystemMessage.mtype,       MTypeEnum.VOTE_HUNTER.toString))

      bited_tag =
        if (user.has_flag(UserEntryFlagEnum.BITED))
          Seq(<tr><td><img src="images/role_demon_ceremony_yes.gif" /></td></tr>)
        else
          Seq(<tr><td><img src="images/role_demon_ceremony_no.gif" /></td></tr>)

      targeted_tag =
        if   ((system_message.length != 0) &&
              (system_message_hunter.length == 0))
          Seq(<tr><td><img src="images/role_fox_targeted.gif"/></td>
        </tr>)
        else
          NodeSeq.Empty
    }
    <table cellSpacing="0" cellPadding="0" border="1"><tbody>
      {action_point_tag}{bited_tag}{targeted_tag}
    </tbody></table>
  }
}

object RoleFallenAngel  extends RoleData(RoleEnum.FALLEN_ANGEL,       "墮天使",   "#666666", RoomVictoryEnum.FALLENANGEL_WIN, List(ActionFallenAngelFallen, ActionNoAction)) {
  override def role_intro = <img src="images/role_fallenangel.gif"/>
  override def role_pic   = <img src="images/rolepic_fallenangel.gif" />

  override def role_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = {
    val bited_tag    : NodeSeq = if (user.has_flag(UserEntryFlagEnum.BITED))
                                   Seq(<tr><td><img src="images/role_angel_rise.gif" /></td></tr>)
                                 else
                                   Seq(<tr><td><img src="images/role_demon_ceremony_no.gif" /></td></tr>)

    <table cellSpacing="0" cellPadding="0" border="1"><tbody>
      {bited_tag}
    </tbody></table>
  }
}

object RolePenguin  extends RoleData(RoleEnum.PENGUIN,     "企鵝",   "#AADDDD", RoomVictoryEnum.PENGUIN_WIN, List(ActionPenguinIce, ActionPenguinChill, ActionNoAction)) {
  //override def role_intro = <span>[角色]<br/>　　您所扮演的角色是企鵝，您必須完成企鵝儀式以冰封整個村子。(註：冰凍需時4天，必須冰凍4人且未受干擾，無法冰凍惡魔，且惡魔死亡全冰凍解除)。</span>
  override def role_intro = <img src="images/role_penguin.gif"/>
  override def role_pic   = <img src="images/rolepic_penguin.gif" />

  override def role_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = {
    val penguin_counts = 1 + 2 * user_entrys.filter(x => (x.current_role == RoleEnum.PENGUIN) &&
      (x.role.is.toString.indexOf(RoleEnum.INHERITER.toString) == -1)).length

    val action_point_tag : NodeSeq =
      if ((user.role.is.length == 1) || (user.role.is(1).toString != RoleEnum.INHERITER.toString))
        Seq(<tr><td><img src="images/role_penguin_frozen.gif" /></td><td>{user.action_point.is}/{penguin_counts}</td></tr>)
      else
        Seq(<tr><td colspan="2"><img src="images/role_penguin_noinfo.gif" /></td></tr>)

    val iced = user_entrys.filter(x =>
      (x.has_flag(UserEntryFlagEnum.ICED_1)) ||
      (x.has_flag(UserEntryFlagEnum.ICED_2)) ||
      (x.has_flag(UserEntryFlagEnum.ICED_3)))

    val iced_tag : NodeSeq =
      Seq(<tr><td><img src="images/role_penguin_freezing.gif" /></td><td>{iced.map(_.handle_name.is).mkString("", "　","")}</td></tr>)

    <table cellSpacing="0" cellPadding="0" border="1"><tbody>
      {action_point_tag}{iced_tag}
    </tbody></table>
  }
}

object RolePontiff     extends RoleData(RoleEnum.PONTIFF,     "教主", "#EEAA55", RoomVictoryEnum.PONTIFF_WIN, List(ActionPontiff, ActionPontiffCommand, ActionPontiffAura, ActionNoAction)) {
  //override def role_intro = <span>[角色]<br/>　　您所扮演的角色是教主。您必須拉人入教使整個村子都在教派的勢力下。</span>
  override def role_intro = <img src="images/role_pontiff.gif"/>
  override def role_pic   = <img src="images/rolepic_pontiff.gif"/>
  
  override def role_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = {
    <span></span>
  }
}

object RoleInheriter   extends RoleData(RoleEnum.INHERITER,   "繼承者", "#AAAA00", RoomVictoryEnum.VILLAGER_WIN, List(ActionInheriter)) {
  //override def role_intro = <span>[角色]<br/>　　您所扮演的角色是繼承者，您將繼承一名被吊死的人的能力，取代它繼續進行遊戲。</span>
  override def role_intro = <img src="images/role_inheriter.gif"/>
  override def role_pic   = <img src="images/rolepic_inheriter.gif" />
}

object RoleShifter     extends RoleData(RoleEnum.SHIFTER,     "模仿師", "#FF7700", RoomVictoryEnum.VILLAGER_WIN, List(ActionShifter, ActionShifterDemon)) {
  //override def role_intro = <span>[角色]<br/>　　您所扮演的角色是模仿師，您可以於首日選擇一名角色，並和它有同樣的能力。</span>
  override def role_intro = <img src="images/role_shifter.gif"/>
  override def role_pic   = <img src="images/rolepic_shifter.gif" />
}

object RoleCardMaster    extends RoleData(RoleEnum.CARDMASTER,  "卡片師", "#FF7700", RoomVictoryEnum.VILLAGER_WIN, List(ActionCardFool, ActionCardMagician, ActionCardChariot, ActionCardHermit,
                                                                                                                          ActionCardStrength, ActionCardJustice, ActionCardTower, ActionCardSun, ActionNoAction)) {
  override def ctext = <span><font color="#FF7700">[卡</font><font color="#EC9010">片</font><font color="#DAA520">師]</font></span>
  override def role_enabled = false

  //override def role_intro = <span>[角色]<br/>　　您所扮演的角色是卡片師，每日可以翻一張牌，根據不同的牌可以擁有不同的能力。</span>
  override def role_intro = <img src="images/role_cardmaster.gif"/>
  override def role_pic   = <img src="images/rolepic_cardmaster.gif" />

  override def role_ability(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = {
    //var cards = List("")
    var cards : NodeSeq = Seq()

    val card_fool     = if (user.has_flag(UserEntryFlagEnum.CARD_FOOL))     cards ++= <img src="images/card_fool.gif" /> //cards = cards ::: List("愚者")
    val card_magician = if (user.has_flag(UserEntryFlagEnum.CARD_MAGICIAN)) cards ++= <img src="images/card_magician.gif" /> //cards = cards ::: List("魔術師")
    val card_chariot  = if (user.has_flag(UserEntryFlagEnum.CARD_CHARIOT))  cards ++= <img src="images/card_chariot.gif" /> //cards = cards ::: List("戰車")
    val card_hermit   = if (user.has_flag(UserEntryFlagEnum.CARD_HERMIT))   cards ++= <img src="images/card_hermit.gif" /> //cards = cards ::: List("隱者")
    val card_strength = if (user.has_flag(UserEntryFlagEnum.CARD_STRENGTH)) cards ++= <img src="images/card_strength.gif" /> //cards = cards ::: List("力")
    val card_justice  = if (user.has_flag(UserEntryFlagEnum.CARD_JUSTICE))  cards ++= <img src="images/card_justice.gif" /> //cards = cards ::: List("正義")
    val card_tower    = if (user.has_flag(UserEntryFlagEnum.CARD_TOWER))    cards ++= <img src="images/card_tower.gif" /> //cards = cards ::: List("塔")
    val card_sun      = if (user.has_flag(UserEntryFlagEnum.CARD_SUN))      cards ++= <img src="images/card_sun.gif" /> //cards = cards ::: List("太陽")

    val status =
    if ((room_day.day_no.is != 0) && (room_day.day_no.is %2 == 0)) {
      val  system_message = SystemMessage.findAll(By(SystemMessage.roomday_id,  room_day.id.is),
                                                  By(SystemMessage.actioner_id, user.id.is),
                                                Like(SystemMessage.mtype,      MTypeEnum.VOTE.toString+"%"))
      if (system_message.length != 0) {
        val system_message0 = system_message(0)
        if (system_message0.mtype.is == MTypeEnum.VOTE_AUGURER.toString) {
          RoleAugurer.augurer_ability(room, room_day, user, user_entrys, system_message0)
        } else if (system_message0.mtype.is == MTypeEnum.VOTE_HUNTER.toString) {
          RoleHunter.hunter_ability(room, room_day, user, user_entrys, system_message0)
        } else if ((room.has_flag(RoomFlagEnum.RUNNER_OPTION3)) &&
            (system_message0.mtype.is == MTypeEnum.VOTE_RUNNER.toString)) {
          RoleRunner.runner_ability(room, room_day, user, user_entrys, system_message0)
        } else if (system_message0.mtype.is == MTypeEnum.VOTE_SCHOLAR_EXAMINE.toString) {
          RoleScholar.scholar_examine(room, room_day, user, user_entrys, system_message0)
        } else 
         <span></span>
      } else
         <span></span>
    } else
      <span></span>

    val status_tr = status\"tbody"\"tr"

    <table cellSpacing="0" cellPadding="0" border="1"><tbody>
      <tr><td><img src="images/role_cardmaster_card_held.gif" /></td><td>{cards}</td></tr>{status_tr}</tbody></table>
    
  }
}
