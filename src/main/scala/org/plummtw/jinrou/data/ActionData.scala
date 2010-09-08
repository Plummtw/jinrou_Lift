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


class ActionData(action: MTypeEnum.Value, str: String, name: String, targetable_boolean: Boolean) {
  def action_enum       = action
  def tag_string        = str
  def command_name      = name 
  def targetable        = targetable_boolean
  
  def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= true

  def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
    user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.id.is != user.id.is) && (x.live.is))
  }
    
  override def toString(): String = "[" + tag_string + "]"
}

object ActionKick extends ActionData(MTypeEnum.VOTE_KICK, "踢人", "kick", true) 
object ActionStartGame extends ActionData(MTypeEnum.VOTE_STARTGAME, "開始遊戲！", "start_game", false)

object ActionVote extends ActionData(MTypeEnum.VOTE_HANG, "投票", "vote", true) {
  override def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
    val result = user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.id.is != user.id.is) && (x.live.is))
    if ((user.has_flag(UserEntryFlagEnum.RELIGION)) ||
        (user.subrole.is == SubroleEnum.SUBPONTIFF.toString))
      result.filter(x=>x.hasnt_flag(UserEntryFlagEnum.PONTIFF_AURA))
    else
      result
  }
}

object ActionBecomeMob extends ActionData(MTypeEnum.VOTE_BECOMEMOB, "暴民模式！", "becomemob", false)  {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean = {
    return ((room.has_flag(RoomFlagEnum.MOB_MODE)) && (user_entrys.length>=22) && (room_day.day_no.is == 11))
  }
}

object ActionAugure extends ActionData(MTypeEnum.VOTE_AUGURER, "占卜", "augure", true) 

object ActionVillagerDetect extends ActionData(MTypeEnum.VOTE_VILLAGER, "推理", "detect", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean = {
    return ((room.has_flag(RoomFlagEnum.VILLAGER_DETECT)) && (room_day.day_no.is == 7))
  }
}

object ActionGuard extends ActionData(MTypeEnum.VOTE_HUNTER, "護衛", "guard", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return (room_day.day_no.is != 1)
  }

  override def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
    if (room.has_flag(RoomFlagEnum.HUNTER_OPTION2))
      user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.live.is))
    else
      user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.id.is != user.id.is) && (x.live.is))
  }
}

object ActionAugHunterAugure extends ActionData(MTypeEnum.VOTE_AUGURER, "占卜", "aughunter_augure", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return (room_day.day_no.is %4 == 1)
  }
}

object ActionAugHunterGuard extends ActionData(MTypeEnum.VOTE_HUNTER, "護衛", "aughunter_guard", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return (room_day.day_no.is %4 == 3)
  }
}

object ActionRun extends ActionData(MTypeEnum.VOTE_RUNNER, "逃亡", "run", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return (room_day.day_no.is != 1)
  }
}


object ActionClericBless    extends ActionData(MTypeEnum.VOTE_CLERIC_BLESS, "祝福術", "cleric_bless", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return ((room.has_flag(RoomFlagEnum.CLERIC_OPTION1)) ||
            (user_entrys.filter{x=>(x.current_role == RoleEnum.DEMON)&&(x.has_flag(UserEntryFlagEnum.BITED))}.length != 0))
  }
}

object ActionClericSancture extends ActionData(MTypeEnum.VOTE_CLERIC_SANCTURE, "聖域術！", "cleric_sancture", false) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return (room_day.day_no.is != 1)
  }
}

object ActionHerbalistElixir extends ActionData(MTypeEnum.VOTE_HERBALIST_ELIXIR, "使用 治療藥", "herbalist_elixir", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return ((room_day.day_no.is != 1) && (user.hasnt_flag(UserEntryFlagEnum.ELIXIR_USED)))
  }
}

object ActionHerbalistPoison extends ActionData(MTypeEnum.VOTE_HERBALIST_POISON, "使用 毒藥",   "herbalist_poison", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return ((room_day.day_no.is != 1) && (user.hasnt_flag(UserEntryFlagEnum.POISON_USED)))
  }

  override def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
    val result = user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.id.is != user.id.is) && (x.live.is))
    if ((user.has_flag(UserEntryFlagEnum.RELIGION)) ||
        (user.subrole.is == SubroleEnum.SUBPONTIFF.toString))
      result.filter(x=>x.hasnt_flag(UserEntryFlagEnum.PONTIFF_AURA))
    else
      result
  }
}

object ActionHerbalistMix extends ActionData(MTypeEnum.VOTE_HERBALIST_MIX, "調製藥品！",   "herbalist_mix", false) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return ((user.has_flag(UserEntryFlagEnum.ELIXIR_USED)) &&
            (user.has_flag(UserEntryFlagEnum.POISON_USED)) &&
            (room.has_flag(RoomFlagEnum.HERBALIST_MIX)))
  }
}

object ActionScholarExamine extends ActionData(MTypeEnum.VOTE_SCHOLAR_EXAMINE, "個案調查",   "scholar_examine", true) {
  override def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
      user_entrys.filter(x=> (x.id.is != user.id.is))
  }

}

object ActionScholarAnalyze extends ActionData(MTypeEnum.VOTE_SCHOLAR_ANALYZE, "事件分析！",   "scholar_analyze", false) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return ((room_day.day_no.is != 1) && (user.hasnt_flag(UserEntryFlagEnum.ANALYZED)) &&
            (room.has_flag(RoomFlagEnum.SCHOLAR_OPTION3)))
  }
}

object ActionScholarReport extends ActionData(MTypeEnum.VOTE_SCHOLAR_REPORT, "現況報告！",   "scholar_report", false) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return ((room_day.day_no.is != 1) && (user.hasnt_flag(UserEntryFlagEnum.REPORTED)))
  }
}

object ActionDispell extends ActionData(MTypeEnum.VOTE_ARCHMAGE_DISPELL, "解除魔法",   "archmage_dispell", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return (user.action_point.is >= 3)
  }

  override def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
    val result = user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.id.is != user.id.is) && (x.live.is))
    if ((user.has_flag(UserEntryFlagEnum.RELIGION)) ||
        (user.subrole.is == SubroleEnum.SUBPONTIFF.toString))
      result.filter(x=>x.hasnt_flag(UserEntryFlagEnum.PONTIFF_AURA))
    else
      result
  }
}

object ActionSummon extends ActionData(MTypeEnum.VOTE_ARCHMAGE_SUMMON, "召喚水元素！",   "archmage_summon", false) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return ((user.action_point.is >= 3) && (user.has_flag(UserEntryFlagEnum.WATER_ELEM_USED)))
  }
}


object ActionWerewolf extends ActionData(MTypeEnum.VOTE_WEREWOLF, "咬人", "wolf_eat", true) {
  override def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
    val result =
      if ((room_day.day_no.is == 1) && (!room.has_flag(RoomFlagEnum.NO_DUMMY)))
        user_entrys.filter(x=>(x.uname.is == "dummy_boy"))
      else
        user_entrys.filter(x=>(x.current_role != RoleEnum.WEREWOLF) && (x.live.is))

    if ((user.has_flag(UserEntryFlagEnum.RELIGION)) ||
        (user.subrole.is == SubroleEnum.SUBPONTIFF.toString))
      result.filter(x=>x.hasnt_flag(UserEntryFlagEnum.PONTIFF_AURA))
    else
      result
  }
}

object ActionWolfcub extends ActionData(MTypeEnum.VOTE_WOLFCUB, "咬人", "wolfcub_eat", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return ((room_day.day_no.is == 15) ||
            ((room_day.day_no.is == 17) && (room.has_flag(RoomFlagEnum.WOLFCUB_OPTION1))))
  }

  override def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
    val result = user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.id.is != user.id.is) && (x.live.is))
    if ((user.has_flag(UserEntryFlagEnum.RELIGION)) ||
        (user.subrole.is == SubroleEnum.SUBPONTIFF.toString))
      result.filter(x=>x.hasnt_flag(UserEntryFlagEnum.PONTIFF_AURA))
    else
      result
  }
}

object ActionMadmanStun1 extends ActionData(MTypeEnum.VOTE_MADMAN_STUN1, "擊昏１", "madman_stun1", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return ((room.has_flag(RoomFlagEnum.MADMAN_STUN)) && (user.action_point.is >= 1))
  }

  override def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
    val result = user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.id.is != user.id.is) && (x.live.is))
    if ((user.has_flag(UserEntryFlagEnum.RELIGION)) ||
        (user.subrole.is == SubroleEnum.SUBPONTIFF.toString))
      result.filter(x=>x.hasnt_flag(UserEntryFlagEnum.PONTIFF_AURA))
    else
      result
  }
}

object ActionMadmanStun3 extends ActionData(MTypeEnum.VOTE_MADMAN_STUN3, "擊昏３", "madman_stun3", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return ((room.has_flag(RoomFlagEnum.MADMAN_STUN)) && (user.action_point.is >= 2))
  }

  override def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
    val result = user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.id.is != user.id.is) && (x.live.is))
    if ((user.has_flag(UserEntryFlagEnum.RELIGION)) ||
        (user.subrole.is == SubroleEnum.SUBPONTIFF.toString))
      result.filter(x=>x.hasnt_flag(UserEntryFlagEnum.PONTIFF_AURA))
    else
      result
  }

}

object ActionMadmanStun extends ActionData(MTypeEnum.VOTE_MADMAN_STUN, "擊忘", "madman_stun", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return ((room.has_flag(RoomFlagEnum.MADMAN_STUN)) && (user.action_point.is >= 2))
  }

  override def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
    val result = user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.id.is != user.id.is) && (x.live.is))
    if ((user.has_flag(UserEntryFlagEnum.RELIGION)) ||
        (user.subrole.is == SubroleEnum.SUBPONTIFF.toString))
      result.filter(x=>x.hasnt_flag(UserEntryFlagEnum.PONTIFF_AURA))
    else
      result
  }

}

object ActionMadmanSuicide extends ActionData(MTypeEnum.VOTE_MADMAN_SUICIDE, "自爆！",  "madman_suicide", false) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return (room.has_flag(RoomFlagEnum.MADMAN_SUICIDE))
  }
}


object ActionSorcerorAugure extends ActionData(MTypeEnum.VOTE_SORCEROR_AUGURE, "占卜術",  "sorceror_augure", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return (user.action_point.is >= 2)
  }
}
object ActionSorcerorWhisper extends ActionData(MTypeEnum.VOTE_SORCEROR_WHISPER, "密言術！", "sorceror_whisper", false) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return ((user.action_point.is >= 3) ||
            (room.has_flag(RoomFlagEnum.SORCEROR_WHISPER1) && (user.action_point.is >= 3)))
  }
}

object ActionSorcerorConjure extends ActionData(MTypeEnum.VOTE_SORCEROR_CONJURE, "咒殺術", "sorceror_conjure", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return (user.action_point.is >= 4)
  }
}

object ActionSorcerorShout extends ActionData(MTypeEnum.VOTE_SORCEROR_SHOUT, "鼓舞術！", "sorceror_shout", false) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return ((user.action_point.is >= 5) && (user.hasnt_flag(UserEntryFlagEnum.SHOUTED)))
  }
}

object ActionSorcerorBelieve extends ActionData(MTypeEnum.VOTE_SORCEROR_BELIEVE, "狼信化", "sorceror_believe", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return ((room.has_flag(RoomFlagEnum.SORCEROR_BELIEVE)) && (user.action_point.is >= 5))
  }
}


object ActionFox extends ActionData(MTypeEnum.VOTE_FOX, "指定背德", "fox_choose_betrayer", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean = {
    return ((room.has_flag(RoomFlagEnum.FOX_OPTION1)) &&
            (room.has_flag(RoomFlagEnum.ROLE_BETRAYER)) &&
            (user_entrys.length >= 20) && 
            (room_day.day_no.is == 1))
  }

  override def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
      user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.live.is) && (x.current_role == RoleEnum.VILLAGER))
  }
}

object ActionFox1 extends ActionData(MTypeEnum.VOTE_FOX1, "指定背德且結界", "fox_betrayer_barrier", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean = {
    return ((room.has_flag(RoomFlagEnum.FOX_OPTION1)) &&
            (room.has_flag(RoomFlagEnum.ROLE_BETRAYER)) &&
            (user_entrys.length >= 20) &&
            (room_day.day_no.is == 1) &&
            (room.has_flag(RoomFlagEnum.FOX_OPTION3)) &&
            (user.hasnt_flag(UserEntryFlagEnum.FOX_SPECIAL)))
  }

  override def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
      user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.live.is) && (x.current_role == RoleEnum.VILLAGER))
  }
}

object ActionFox2 extends ActionData(MTypeEnum.VOTE_FOX2, "結界！", "fox_barrier", false) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean = {
    return ((room.has_flag(RoomFlagEnum.FOX_OPTION3)) &&
            (user.hasnt_flag(UserEntryFlagEnum.FOX_SPECIAL)) &&
            !((room.has_flag(RoomFlagEnum.FOX_OPTION1)) &&
              (room.has_flag(RoomFlagEnum.ROLE_BETRAYER)) &&
              (user_entrys.length >= 20) &&
              (room_day.day_no.is == 1)))
  }
}

object ActionBetrayerDisguise extends ActionData(MTypeEnum.VOTE_BETRAYER_DISGUISE, "偽裝", "betrayer_disguise", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean = {
    if (room.room_flags.is.indexOf(RoomFlagEnum.BETRAYER_OPTION1.toString) == -1)
      return false

    if (room.room_flags.is.indexOf(RoomFlagEnum.CLERIC_OPTION2.toString) == -1)
      return (user.action_point.is >= 3)

    return (user.action_point.is >= 2)
  }

  override def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
    val result = user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.id.is != user.id.is) && (x.live.is))
    if ((user.has_flag(UserEntryFlagEnum.RELIGION)) ||
        (user.subrole.is == SubroleEnum.SUBPONTIFF.toString))
      result.filter(x=>x.hasnt_flag(UserEntryFlagEnum.PONTIFF_AURA))
    else
      result
  }
}

object ActionBetrayerChange extends ActionData(MTypeEnum.VOTE_BETRAYER_CHANGE, "變化", "betrayer_change", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean = {
    if (room.room_flags.is.indexOf(RoomFlagEnum.BETRAYER_OPTION2.toString) == -1)
      return false

    if (user.subrole.is != "")
      return false

    return (user.action_point.is >= 2)
  }
}

object ActionBetrayerFog extends ActionData(MTypeEnum.VOTE_BETRAYER_FOG, "粉紅迷霧！", "betrayer_fog", false) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean = {
    if (room.room_flags.is.indexOf(RoomFlagEnum.BETRAYER_OPTION3.toString) == -1)
      return false

    return (user.action_point.is >= 4)
  }
}

object ActionGodfatSpecial1 extends ActionData(MTypeEnum.VOTE_GODFAT_SPECIAL1, "咒術特化！", "godfat_special1", false) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean = {
    return ((room_day.day_no.is == 1) && (room.has_flag(RoomFlagEnum.GODFAT_SPECIAL1)))
  }
}

object ActionGodfatSpecial2 extends ActionData(MTypeEnum.VOTE_GODFAT_SPECIAL2, "方陣特化！", "godfat_special2", false) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean = {
    return ((room_day.day_no.is == 1) && (room.has_flag(RoomFlagEnum.GODFAT_SPECIAL2)))
  }
}

object ActionGodfatSpecial3 extends ActionData(MTypeEnum.VOTE_GODFAT_SPECIAL3, "秘術特化！", "godfat_special3", false) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean = {
    return ((room_day.day_no.is == 1) && (room.has_flag(RoomFlagEnum.GODFAT_SPECIAL3)) &&
            (user.subrole.is != SubroleEnum.WOLFBELIEVER.toString) &&
            (user.subrole.is != SubroleEnum.SUBPONTIFF.toString))
  }
}


object ActionGodfatDeathGaze extends ActionData(MTypeEnum.VOTE_GODFAT_DEATHGAZE, "絕望視線", "godfat_deathgaze", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean = {
    return  ((user.has_flag(UserEntryFlagEnum.GODFAT_SPECIAL1)) &&
             (user.hasnt_flag(UserEntryFlagEnum.GODFAT_SPECIAL_USED)) &&
             (targetable_users(room, room_day, user, user_entrys).length != 0))
  }

  override def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
    val result = user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.id.is != user.id.is) && (x.live.is) &&
                                       (x.has_flag(UserEntryFlagEnum.GODFAT_TARGETED)))
    if ((user.has_flag(UserEntryFlagEnum.RELIGION)) ||
        (user.subrole.is == SubroleEnum.SUBPONTIFF.toString))
      result.filter(x=>x.hasnt_flag(UserEntryFlagEnum.PONTIFF_AURA))
    else
      result
  }
}

object ActionGodfatColorSpray extends ActionData(MTypeEnum.VOTE_GODFAT_COLORSPRAY, "七彩噴射", "godfat_colorspray", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean = {
    return  ((user.has_flag(UserEntryFlagEnum.GODFAT_SPECIAL2)) &&
             (user.hasnt_flag(UserEntryFlagEnum.GODFAT_SPECIAL_USED)))
  }

  override def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
    val result = user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.id.is != user.id.is) && (x.live.is))
    
    if ((user.has_flag(UserEntryFlagEnum.RELIGION)) ||
        (user.subrole.is == SubroleEnum.SUBPONTIFF.toString))
      result.filter(x=>x.hasnt_flag(UserEntryFlagEnum.PONTIFF_AURA))
    else
      result
  }
}

object ActionGodfatBlind extends ActionData(MTypeEnum.VOTE_GODFAT_BLIND, "眩光", "godfat_blind", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean = {
    return  ((user.has_flag(UserEntryFlagEnum.GODFAT_SPECIAL2)) &&
             (user.hasnt_flag(UserEntryFlagEnum.GODFAT_BLIND_USED)))
  }

  override def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
    val result = user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.id.is != user.id.is) && (x.live.is))

    if ((user.has_flag(UserEntryFlagEnum.RELIGION)) ||
        (user.subrole.is == SubroleEnum.SUBPONTIFF.toString))
      result.filter(x=>x.hasnt_flag(UserEntryFlagEnum.PONTIFF_AURA))
    else
      result
  }
}

object ActionGodfatBlind2 extends ActionData(MTypeEnum.VOTE_GODFAT_BLIND2, "眩光！", "godfat_blind2", false) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean = {
    return  ((user.has_flag(UserEntryFlagEnum.GODFAT_SPECIAL2)) &&
             (user.hasnt_flag(UserEntryFlagEnum.GODFAT_BLIND_USED)))
  }
}

object ActionGodfatExchange extends ActionData(MTypeEnum.VOTE_GODFAT_EXCHANGE, "秘術換身", "godfat_exchange", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean = {
    return  ((user.has_flag(UserEntryFlagEnum.GODFAT_SPECIAL3)) &&
             (user.hasnt_flag(UserEntryFlagEnum.GODFAT_SPECIAL_USED)) &&
             (targetable_users(room, room_day, user, user_entrys).length != 0))
  }

  override def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
    return user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.id.is != user.id.is) && (x.live.is) &&
                                 (x.current_role == RoleEnum.FOX) && (x.has_flag(UserEntryFlagEnum.GODFAT_TARGETED)))
  }
}

object ActionDemonChaos extends ActionData(MTypeEnum.VOTE_DEMON_CHAOS, "混沌", "demon_chaos", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return ((user.hasnt_flag(UserEntryFlagEnum.BITED)) &&
            ((room.has_flag(RoomFlagEnum.NO_DUMMY)) || (room_day.day_no.is != 1)))
    //        (room_day.day_no.is %4 == 3))
  }
}

object ActionDemonDominate extends ActionData(MTypeEnum.VOTE_DEMON_DOMINATE, "支配", "demon_donimate", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return ((user.has_flag(UserEntryFlagEnum.BITED)))
  }

  override def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
    val result = user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.id.is != user.id.is) && (x.live.is))
    if ((user.has_flag(UserEntryFlagEnum.RELIGION)) ||
        (user.subrole.is == SubroleEnum.SUBPONTIFF.toString))
      result.filter(x=>x.hasnt_flag(UserEntryFlagEnum.PONTIFF_AURA))
    else
      result
  }
}

object ActionDemonCurse extends ActionData(MTypeEnum.VOTE_DEMON_CURSE, "詛咒！", "demon_curse", false) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return ((user.has_flag(UserEntryFlagEnum.BITED)) &&
            (room.room_flags.is.indexOf(RoomFlagEnum.DEMON_OPTION2.toString) == -1))
    //        (user.hasnt_flag(UserEntryFlagEnum.CURSE_USED)))
  }
}

object ActionDemonCurse2 extends ActionData(MTypeEnum.VOTE_DEMON_CURSE2, "詛咒", "demon_curse2", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return ((room.has_flag(RoomFlagEnum.DEMON_OPTION2)))
  }

  override def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
    val result = user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.live.is))
    if ((user.has_flag(UserEntryFlagEnum.RELIGION)) ||
        (user.subrole.is == SubroleEnum.SUBPONTIFF.toString))
      result.filter(x=>x.hasnt_flag(UserEntryFlagEnum.PONTIFF_AURA))
    else
      result
  }
}

object ActionDemonVortex extends ActionData(MTypeEnum.VOTE_DEMON_VORTEX, "斗轉星移", "demon_vortex", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return ((room.has_flag(RoomFlagEnum.DEMON_OPTION3)) &&
            (user.hasnt_flag(UserEntryFlagEnum.VORTEX_USED)) &&
            (user.role.is.length == 1))
  }
}

object ActionPenguinIce extends ActionData(MTypeEnum.VOTE_PENGUIN_ICE, "冰凍", "penguin_ice", true) {
}

object ActionPontiff extends ActionData(MTypeEnum.VOTE_PONTIFF, "拉人入教", "pontiff", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    if ((user.role.is.length != 1) && (room_day.day_no.is % 4 == 1))
      return false

    if ((room_day.day_no.is == 1) &&
        ((room.has_flag(RoomFlagEnum.PONTIFF_OPTION2)) ||
         (room.has_flag(RoomFlagEnum.SUBROLE_SUBPONTIFF))))
      return false

    return ((user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.current_role != RoleEnum.PONTIFF) && (x.live.is) &&
             (x.hasnt_flag(UserEntryFlagEnum.RELIGION))).length != 0) &&
             (user.hasnt_flag(UserEntryFlagEnum.PONTIFF_STUNNED)))
}
  override def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
    user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.current_role != RoleEnum.PONTIFF) && (x.live.is) &&
                          (x.hasnt_flag(UserEntryFlagEnum.RELIGION)))
  }
}

object ActionPontiffCommand extends ActionData(MTypeEnum.VOTE_PONTIFF_COMMAND, "指定投票", "pontiff_command", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    if (user.has_flag(UserEntryFlagEnum.PONTIFF_COMMAND_USED))
      return false

    return ((user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.current_role != RoleEnum.PONTIFF) && (x.live.is) &&
             (x.hasnt_flag(UserEntryFlagEnum.RELIGION))).length != 0) &&
             (user.hasnt_flag(UserEntryFlagEnum.PONTIFF_STUNNED)))
}
  override def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
    user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.current_role != RoleEnum.PONTIFF) && (x.live.is) &&
                          (x.hasnt_flag(UserEntryFlagEnum.RELIGION)))
  }
}

object ActionPontiffAura extends ActionData(MTypeEnum.VOTE_PONTIFF_AURA, "教主光環！", "pontiff_aura", false) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return ((user.hasnt_flag(UserEntryFlagEnum.PONTIFF_AURA)) &&
            (room_day.day_no.is >= 13) &&
            (user.hasnt_flag(UserEntryFlagEnum.PONTIFF_STUNNED)))
  }
}

object ActionInheriter extends ActionData(MTypeEnum.VOTE_INHERITER, "繼承", "inherit", true)


object ActionShifter extends ActionData(MTypeEnum.VOTE_SHIFTER, "模仿", "shift", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return (room_day.day_no.is == 1)
  }
}

object ActionShifterDemon extends ActionData(MTypeEnum.VOTE_SHIFTER2, "模仿惡魔！", "shift_demon", false) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return (room_day.day_no.is == 1)
  }
}

object ActionCardFool extends ActionData(MTypeEnum.VOTE_CARD_FOOL, "愚者", "card_fool", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return (user.has_flag(UserEntryFlagEnum.CARD_FOOL))
  }
}

object ActionCardMagician extends ActionData(MTypeEnum.VOTE_CARD_MAGICIAN, "魔術師", "card_magician", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return (user.has_flag(UserEntryFlagEnum.CARD_MAGICIAN))
  }
}


object ActionCardChariot extends ActionData(MTypeEnum.VOTE_CARD_CHARIOT, "戰車", "card_chariot", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return (user.has_flag(UserEntryFlagEnum.CARD_CHARIOT))
  }
}

object ActionCardHermit extends ActionData(MTypeEnum.VOTE_CARD_HERMIT, "隱者", "card_hermit", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return (user.has_flag(UserEntryFlagEnum.CARD_HERMIT))
  }

  override def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
    val result = user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.id.is != user.id.is) && (x.live.is))
    if ((user.has_flag(UserEntryFlagEnum.RELIGION)) ||
        (user.subrole.is == SubroleEnum.SUBPONTIFF.toString))
      result.filter(x=>x.hasnt_flag(UserEntryFlagEnum.PONTIFF_AURA))
    else
      result
  }
}

object ActionCardStrength extends ActionData(MTypeEnum.VOTE_CARD_STRENGTH, "力", "card_strength", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return (user.has_flag(UserEntryFlagEnum.CARD_STRENGTH))
  }

  override def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
    val result = user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.id.is != user.id.is) && (x.live.is))
    if ((user.has_flag(UserEntryFlagEnum.RELIGION)) ||
        (user.subrole.is == SubroleEnum.SUBPONTIFF.toString))
      result.filter(x=>x.hasnt_flag(UserEntryFlagEnum.PONTIFF_AURA))
    else
      result
  }
}

object ActionCardJustice extends ActionData(MTypeEnum.VOTE_CARD_JUSTICE, "正義",   "card_justice", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return (user.has_flag(UserEntryFlagEnum.CARD_JUSTICE))
  }
}

object ActionCardTower extends ActionData(MTypeEnum.VOTE_CARD_TOWER, "塔", "card_tower", true) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return (user.has_flag(UserEntryFlagEnum.CARD_TOWER))
  }
}

object ActionCardSun extends ActionData(MTypeEnum.VOTE_CARD_SUN, "太陽！", "card_sun", false) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return (user.has_flag(UserEntryFlagEnum.CARD_SUN))
  }
}

object ActionNoAction extends ActionData(MTypeEnum.VOTE_NO_ACTION, "不行動！", "no_action", false)

object ActionNoAction2 extends ActionData(MTypeEnum.VOTE_NO_ACTION, "不行動！", "no_action", false) {
  override def enabled(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : Boolean= {
    return (!((room.has_flag(RoomFlagEnum.FOX_OPTION1)) &&
              (room.has_flag(RoomFlagEnum.ROLE_BETRAYER)) &&
              (user_entrys.length >= 20) &&
              (room_day.day_no.is == 1)))
  }
}
