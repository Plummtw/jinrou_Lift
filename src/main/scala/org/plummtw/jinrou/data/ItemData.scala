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

trait ItemOption {
  def option_map : scala.collection.immutable.Map[String,String]
}

class ItemData (action: MTypeEnum.Value, str: String, name: String, targetable_boolean: Boolean, weight_no: Int)
    extends ActionData(action, str, name, targetable_boolean) {
  def weight = weight_no

  override def toString(): String = "【" + tag_string + "】"

  def item_intro(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) : NodeSeq = Seq() //""
  def item_pic : NodeSeq = NodeSeq.Empty

  // 產生 Item Tag
  def generate_action_tag(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry], vote_list:List[ItemVote]) : NodeSeq = {
    if (room.status.is == RoomStatusEnum.ENDED.toString)
      return Seq(<span></span>)
    else if (!user.live.is)
      return Seq(<span></span>)

    val is_voted = (vote_list.filter(_.actioner_id.is == user.id.is).length != 0)

    if (is_voted)
      return Seq(<span></span>)

    val user_item = ItemEnum.get_item(user.item_flags.is)
    if (user_item == ItemNoItem)
      return Seq(<span></span>)

    if (user_item.targetable)
      return Seq(<a href={"up_action.html?room_no=" + room.id.is.toString + "&command=" + user_item.command_name}>{this.toString}</a>)

    return Seq(<a href={"javascript:submit_action('" + user_item.command_name + "')"}>{this.toString}</a>)
  }
}

object ItemNoItem extends ItemData(MTypeEnum.ITEM_NO_ITEM, "無道具", "item_no_item", false, 0) {
}

object ItemUnluckyPurse extends ItemData(MTypeEnum.ITEM_UNLUCKY_PURSE, "不運錢包", "item_unlucky_purse", true, 8) {
  override def item_pic = Seq(<img src="icon/UP.gif" />)
  override def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
    val result = user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.id.is != user.id.is) && (x.live.is))
    if ((user.has_flag(UserEntryFlagEnum.RELIGION)) ||
        (user.subrole.is == SubroleEnum.SUBPONTIFF.toString))
      result.filter(x=>x.hasnt_flag(UserEntryFlagEnum.PONTIFF_AURA))
    else
      result
  }
}

object ItemBlessStaff extends ItemData(MTypeEnum.ITEM_BLESS_STAFF, "祝福之杖", "item_bless_staff", true, 7) {
  override def item_pic = Seq(<img src="icon/BS.gif" />)
  override def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
    user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.live.is))
  }
}

object ItemBlackFeather extends ItemData(MTypeEnum.ITEM_BLACK_FEATHER, "咒縛黑羽", "item_black_feather", true, 6) {
  override def item_pic = Seq(<img src="icon/BF.gif" />)
  override def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
    val result = user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.id.is != user.id.is) && (x.live.is))
    if ((user.has_flag(UserEntryFlagEnum.RELIGION)) ||
        (user.subrole.is == SubroleEnum.SUBPONTIFF.toString))
      result.filter(x=>x.hasnt_flag(UserEntryFlagEnum.PONTIFF_AURA))
    else
      result
  }
}

object ItemThiefSecret extends ItemData(MTypeEnum.ITEM_THIEF_SECRET, "盜賊極意", "item_thief_secret", true, 5) {
  override def item_pic = Seq(<img src="icon/TS.gif" />)
  override def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
    val result = user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.id.is != user.id.is))
    if ((user.has_flag(UserEntryFlagEnum.RELIGION)) ||
        (user.subrole.is == SubroleEnum.SUBPONTIFF.toString))
      result.filter(x=>x.hasnt_flag(UserEntryFlagEnum.PONTIFF_AURA))
    else
      result
  }
}

object ItemVentriloquist extends ItemData(MTypeEnum.ITEM_VENTRILOQUIST, "腹語娃娃！", "item_ventriloquist", false, 5) {
  override def item_pic = Seq(<img src="icon/VE.gif" />)
}

object ItemDMessageSeal extends ItemData(MTypeEnum.ITEM_DMESSAGE_SEAL, "封印遺書", "item_dmessage_seal", true, 4) {
  override def item_pic = Seq(<img src="icon/DS.gif" />)
  override def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
    val result = user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.id.is != user.id.is) && (x.live.is))
    if ((user.has_flag(UserEntryFlagEnum.RELIGION)) ||
        (user.subrole.is == SubroleEnum.SUBPONTIFF.toString))
      result.filter(x=>x.hasnt_flag(UserEntryFlagEnum.PONTIFF_AURA))
    else
      result
  }
}

object ItemMirrorShield extends ItemData(MTypeEnum.ITEM_MIRROR_SHIELD, "鏡盾捲軸！", "item_mirror_shield", false, 4) {
  override def item_pic = Seq(<img src="icon/MS.gif" />)
}

object ItemShamanCrown extends ItemData(MTypeEnum.ITEM_SHAMAN_CROWN, "薩滿冕冠", "item_shaman_crown", true, 3) {
  override def item_pic = Seq(<img src="icon/SC.gif" />)
  override def item_intro(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = {
    val  system_message = SystemMessage.findAll(By(SystemMessage.roomday_id,  room_day.id.is),
                                                By(SystemMessage.actioner_id, user.id.is),
                                                By(SystemMessage.mtype,       MTypeEnum.ITEM_SHAMAN_CROWN.toString))

    val result_augure : NodeSeq =
      if (system_message.length != 0) {
        val actionee   = user_entrys.filter(_.id.is == system_message(0).actionee_id.is)(0)
        val actionee_role = RoleEnum.get_role(actionee.role.is.substring(0,1))
        val actionee_role_node =
          if (actionee_role.role_side == RoomVictoryEnum.VILLAGER_WIN)
             actionee_role.role_pic //actionee_role.toString
          else
             <img src="images/role_result_inhuman.gif" /> // "非人側"

      // <img src="images/yes.gif" />
      //actionee.handle_name.is + "是" + actionee_role_str + "(" +
      ////  SubroleEnum.get_subrole(actionee.subrole.is).toString + ")"
        Seq(<span>{actionee.handle_name.is}</span>,
          <img src="images/yes.gif" />,
          <img src="images/parenthesis_left.gif" />,
          actionee_role_node,
          <img src="images/parenthesis_left.right.gif" />)
      } else NodeSeq.Empty // ""

    result_augure
  }
}

object ItemWeatherRod extends ItemData(MTypeEnum.ITEM_WEATHER_ROD, "天候棒！", "item_weather_rod", false, 3) with ItemOption {
  override def item_pic = Seq(<img src="icon/WR.gif" />)
  override def option_map = scala.collection.immutable.TreeMap[String,String](
     WeatherEnum.SUNNY.toString  -> "晴",
     WeatherEnum.CLOUDY.toString -> "陰",
     WeatherEnum.RAINY.toString  -> "雨",
     WeatherEnum.SNOWY.toString  -> "雪",
     WeatherEnum.MISTY.toString  -> "霧"
   )
}

object ItemDeathNote extends ItemData(MTypeEnum.ITEM_DEATH_NOTE, "死亡筆記", "item_death_note", true, 2) {
  override def item_pic = Seq(<img src="icon/DN.gif" />)
  override def targetable_users(room:Room, room_day:RoomDay, user:UserEntry, user_entrys:List[UserEntry]) : List[UserEntry] = {
    val result = user_entrys.filter(x=>(x.uname.is != "dummy_boy") && (x.id.is != user.id.is) && (x.live.is))
    if ((user.has_flag(UserEntryFlagEnum.RELIGION)) ||
        (user.subrole.is == SubroleEnum.SUBPONTIFF.toString))
      result.filter(x=>x.hasnt_flag(UserEntryFlagEnum.PONTIFF_AURA))
    else
      result
  }
}

object ItemPandoraBox extends ItemData(MTypeEnum.ITEM_PANDORA_BOX, "潘朵拉箱！", "item_pandora_box", false, 2) {
  override def item_pic = Seq(<img src="icon/PB.gif" />)
}

object ItemCubicArrow extends ItemData(MTypeEnum.ITEM_CUBIC_ARROW, "邱比特之箭", "item_cubic_arrow", true, 2) {
  override def item_pic = Seq(<img src="icon/CA.gif" />)
}

object ItemPopulationCensus extends ItemData(MTypeEnum.ITEM_POPULATION_CENSUS, "人口普查！", "item_population_census", false, 1) {
  override def item_pic = Seq(<img src="icon/PC.gif" />)
  override def item_intro(room:Room, room_day:RoomDay, user: UserEntry, user_entrys: List[UserEntry]) = {
    val  system_message = SystemMessage.findAll(By(SystemMessage.roomday_id,  room_day.id.is),
                                                By(SystemMessage.actioner_id, user.id.is),
                                                By(SystemMessage.mtype,       MTypeEnum.ITEM_POPULATION_CENSUS.toString))

    val result_census : NodeSeq =
      if (system_message.length != 0) {
        val live_users = user_entrys.filter(_.live.is)
        val role_list = RoleEnum.ROLE_MAP.keys.toList.filter(_ != RoleNone)
        //var role_text = new StringBuffer("")
        var role_seq : NodeSeq = Seq()
        role_list.foreach { role =>
          var role_number = live_users.filter(_.current_role == role).length

          if (role_number > 0) {
            //role_text.append("　")
            //role_text.append(RoleEnum.get_role(role).role_name)
            role_seq ++= RoleEnum.get_role(role).role_pic
            //role_text.append(" ")
            //role_text.append(role_number.toString)
            role_seq ++= <span>{role_number.toString}</span>
          }
        }
        //role_text.toString
        role_seq
    } else NodeSeq.Empty // ""

    result_census
  }
}