package org.plummtw.jinrou.model 
 
import scala.xml._
import net.liftweb._
import net.liftweb.mapper._
import http._
import js._
import util._
import S._
import SHtml._
import Helpers._

import org.plummtw.jinrou.enum._
import org.plummtw.jinrou.util._
import org.plummtw.jinrou.data._

class UserEntry extends LongKeyedMapper[UserEntry] with IdPK {
  def getSingleton = UserEntry // what's the "meta" object
  //def primaryKeyField = id

  // the primary key
  //object id            extends MappedLongIndex(this)
  object room_id       extends MappedLongForeignKey(this, Room)
  object user_icon_id  extends MappedLongForeignKey(this, UserIcon) {
    override def validations = validPriority _ :: super.validations 
 
    def validPriority(in: Long): List[FieldError] = 
      if (in == 0)  List(FieldError(this, <b>尚未選擇圖像</b>)) 
      else Nil
  }
  
  object user_no       extends MappedInt(this)
  object uname         extends MappedString(this,20) {
    override def validations = validPriority _ :: super.validations 
 
    def validPriority(in: String): List[FieldError] = 
      if (in.length() <= 0)       List(FieldError(this, <b>帳號名稱過短</b>))
      else if (in.length() > 20)  List(FieldError(this, <b>帳號名稱過長＞２０</b>))
      else Nil
  }
  object handle_name   extends MappedString(this,20) {
    override def validations = validPriority _ :: super.validations 
 
    def validPriority(in: String): List[FieldError] = 
      if (in.length() <= 0)       List(FieldError(this, <b>暱稱過短</b>))
      else if (in.length() > 20)  List(FieldError(this, <b>暱稱過長＞２０</b>))
      else Nil
  }
  object trip   extends MappedString(this,20) {
    override def validations = validPriority _ :: super.validations 
 
    def validPriority(in: String): List[FieldError] = 
      if (in.length() > 20)  List(FieldError(this, <b>ｔｒｉｐ過長＞２０</b>))
      else Nil
  }
  object sex           extends MappedString(this,1)  {
    override def validations = validPriority _ :: super.validations 
 
    def validPriority(in: String): List[FieldError] = 
      if ((in != "M")&&(in != "F")) List(FieldError(this, <b>性別錯誤</b>))
      else Nil
  }
  object password      extends MappedString(this,20) {
    override def validations = validPriority _ :: super.validations 
 
    def validPriority(in: String): List[FieldError] = 
      if (in.length() < 6)        List(FieldError(this, <b>密碼過短＜６</b>))
      else if (in.length() > 20)  List(FieldError(this, <b>密碼過長＞２０</b>))
      else Nil
  }
  
  object role          extends MappedString(this,3)
  object subrole       extends MappedString(this,1) {
    override def defaultValue = ""
  }
  
  object action_point  extends MappedInt(this) {
    override def defaultValue = 0
  }
  
  object live          extends MappedBoolean(this) {
    override def defaultValue = true
  }
  
  object last_words    extends MappedString(this, 250) {
    override def validations = validPriority _ :: super.validations 
 
    def validPriority(in: String): List[FieldError] = 
      if (in.length() > 250)  List(FieldError(this, <b>遺言過長＞２５０</b>))
      else Nil
  }
  object ip_address0     extends MappedString(this, 20) with LifecycleCallbacks {
    override def beforeCreate = {
      this(ip_address.is)
    }
  }
  object ip_address     extends MappedString(this, 20)
  object ip_address_md5 extends MappedString(this, 34) with LifecycleCallbacks {
    override def beforeCreate = {
      this(JinrouUtil.generateSHA1(ip_address.is))
    }
  }

  object last_day_no   extends MappedInt(this)
  object last_talk     extends MappedString(this, 250)
  
  object user_flags    extends MappedString(this, 20)
  

  object created       extends MappedDateTime(this) {
    override def defaultValue = new java.util.Date()
  }

  object updated       extends MappedDateTime(this) with LifecycleCallbacks {
    override def beforeCreate = this(new java.util.Date())
    override def beforeUpdate = this(new java.util.Date())
  }

  
  def check_voted(room_day: RoomDay, room: Room) : String = ""
  /*  
  def check_voted(room_day: RoomDay, room: Room) : String = {
    var vote_list : List[Vote] = null
    if (room_day.day_no.is == 0)
      return ""
    else if (room_day.day_no.is %2 == 0) {  // 白天
      vote_list = Vote.findAll(By(Vote.roomday_id, room_day.id.is), 
                               By(Vote.vote_time, room_day.vote_time.is))
      if (live.is) {
        if (vote_list.length == 0) 
           return "你還沒投票"
         else
           return "你已經投票過了"
      } else
        return "你無法投票"
    } 
    else {  // 晚上
      var role_actable : List[RoleEnum.Value] = null
      if (room_day.day_no.is == 1) { // 第一天晚上
        role_actable = List(RoleEnum.AUGURER, RoleEnum.INHERITER, 
                            RoleEnum.SHIFTER, RoleEnum.WEREWOLF)
        if (room.has_flag(RoomFlagEnum.FOX_OPTION))
          role_actable ::= RoleEnum.FOX // 妖狐自選背德
      } else {
        role_actable = List(RoleEnum.AUGURER, RoleEnum.CLERIC, 
                            RoleEnum.HUNTER,
                            RoleEnum.INHERITER, RoleEnum.RUNNER,
                            RoleEnum.SHIFTER, RoleEnum.WEREWOLF)
          if ((room.has_flag(RoomFlagEnum.VILLAGER_DETECT)) &&
            (room_day.day_no.is == 7)) // 村民推理
          role_actable ::= RoleEnum.VILLAGER
      }
      val role_str : String = role.is
      if ((live.is) && role_actable.map(_.toString).contains(role_str.substring(0,1))) {
         if (role_str.substring(0,1) == RoleEnum.WEREWOLF.toString) { // 人狼群體行動
           vote_list = Vote.findAll(By(Vote.roomday_id, room_day.id.is), 
                                    By(Vote.vote_time, room_day.vote_time.is),
                                    By(Vote.mtype, MTypeEnum.VOTE_WEREWOLF.toString))
         } else {
           vote_list = Vote.findAll(By(Vote.roomday_id, room_day.id.is), 
                                    By(Vote.vote_time, room_day.vote_time.is),
                                    By(Vote.actioner_id, id.is))
         }
         if (vote_list.length == 0) 
           return "你還沒行動"
         else
           return "你已經行動過了"
      }
      else
        return "你無法行動"
    } 
    
    return "" 
  } */
  
  def current_role() : RoleEnum.Value = {
    RoleEnum.valueOf(role.is.substring(0,1)).getOrElse(RoleEnum.NONE)
  }

  def get_user_icon() : UserIcon = {
    val user_icon_list = UserIcon.findAll(By(UserIcon.id, user_icon_id.is))
    val user_icon : UserIcon = if (user_icon_list.length == 0) UserIcon.findAll(By(UserIcon.id, 1))(0) else user_icon_list(0)

    return user_icon
  }
  
  def get_role_field()  = {
    var result = Seq(RoleEnum.get_role(role.is.substring(0,1)).ctext)
    if (role.is.length > 1)
      result = result ++ Seq(RoleEnum.get_role(role.is.substring(1,2)).simple_ctext)
    if (role.is.length > 2)
      result = result ++ Seq(RoleEnum.get_role(role.is.substring(2,3)).simple_ctext)
    result
  }
  
  def get_subrole_field() = {
    if (subrole.is != "") {
      val subrole_data = SubroleEnum.get_subrole(subrole.is)
      if (subrole_data != SubroleNone)
        Seq(subrole_data.ctext, <br/>)
      else
        NodeSeq.Empty
    } else
        NodeSeq.Empty
  }

  def get_action_list(room:Room, room_day:RoomDay, user_entrys:List[UserEntry], vote_list:List[Vote]) = {
    val user_role       = if (room_day.day_no.is == 0) RoleEnum.NONE.toString
                          else role.is.substring(0,1)
    val user_role_enum  = RoleEnum.valueOf(user_role).getOrElse(RoleEnum.NONE)
    val user_role_data  = RoleEnum.get_role(user_role_enum)

    if (uname.is == "dummy_boy")
      List()
    else
      user_role_data.check_action_list(room, room_day, this, user_entrys, vote_list)
  }

  def get_action_tag(room:Room, room_day:RoomDay, user_entrys:List[UserEntry], vote_list:List[Vote]) = {
    val user_role       = if (room_day.day_no.is == 0) RoleEnum.NONE.toString
                          else role.is.substring(0,1)
    val user_role_enum  = RoleEnum.valueOf(user_role).getOrElse(RoleEnum.NONE)
    val user_role_data  = RoleEnum.get_role(user_role_enum)

    if (uname.is == "dummy_boy")
       <span>替身君不能行動</span>
    else
      user_role_data.generate_action_tag(room, room_day, this, user_entrys, vote_list)
  }

  // 測試是否為失憶者
  def test_memoryloss(room:Room, room_day:RoomDay, user_entrys: List[UserEntry]) : Boolean = {
    if (((this.subrole.is.indexOf(SubroleEnum.MEMORYLOSS4.toString) != -1 ) &&
         (room_day.day_no.is <= 7)) ||
        ((this.subrole.is.indexOf(SubroleEnum.MEMORYLOSS6.toString) != -1 ) &&
         (room_day.day_no.is <= 11)) ||
        ((this.subrole.is.indexOf(SubroleEnum.MEMORYLOSS8.toString) != -1 ) &&
         (room_day.day_no.is <= 15)) ||
        ((this.subrole.is.indexOf(SubroleEnum.ALPHAWOLF.toString) != -1 ) &&
         (room_day.day_no.is <= 11)) ||
        (this.has_flag(UserEntryFlagEnum.STUNNED_1) ) ||
        (this.has_flag(UserEntryFlagEnum.STUNNED_2) ) ||
        (this.has_flag(UserEntryFlagEnum.STUNNED_3) )) {

      // 這邊加入特殊處理，非失憶者的人狼全滅時，失憶者是狼的話會提早清醒
      if ((this.current_role == RoleEnum.WEREWOLF) &&
          (user_entrys.filter(x=>(x.current_role == RoleEnum.WEREWOLF) &&
                                 (x.subrole.is.indexOf(SubroleEnum.MEMORYLOSS4.toString) == -1) &&
                                 (x.subrole.is.indexOf(SubroleEnum.MEMORYLOSS6.toString) == -1) &&
                                 (x.subrole.is.indexOf(SubroleEnum.MEMORYLOSS8.toString) == -1) &&
                                 (x.subrole.is.indexOf(SubroleEnum.ALPHAWOLF.toString) == -1) &&
                                 (x.hasnt_flag(UserEntryFlagEnum.STUNNED_1) ) &&
                                 (x.hasnt_flag(UserEntryFlagEnum.STUNNED_2) ) &&
                                 (x.hasnt_flag(UserEntryFlagEnum.STUNNED_3) ) &&
                                 (x.live.is)).length == 0))
        false
      else
        true
    } else false
  }

  // 測試是否為冒牌者
  def test_fake(room_day:RoomDay) : Boolean = {
    return ((subrole.is == SubroleEnum.FAKEAUGURER.toString) &&
            (room_day.day_no.is <= 7))
  }

  def test_foxside(room : Room, room_day : RoomDay, user_entrys : List[UserEntry]) : Boolean = {
    return ((!test_memoryloss(room, room_day, user_entrys)) && (!(test_fake(room_day)))
             && (((role.is.substring(0,1) == RoleEnum.FOX.toString) || (role.is.substring(0,1) == RoleEnum.BETRAYER.toString) || (role.is.substring(0,1) == RoleEnum.GODFAT.toString) ||
                 (subrole.is == SubroleEnum.FOXBELIEVER.toString))
             && (subrole.is != SubroleEnum.WOLFBELIEVER.toString))) 
  }

  def has_flag(flag : UserEntryFlagEnum.UserEntryFlagEnum) : Boolean = {
    return (user_flags.is.indexOf(flag.toString) != -1)
  }

  def hasnt_flag(flag : UserEntryFlagEnum.UserEntryFlagEnum) : Boolean = {
    return (user_flags.is.indexOf(flag.toString) == -1)
  }
}

object UserEntry extends UserEntry with LongKeyedMetaMapper[UserEntry] {
  override def fieldOrder = List(id, room_id, user_no, uname, handle_name, trip, sex, password, user_icon_id,
                                 role, subrole, action_point, live, last_words, ip_address0, ip_address, ip_address_md5,
                                 last_day_no, last_talk, user_flags, created, updated)
}

object WaterElemental extends UserEntry
