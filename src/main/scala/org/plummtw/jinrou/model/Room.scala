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

class Room extends LongKeyedMapper[Room] with IdPK {
  def getSingleton = Room // what's the "meta" object
  //def primaryKeyField = id

  // the primary key
  // object id            extends MappedLongIndex(this)
  
  object room_name     extends MappedString(this, 20) {
    override def validations = validPriority _ :: super.validations 
 
    def validPriority(in: String): List[FieldError] = 
      if (in.length <= 0)       List(FieldError(this, <b>村子名稱不得為空白</b>))
      else if (in.length > 20)  List(FieldError(this, <b>村子名稱過長＞２０</b>))
      else Nil
  }

  object room_comment  extends MappedString(this, 60) {
    override def validations = validPriority _ :: super.validations 
 
    def validPriority(in: String): List[FieldError] = 
      if (in.length <= 0)       List(FieldError(this, <b>村子說明不得為空白</b>))
      else if (in.length > 60)  List(FieldError(this, <b>村子說明過長＞６０</b>))
      else Nil
  }
  object day_minutes   extends MappedInt(this) {
    override def validations = validPriority _ :: super.validations 
 
    def validPriority(in: Int): List[FieldError] = 
      if (in <= 0)       List(FieldError(this, <b>日時間過短</b>))
      else if (in > 99)  List(FieldError(this, <b>日時間過長＞９９</b>))
      else Nil
  }
  object night_minutes extends MappedInt(this) {
    override def validations = validPriority _ :: super.validations 
 
    def validPriority(in: Int): List[FieldError] = 
      if (in <= 0)       List(FieldError(this, <b>夜時間過短</b>))
      else if (in > 99)  List(FieldError(this, <b>夜時間過長＞９９</b>))
      else Nil
  }
  object max_user      extends MappedInt(this) {
    override def validations = validPriority _ :: super.validations 
 
    def validPriority(in: Int): List[FieldError] = 
      if (in < 8)        List(FieldError(this, <b>最大玩家數過少＜８</b>))
      else if (in > 25)  List(FieldError(this, <b>最大玩家數過多＜２５</b>))
      else Nil
  }
  object room_flags    extends MappedString(this,500) {
    override def validations = validPriority _ :: super.validations 
 
    def validPriority(in: String): List[FieldError] = 
      if (in.length > 500)  List(FieldError(this, <b>選項字串過長＞５００</b>))
      else Nil
  }
  //object room_options  extends MappedString(this,20)
  object status        extends MappedString(this,1)
  object victory       extends MappedString(this,1)
  
  object created       extends MappedDateTime(this) {
    override def defaultValue = new java.util.Date()
  }

  object updated       extends MappedDateTime(this) with LifecycleCallbacks {
    override def beforeCreate = this(new java.util.Date())
    override def beforeUpdate = this(new java.util.Date())
  }

  def option_text : String = {
    var result  = new java.lang.StringBuffer()

    val roomflags_enums : Array[String] = room_flags.is.split(',')

    for (item <- roomflags_enums) {
      val roomflags_enum = RoomFlagEnum.valueOf(item)
      if (!roomflags_enum.isEmpty) {
        result.append(RoomFlagEnum.flag_name(roomflags_enum.getOrElse(null)).getOrElse(""))
      }
    }
    return result.toString()
  }

  def has_flag(flag : RoomFlagEnum.RoomFlagEnum) : Boolean = {
    return (room_flags.is.indexOf(flag.toString) != -1)
  }
}

object Room extends Room with LongKeyedMetaMapper[Room] {
  override def fieldOrder = List(id, room_name, room_comment, day_minutes, night_minutes,
                                 max_user, room_flags, status, victory, created, updated)
}

