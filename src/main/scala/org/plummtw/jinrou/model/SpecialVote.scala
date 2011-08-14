package org.plummtw.jinrou.model

import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

class SpecialVote extends LongKeyedMapper[SpecialVote] with IdPK {
  def getSingleton = SpecialVote // what's the "meta" object
  //def primaryKeyField = id

  // the primary key
  // object id            extends MappedLongIndex(this)
  object roomday_id    extends MappedLongForeignKey(this, RoomDay)
  object actioner_id   extends MappedLongForeignKey(this, UserEntry)

  object actionee_id   extends MappedLongForeignKey(this, UserEntry)

  //object vote_number   extends MappedInt(this)
  //object vote_time     extends MappedInt(this)
  object mtype         extends MappedString(this, 3)
  object vote_flags    extends MappedString(this, 5)


  object created       extends MappedDateTime(this) {
    override def defaultValue = new java.util.Date()
  }

  object updated       extends MappedDateTime(this) with LifecycleCallbacks {
    override def beforeCreate = this(new java.util.Date())
    override def beforeUpdate = this(new java.util.Date())
  }

}

object SpecialVote extends SpecialVote with LongKeyedMetaMapper[SpecialVote] {
  override def fieldOrder = List(id, roomday_id, actioner_id, actionee_id,
                                 mtype, vote_flags, created, updated)
}

