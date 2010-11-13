package org.plummtw.jinrou.model 

import org.plummtw.jinrou.enum._

import net.liftweb._ 
import mapper._ 
import http._ 
import SHtml._ 
import util._

class RoomDay extends LongKeyedMapper[RoomDay] with IdPK {
  def getSingleton = RoomDay // what's the "meta" object
  //def primaryKeyField = id

  // the primary key
  // object id            extends MappedLongIndex(this)
  object room_id       extends MappedLongForeignKey(this, Room) 
  
  object day_no        extends MappedInt(this)
  object vote_time     extends MappedInt(this)
  object deadline      extends MappedDateTime(this)

  object weather       extends MappedString(this, 1)

  object item           extends MappedString(this, 3) {
    override def defaultValue = ItemEnum.ITEM_NO_ITEM.toString
  }
  
  object created       extends MappedDateTime(this) {
    override def defaultValue = new java.util.Date()
  }

  object updated       extends MappedDateTime(this) with LifecycleCallbacks {
    override def beforeCreate = this(new java.util.Date())
    override def beforeUpdate = this(new java.util.Date())
  }
  
}

object RoomDay extends RoomDay with LongKeyedMetaMapper[RoomDay] {
  override def fieldOrder = List(id, room_id, day_no, vote_time, deadline, created, updated, weather, item)
}

/*
class RoomDay {
  //static   belongsTo = [room:Room]
  static   hasMany   = [talks:Talk, system_messages:SystemMessage, votes:Vote]
  static mapping = {
		       talks sort:'id desc'
	}

  
  //Room     room
  //List     talks
  //List     system_messages
  //List     votes
  
  Integer  day_no
  Integer  vote_time
  Date     deadline  

  static constraints = {
    day_no(nullable:false, min:0, max:999)
    vote_time(nullable:false, min:1, max:10)
    
    created(nullable:true)
    updated(nullable:true)
    
    deadline(nullable:true)
  }
  
  Date created
  Date updated
  def beforeInsert = {
    created = new Date()
  }
  def beforeUpdate = {
    updated = new Date()
  }
  
  public getAllSystemMessageByType(type) {
    return system_messages.findAll{it.type == type}
  }
  
  public getSystemMessageByType(type) {
    return system_messages.find{it.type == type}
  }
  
  public getSystemMessageByUser(user, type) {
    return system_messages.find{(it.actioner.id == user.id) && (it.type == type)}
  }
  
  public getVoteString(vote_time) {
    return RoomUtil.generateVoteString(this, vote_time)
  }
  
  public getLastwordsString() {
    return RoomUtil.generateLastwordsString(system_messages)
  }
  
  public getDeadString() {
    RoomUtil.generateDeadString(system_messages)
  }

}

*/