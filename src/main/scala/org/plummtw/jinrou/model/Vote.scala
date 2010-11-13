package org.plummtw.jinrou.model 
 
import net.liftweb._ 
import mapper._ 
import http._ 
import SHtml._ 
import util._

class Vote extends LongKeyedMapper[Vote] with IdPK {
  def getSingleton = Vote // what's the "meta" object
  //def primaryKeyField = id

  // the primary key
  // object id            extends MappedLongIndex(this)
  object roomday_id    extends MappedLongForeignKey(this, RoomDay) 
  object actioner_id   extends MappedLongForeignKey(this, UserEntry)

  object actionee_id   extends MappedLongForeignKey(this, UserEntry)
  
  object vote_number   extends MappedInt(this)
  object vote_time     extends MappedInt(this)
  object mtype         extends MappedString(this, 3)
  object vote_flags    extends MappedString(this, 20)

  object auc_number   extends MappedInt(this) {
    override def defaultValue = 0
  }

  object created       extends MappedDateTime(this) {
    override def defaultValue = new java.util.Date()
  }
  
  object updated       extends MappedDateTime(this) with LifecycleCallbacks {
    override def beforeCreate = this(new java.util.Date())
    override def beforeUpdate = this(new java.util.Date())
  }
  
}

object Vote extends Vote with LongKeyedMetaMapper[Vote] {
  override def fieldOrder = List(id, roomday_id, actioner_id, actionee_id, vote_number, vote_time,
                                 mtype, vote_flags, created, updated)
}

/*
class Vote {
  //static  belongsTo = [room_day: RoomDay, voter:UserEntry, target:UserEntry]
  
  //RoomDay   room_day
  UserEntry voter
  UserEntry target
  
  Integer vote_number
  Integer vote_time
  String  situation
  String  vote_flag
  
  static constraints = {
    voter(nullable:false)
    target(nullable:true)
    
    vote_number(nullable:true,  min:1, max:999)
    vote_time(nullable:false,  min:1, max:99)
    
    situation(nullable:false, maxSize:2)
    vote_flag(nullable:true,  maxSize:1)
    
    created(nullable:true)
    updated(nullable:true)
  }
  
  Date created
  Date updated
  def beforeInsert = {
    created = new Date()
  }
  def beforeUpdate = {
    updated = new Date()
  }}
*/

/*
      	`room_no` smallint(5) unsigned NOT NULL default '0',
				`date` tinyint(3) unsigned default NULL,
				`uname` varchar(20) default NULL,
				`target_uname` varchar(20) default NULL,
				`vote_number` int(11) default NULL,
				`vote_times` int(11) default NULL,
				`situation` text,
				KEY `room_no` (`room_no`,`date`)
*/