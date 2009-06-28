package org.plummtw.jinrou.model 
 
import net.liftweb._ 
import mapper._ 
import http._ 
import SHtml._ 
import util._

class Talk extends LongKeyedMapper[Talk] with IdPK {
  def getSingleton = Talk // what's the "meta" object
  //def primaryKeyField = id

  // the primary key
  // object id            extends MappedLongIndex(this)
  object roomday_id    extends MappedLongForeignKey(this, RoomDay) 
  object actioner_id   extends MappedLongForeignKey(this, UserEntry)
  object actionee_id   extends MappedLongForeignKey(this, UserEntry)
  object font_type     extends MappedString(this, 2)
  
  object message       extends MappedString(this, 250)
  object mtype         extends MappedString(this, 3)
  
  object created       extends MappedDateTime(this) {
    override def defaultValue = new java.util.Date()
  }

  object updated       extends MappedDateTime(this) with LifecycleCallbacks {
    override def beforeCreate = this(new java.util.Date())
    override def beforeUpdate = this(new java.util.Date())
  }
  
}

object Talk extends Talk with LongKeyedMetaMapper[Talk] {
  override def fieldOrder = List(id, roomday_id, actioner_id, font_type, message, mtype,
                                 created, updated)
}

/*


class Talk {
  //static belongsTo = [room_day:RoomDay, user_entry:UserEntry]
  UserEntry user_target
  
  RoomDay   room_day
  UserEntry user_entry
  
  String location
  String sentence
  String font_type
  //String spend_time
  
  static constraints = {
    location(nullable:false,  blank:false, maxSize:2)
    sentence(nullable:false,  blank:true, maxSize:200)
    font_type(nullable:false, blank:true, maxSize:2)
    user_entry(nullable:true)
    user_target(nullable:true)
    
    //session_id(length:34,unique:true)
    
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
  }
}
*/

/*
        `room_no` smallint(5) unsigned default NULL,
				`date` tinyint(3) unsigned default NULL,
				`location` varchar(24) default NULL,
				`uname` varchar(20) default NULL,
				`time` int(10) unsigned default NULL,
				`sentence` tinytext,
				`font_type` varchar(20) default NULL,
				`spend_time` int(11) default NULL,
				KEY `DEF` (`room_no`,`date`,`time`)
*/