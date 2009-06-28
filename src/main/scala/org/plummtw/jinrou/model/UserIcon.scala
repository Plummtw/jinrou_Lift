package org.plummtw.jinrou.model 
 
import net.liftweb._ 
import mapper._ 
import http._ 
import SHtml._ 
import util._

class UserIcon extends LongKeyedMapper[UserIcon] with IdPK {
  def getSingleton = UserIcon // what's the "meta" object
  //def primaryKeyField = id

  // the primary key
  // object id            extends MappedLongIndex(this)
  object icon_group    extends MappedInt(this)
  
  object icon_name     extends MappedString(this,20)
  object icon_filename extends MappedString(this,80)
  object icon_width    extends MappedInt(this)
  object icon_height   extends MappedInt(this)
  object color         extends MappedString(this,10)
  

  object created       extends MappedDateTime(this) {
    override def defaultValue = new java.util.Date()
  }
  object updated       extends MappedDateTime(this) with LifecycleCallbacks {
    override def beforeCreate = this(new java.util.Date())
    override def beforeUpdate = this(new java.util.Date())
  }
  
}

object UserIcon extends UserIcon with LongKeyedMetaMapper[UserIcon] {
  override def fieldOrder = List(id, icon_group, icon_name, icon_filename, icon_width, icon_height,
                                 color, created, updated)
}

/*class UserIcon {
  String  icon_name
  String  icon_filename
  Integer icon_width
  Integer icon_height
  String  color
  //String  session_id
  
  static constraints = {
    icon_name(nullable:false, blank:false, maxSize:20)
    icon_filename(nullable:false, blank:false, maxSize:80)
    icon_width(nullable:false, min:1, max:64)
    icon_height(nullable:false, min:1, max:64)
    
    color(nullable:false, maxSize:10)
    //session_id(nullable:false, maxSize:34)
    
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
        `icon_no` smallint(5) unsigned NOT NULL default '0',
				`icon_name` varchar(20) default NULL,
				`icon_filename` varchar(30) default NULL,
				`icon_width` tinyint(3) unsigned default NULL,
				`icon_height` tinyint(3) unsigned default NULL,
				`color` varchar(8) default NULL,
				`session_id` varchar(34) default NULL,
				PRIMARY KEY(`icon_no`)
*/