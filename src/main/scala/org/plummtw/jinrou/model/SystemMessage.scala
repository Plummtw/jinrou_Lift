package org.plummtw.jinrou.model 
 
import net.liftweb._ 
import mapper._ 
import http._ 
import SHtml._ 
import util._

class SystemMessage extends LongKeyedMapper[SystemMessage] with IdPK {
  def getSingleton = SystemMessage // what's the "meta" object
  //def primaryKeyField = id

  // the primary key
  // object id            extends MappedLongIndex(this)
  object roomday_id    extends MappedLongForeignKey(this, RoomDay) 
  object actioner_id   extends MappedLongForeignKey(this, UserEntry)
  object actionee_id   extends MappedLongForeignKey(this, UserEntry)
  
  object message       extends MappedString(this, 80)
  object mtype         extends MappedString(this, 3)
  
 
  object created       extends MappedDateTime(this) {
    override def defaultValue = new java.util.Date()
  }

  object updated       extends MappedDateTime(this) with LifecycleCallbacks {
    override def beforeCreate = this(new java.util.Date())
    override def beforeUpdate = this(new java.util.Date())
  }
  
}

object SystemMessage extends SystemMessage with LongKeyedMetaMapper[SystemMessage] {
  override def fieldOrder = List(id, roomday_id, actioner_id, actionee_id, message, mtype,
                                 created, updated)
}

/*


public enum MessageCode {
  V,  // Victory 勝利
  O,  // Objection 抗議
  L,  // 遺言
  S,  // 系統訊息
  
  VT, // 投票
  VR, // 重新投票
  
  AA, // 占卜師行動
  AC, // 牧師行動
  AH, // 獵人行動
  AS, // 模仿師行動
  AW, // 人狼行動
  AV, // 村民推理
  
  //RA, // 占卜師結果
  //RB, // 背德結果
  //RC, // 牧師結果
  //RF, // 妖狐結果
  //RH, // 獵人結果
  //RM, // 狂人結果
  //RN, // 靈能者結果
  //RR, // 逃亡者結果
  
  DB, // 背德連帶死亡
  DC, // 牧師犧牲
  DD, // 惡魔特殊死亡
  DF, // 妖狐占死
  DH, // 吊死
  DP, // 毒死(日)
  DQ, // 毒死(夜)
  DR, // 逃亡者特殊死亡
  DV, // 暴斃死亡
  DW  // 人狼咬死
  
  ;
  
  private static List list;        
  
  public String value() {
    return name();    
  }
  
  public static List getList() {
    if(list != null) { 
      return list;     
    }     
    
    return buildList();    
  } 
  
  private static synchronized List buildList() {  
    list = new ArrayList();        
    for (MessageCode c: MessageCode.values()) {  
      list.add(c.name());        
    }     
    
    return list; 
  }    
  
  public static MessageCode fromValue(String v) {
    return valueOf(v);    
  }
}


class SystemMessage {
  //static belongsTo = [room_day: RoomDay]
  //RoomDay      room_day
  
  UserEntry    actioner
  UserEntry    actionee
  
  String       message
  String       type
  
  static constraints = {
    message(nullable:true, maxSize:80)
    type(nullable:false, maxSize:2, inList:MessageCode.getList())
    
    actioner(nullable:true)
    actionee(nullable:true)
    
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