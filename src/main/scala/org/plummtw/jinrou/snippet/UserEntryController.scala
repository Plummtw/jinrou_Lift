package org.plummtw.jinrou.snippet

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
import org.plummtw.jinrou.data._

// 創新使用者用的 Lock，以免人數超過人數上限
object UserEntryLock {}

// 使用者說話用的 Lock
object UserSayLock {
  val lock_hash = scala.collection.mutable.Map[Long, Object]()

  // 每個 User 給一個 Lock
  def get_lock(id: Long) : Object = {
    if (!lock_hash.contains(id)) {
      lock_hash(id) = new Object()
    }

    return lock_hash(id)
  }

  def remove_lock(id: Long) = {
    lock_hash -= id
  }
}

class UserEntryController {
  // 新增使用者
  def add (xhtml : Group) : NodeSeq = {
    val room_no = S.param("room_no").getOrElse("0")
    val room_id : Long =
      try { room_no.toLong } 
      catch { case e: Exception => 0 }    
    //val room        = Room.get(room_id)
    val room_list   = Room.findAll(By(Room.id, room_id))
    val room        = if (room_list.length == 0) null else room_list(0)

    val group : Int=
      try { S.param("group").getOrElse("1").toInt }
      catch { case e: Exception => 1 }
    
    // 參數
    var uname       = ""
    var handle_name = ""
    var password    = ""
    var trip        = ""
    var sex         = "M"
    var role        = RoleEnum.NONE.toString
    var user_icon   = "2"
    
    val sex_map  = Map("M"->"Male", "F"->"Female")
    val sex_radios = SHtml.radio(sex_map.keys.toList, Full(sex), sex = _)
    
    val role_radios = SHtml.radio(RoleEnum.ROLE_MAP.keys.map(_.toString).toList, Full(role), role = _)
    val user_icon_radios = SHtml.radio(UserIcon.findAll(NotBy(UserIcon.id, 1)).map(_.id.is.toString), Full(user_icon), user_icon = _)
     
    //println("TEST")
    if (room == null) {
      S.error(<b>找不到村莊A</b>)
      S.redirectTo("main.html")
    }
    if (room.status.is == RoomStatusEnum.ENDED.toString()) {
      S.error(<b>遊戲已經結束</b>)
      S.redirectTo("main.html")
    }
    
    //for (role_group <- JinrouUtil.zipListBySize(2)(RoleEnum.ROLE_MAP.keys.toList);role <- role_group)
    //  println (role.toString) 

    val role_list = JinrouUtil.zipListBySize(2)(RoleEnum.ROLE_MAP.keys.toList)
    val role_xhtml : scala.xml.Elem = 
      if (room.room_flags.is.indexOf(RoomFlagEnum.WISH_ROLE.toString) == -1)
        <tr>{SHtml.hidden{() => role=RoleEnum.NONE.toString}}</tr>
      else
        <tr>
          <td align="center"><img src="images/user_regist_role.gif"/></td>
          <td>
            <table>
            {
              for (role_group <- role_list) yield
                <tr> {
                  for (role <- role_group) yield 
                    <td>{var x : RoleData = RoleEnum.get_role(role)
                         if (x != null) 
                           x.ctext
                         else
                           ""}</td>
                    <td>{role_radios(role.toString)}</td>
                } </tr>
            }
            </table>
          </td>
          <td></td>
        </tr>
    
    val user_icon_xhtml : scala.xml.Elem = { 
      val icon_list  = UserIcon.findAll(By(UserIcon.icon_group, group))
      val icon_groups = JinrouUtil.zipListBySize(5)(icon_list)
    
      <table border="0" style="font-size:10pt;"> {
        for (val icon_group <- icon_groups) yield <tr> { 
           for (val icon <- icon_group) yield
             <td valign="top"><img src={icon.icon_filename.is} width={icon.icon_width.is.toString} height={icon.icon_height.is.toString} border="2" style={"border-color:" + icon.color.is}/></td>
             <td width="150px">{icon.icon_name.is}<br/><font color={icon.color.is}>◆</font>{user_icon_radios(icon.id.is.toString)}</td>
        } </tr> 
      } 
      </table> 
    }
  
    def create_user () {
      //val user_icon_no = S.param("user_icon").getOrElse("0")
      val user_icon_id : Long =
        try { user_icon.toLong } 
        catch { case e: Exception => 2 }
        
      // 欄位檢核
      if (uname.trim() == "dummy_boy") {
        S.error(<b>使用者帳號錯誤</b>);
        return redirectTo("regist.html?group="+group+"&room_no=" + room_no)
      }

      val trip_value =
        if (trip == "") "" else JinrouUtil.generateSHA1_php(trip.trim()).substring(1,9)
      val user_entry = UserEntry.create.uname(uname.trim()).handle_name(handle_name.replace('　',' ').trim()).sex(sex)
                                .trip(trip_value)
                                .password(JinrouUtil.generateSHA1(password.trim()).substring(0,20))
                                .room_id(room_id).user_icon_id(user_icon_id)
                                .role(S.param("role").getOrElse("0")).last_words("")
                                .ip_address(S.request.map{x=>JinrouUtil.getIpAddress(x)}.openOr(""))
                      
      
      user_entry.validate match {
        case Nil => ;
        case xs  => S.error(xs); return redirectTo("regist.html?group="+group+"&room_no=" + room_no)
      }
      
      
      if (room.status.is != RoomStatusEnum.WAITING.toString) {
        S.error(<b>遊戲已經開始</b>); return redirectTo("regist.html?group="+group+"&room_no=" + room_no)
      }
      
      UserEntryLock.synchronized {
        // 檢查是否 uname 或 handle_name 重複
        val uname_count = UserEntry.count(By(UserEntry.room_id, room_id), By(UserEntry.uname, uname)) +
                          UserEntry.count(By(UserEntry.room_id, room_id), By(UserEntry.handle_name, handle_name))
        
        if (uname_count > 0) {
          S.error(<b>帳號或暱稱重複</b>); return redirectTo("regist.html?group="+group+"&room_no=" + room_no)
        }
      
      
        // 檢查是否超過人數上限
        val user_count = UserEntry.count(By(UserEntry.room_id, room_id))
        
        if (user_count >= room.max_user.is) {
          S.error(<b>村民已滿</b>); return redirectTo("regist.html?group="+group+"&room_no=" + room_no)
        }

        GameProcessLock.get_lock(room_id).synchronized {
          val room_day_list   = RoomDay.findAll(By(RoomDay.room_id, room_id), OrderBy(RoomDay.day_no, Descending))
          val room_day        = if (room_day_list.length == 0) null else room_day_list(0)

          if (room_day.day_no.is == 0) {
            // 加入使用者
            user_entry.save()
            S.setSessionAttribute("room_id", room_no)
            S.setSessionAttribute("user_id", user_entry.id.is.toString)
        
            val talk = Talk.create.roomday_id(room_day.id.is).actioner_id(user_entry.id.is)
                       .mtype(MTypeEnum.MESSAGE_COME.toString).message(user_entry.handle_name.is)
            talk.save()

            room.updated(new java.util.Date())
            room.save
          }
        
          //println("login.html?room_no=" + room_no)
        }
        redirectTo("login.html?room_no=" + room_no)
      }
    }
    
    bind("entry", xhtml,
      "room_id"           -> <span>{room_id}</span>,
      "room_name"         -> <span>{room.room_name.is}</span>,
      "room_comment"      -> <span>{room.room_comment.is}</span>,
      
      "uname"             -> SHtml.text(uname,         uname = _,       "size"->"30", "maxlength"->"30", "style"->"background-color:#eeccaa;color:#774400;"),
      "trip"              -> SHtml.text(trip,          trip  = _,       "size"->"30", "maxlength"->"30", "style"->"background-color:#eeccaa;color:#774400;"),
      "handle_name"       -> SHtml.text(handle_name,   handle_name = _, "size"->"30", "maxlength"->"30", "style"->"background-color:#eeccaa;color:#774400;"),
      "password"          -> SHtml.password(password,  password = _,    "size"->"30", "maxlength"->"30", "style"->"background-color:#eeccaa;color:#774400;"),
      
      "male"              -> sex_radios(0),
      "female"            -> sex_radios(1),
      
      "wish_role"         -> role_xhtml,
      "user_icon"         -> user_icon_xhtml,
      
      "submit"            -> SHtml.submit(" 登  錄 ",  create_user)
    )
  }
  
  // 登入
  def login(xhtml : Group) : NodeSeq = {
    val room_no = S.param("room_no").getOrElse("0")
    val room_id : Long =
      try { room_no.toLong } 
      catch { case e: Exception => 0 }
    val room_list   = Room.findAll(By(Room.id, room_id))
    val room        = if (room_list.length == 0) null else room_list(0)
    
    if (room == null) {
      S.error(<b>找不到村莊</b>)
      S.redirectTo("main.html")
    }
    if (room.status.is == RoomStatusEnum.ENDED.toString()) {
      S.error(<b>遊戲已經結束</b>)
      S.redirectTo("main.html")
    }
    
    val param_uname    = S.param("uname").getOrElse("").trim()
    val param_password = S.param("password").getOrElse("").trim()
    
    if (param_uname  == "dummy_boy") {
      S.error(<b>不可使用替身君登入</b>)
      S.redirectTo("main.html")
    }
    
    // 輸入 ID 和 PASSWORD 登入
    var user_entry : UserEntry = null
    if ((param_uname != "") && (param_password != "")) {
      val user_entry_list = UserEntry.findAll(By(UserEntry.uname, param_uname), 
                                              By(UserEntry.password, JinrouUtil.generateSHA1(param_password).substring(0,20)),
                                              By(UserEntry.room_id, room_id))
      
      if (user_entry_list.length != 0) { 
        user_entry = user_entry_list(0)
        S.setSessionAttribute("room_id", room_no)
        S.setSessionAttribute("user_id", user_entry.id.is.toString)
      }
    } else if ((S.getSessionAttribute("room_id").getOrElse("") == room_no) && 
               (S.getSessionAttribute("user_id").getOrElse("") != "")){
      val user_entry_id : Long = 
        try { S.getSessionAttribute("user_id").getOrElse("0").toLong }
        catch { case e: Exception => 0 }
        
      val user_entry_list = UserEntry.findAll(By(UserEntry.id, user_entry_id), 
                                              By(UserEntry.room_id, room_id))
      
      if (user_entry_list.length != 0) { 
        user_entry = user_entry_list(0)
      }
    }
    
    var message       = ""
    var redirect_page = "" 
    if (user_entry != null) {
      val dead_mode   = ((!(user_entry.live.is)) && (room.status.is != RoomStatusEnum.ENDED.toString))
      val heaven_mode = ((!(user_entry.live.is)) && (room.room_flags.is.indexOf(RoomFlagEnum.DEATH_LOOK.toString) != -1))

      // 一登入之後先設初期的 Session 內容        
      S.setSessionAttribute("dead_mode",   dead_mode.toString)
      S.setSessionAttribute("heaven_mode", heaven_mode.toString)
      S.setSessionAttribute("auto_reload", "")
      S.setSessionAttribute("play_sound",  false.toString)
      S.setSessionAttribute("list_down",   false.toString)

      user_entry.ip_address(S.request.map{x=>JinrouUtil.getIpAddress(x)}.openOr(""))
      user_entry.ip_address_md5(JinrouUtil.generateSHA1(user_entry.ip_address.is))
      user_entry.save()
    
      redirect_page = "game_frame"
      message       = "登入完成"
    } else {
      redirect_page = "game_view"
      message       = "觀戰畫面移動中"
    }
    
    val redirect_url = redirect_page + ".html?room_no=" + room_no
  
    bind("entry", xhtml,
      "meta_tag"    -> <meta http-equiv="refresh" content={"1;URL=" + redirect_url} />,
      "message_tag" -> <span>{message}</span>,
      "a_tag"       -> <a href={redirect_url}>按我繼續</a>
    )
  }
  
  // 登出
  def logout(xhtml : Group) : NodeSeq = {
    val room_no = S.param("room_no").getOrElse("0")
    val room_id : Long =
      try { room_no.toLong } 
      catch { case e: Exception => 0 }
    val room_list   = Room.findAll(By(Room.id, room_id))
    val room        = if (room_list.length == 0) null else room_list(0)
    
    if (room == null) {
      S.error(<b>找不到村莊</b>)
      S.redirectTo("main.html")
    }
  
    S.setSessionAttribute("room_id", "")
    S.setSessionAttribute("user_id", "")
    S.redirectTo("game_view.html?room_no=" + room_no)
  }
  
  // 自刪
  def go_out(xhtml : Group) : NodeSeq = {
    val room_no = S.param("room_no").getOrElse("0")
    val room_id : Long =
      try { room_no.toLong } 
      catch { case e: Exception => 0 }
    val room_list   = Room.findAll(By(Room.id, room_id))
    val room        = if (room_list.length == 0) null else room_list(0)
    
    if (room == null) {
      S.error(<b>找不到村莊</b>)
      S.redirectTo("main.html")
    }
    
    // 檢查是否登入
    var user_entry : UserEntry = null
    var user_role  : RoleData  = RoleNone
    
    val user_entry_no : String = S.getSessionAttribute("user_id").getOrElse("")
    if ((S.getSessionAttribute("room_id").getOrElse("") == room_no) && 
        (user_entry_no != "") && (room.status.is == RoomStatusEnum.WAITING.toString)){
      val user_entry_list   = UserEntry.findAll(By(UserEntry.id, user_entry_no.toLong), By(UserEntry.room_id, room_id))
      user_entry            = if (user_entry_list.length == 0) null else user_entry_list(0)
      
      if (user_entry == null) {
        S.error(<b>找不到對應的玩家 {user_entry_no}</b>)
        S.redirectTo("main.html")
      }
      
      val room_day_list   = RoomDay.findAll(By(RoomDay.room_id, room_id), OrderBy(RoomDay.day_no, Descending))
      val room_day        = if (room_day_list.length == 0) null else room_day_list(0)
      
      // 使用者離開了
      user_entry.room_id(0)
      user_entry.save()
      
      val talk = Talk.create.roomday_id(room_day.id.is).actioner_id(user_entry.id.is)
                 .mtype(MTypeEnum.MESSAGE_LEAVE.toString).message(user_entry.handle_name.is)
      talk.save()
      
      
      // 投票重新開始
      Vote.bulkDelete_!!(By(Vote.roomday_id, room_day.id.is))
      
      // Update 使用者狀態
      //val user_entry_for_update = UserEntry.findAll(NotBy(UserEntry.handle_name, "dummy_boy"), By(UserEntry.room_id, room_id))
      //user_entry_for_update.foreach({user => user.user_flags("").save()})
      DB.use(DefaultConnectionIdentifier) { conn =>
        DB.prepareStatement("update UserEntry set user_flags = '' where room_id = ? and uname != 'dummy_boy'", conn) { stmt =>
          stmt.setLong(1, room.id.is)
          stmt.executeUpdate()
        }
      }
      
      val talk2 = Talk.create.roomday_id(room_day.id.is)
                  .mtype(MTypeEnum.MESSAGE_REVOTE0.toString)
      talk2.save()

      room.updated(new java.util.Date())
      room.save
    }
  
    S.setSessionAttribute("room_id", "")
    S.setSessionAttribute("user_id", "")
    S.redirectTo("game_view.html?room_no=" + room_no)
  }

}