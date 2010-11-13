package org.plummtw.jinrou.util

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

object LinkHelper {
  lazy val identicon = {
    val identicon_list = AdminManage.findAll(By(AdminManage.param_name, "identicon_server"))
    if (identicon_list.length == 0)
      "http://identicon.relucks.org/"
    else
      identicon_list(0).param_value.is
  }

  lazy val trip = {
    val trip_list = AdminManage.findAll(By(AdminManage.param_name, "trip_server"))
    if (trip_list.length == 0)
      "http://diam.ngct.net/trip.php?go=trip&id="
    else
      trip_list(0).param_value.is
  }
}

object UserEntryHelper {
  // http://74.82.5.143/
  // http://identicon.relucks.org/
  def user_cell(room: Room, current_user: UserEntry, user_entry: UserEntry, reveal: Boolean ) : NodeSeq = {
    var result : NodeSeq = Seq()
    val user_icon : UserIcon = user_entry.get_user_icon()
    val id_icon : NodeSeq =
      if ((room.status.is != RoomStatusEnum.PLAYING.toString) || (user_entry.ip_address.is != user_entry.ip_address0.is))
        Seq(<img src={LinkHelper.identicon + user_entry.ip_address_md5.is} />)
      else
        NodeSeq.Empty

    if (user_entry.live.is)
      result ++= <td valign="top" bgcolor={if (user_entry.user_flags.is == UserEntryFlagEnum.VOTED.toString) "#FF50FF" else ""}>
      <img src={user_icon.icon_filename.is} width={user_icon.icon_width.is.toString} height={user_icon.icon_height.is.toString} border="2" style={"border-color:" + user_icon.color.is} />{
       if (room.has_flag(RoomFlagEnum.ITEM_MODE) && (reveal || ((current_user != null) && (user_entry.id.is == current_user.id.is))))
         Seq(<br/><span>{"金:" + user_entry.cash.is}</span>) else NodeSeq.Empty}</td>
    else
      result ++= <td valign="top" bgcolor="#992222"><img src="images/grave.gif"   border="2"  onMouseover={"this.src='" + user_icon.icon_filename.is + "'"} onMouseout="this.src='images/grave.gif'"  style={"border-color:" + user_icon.color.is} />{
       // if (user_entry.user_flags.is == UserEntryFlagEnum.VOTED.toString) "#FF50FF" else ""}{
       if (room.has_flag(RoomFlagEnum.ITEM_MODE) && reveal) Seq(<br/><span>{"金：" + user_entry.cash.is}</span>) else NodeSeq.Empty}</td>

    // <img src={"http://identicon.relucks.org/" + user_entry.ip_address_md5.is} />
    result ++= <td bgcolor={if (!user_entry.live.is) "#992222" else if (user_entry.user_flags.is == UserEntryFlagEnum.VOTED.toString) "#FF50FF" else ""}>
          <font color={user_icon.color.is}>◆</font>{user_entry.handle_name.is}<br/>
          {
            if (user_entry.trip.is != "")
              Seq(<span>◆</span>, <a target="_blank" style="text-decoration: none;" href={LinkHelper.trip + user_entry.trip.is}>{user_entry.trip.is}</a>, <br/>)
            else
              NodeSeq.Empty
          }
          { if (reveal) 
             Seq(<strong>{user_entry.get_role_field()}<br/>{user_entry.get_subrole_field()}</strong>)
            else NodeSeq.Empty}

          {id_icon}({ if (user_entry.live.is) "生存中" else "死亡" })
          { if (reveal) {if (user_entry.has_flag(UserEntryFlagEnum.RELIGION) ) Seq(<font color="#EEAA55">教</font>) else NodeSeq.Empty} else NodeSeq.Empty }
          { if (reveal) {if (user_entry.has_flag(UserEntryFlagEnum.BECAME_MOB) ) Seq(<font color="#AAAAAA">暴</font>) else NodeSeq.Empty} else NodeSeq.Empty }
         </td>
         
    return result  
  }

  def user_admin_cell(room: Room, user_entry: UserEntry ) : NodeSeq = {
    var result : NodeSeq = Seq()
    val user_icon : UserIcon = user_entry.get_user_icon()
    val id_icon : NodeSeq =
      if ((room.status.is != RoomStatusEnum.PLAYING.toString) || (user_entry.ip_address.is != user_entry.ip_address0.is))
        Seq(<img src={LinkHelper.identicon + user_entry.ip_address_md5.is} />)
      else
        NodeSeq.Empty

    if (user_entry.live.is)
      result ++= <td valign="top" bgcolor={if (user_entry.user_flags.is == UserEntryFlagEnum.VOTED.toString) "#FF50FF" else ""}>
      <img src={user_icon.icon_filename.is} width={user_icon.icon_width.is.toString} height={user_icon.icon_height.is.toString} border="2" style={"border-color:" + user_icon.color.is} /></td>
    else
      result ++= <td valign="top" bgcolor="#992222"><img src="images/grave.gif"   border="2"  onMouseover={"this.src='" + user_icon.icon_filename.is + "'"} onMouseout="this.src='images/grave.gif'"  style={"border-color:" + user_icon.color.is} /></td>

    // <img src={"http://identicon.relucks.org/" + user_entry.ip_address_md5.is} />
    result ++= <td bgcolor={if (!user_entry.live.is) "#992222" else if (user_entry.user_flags.is == UserEntryFlagEnum.VOTED.toString) "#FF50FF" else ""}>
          <font color={user_icon.color.is}>◆</font>{user_entry.handle_name.is}<br/>
          {
            if (user_entry.trip.is != "")
              Seq(<span>◆</span>, <a target="_blank" style="text-decoration: none;" href={LinkHelper.trip + user_entry.trip.is}>{user_entry.trip.is}</a>, <br/>)
            else
              NodeSeq.Empty
          }
          {
             Seq(<strong>{user_entry.get_role_field()}<br/>{user_entry.get_subrole_field()}</strong>)
          }

          {Seq(<input type="checkbox" id={"id" + user_entry.user_no.is} name={"id" + user_entry.user_no.is} />)}
          {id_icon}({ if (user_entry.live.is) "生存中" else "死亡" })
          {if (user_entry.has_flag(UserEntryFlagEnum.RELIGION) ) Seq(<font color="#EEAA55">教</font>) else NodeSeq.Empty}
         </td>

    return result
  }

  def user_select_cell(user_entry : UserEntry, targetable : Boolean ) : NodeSeq = {
    var result : NodeSeq = Seq()
    val user_icon : UserIcon = user_entry.get_user_icon()
    if (user_entry.live.is)
      result ++= <td class="table_votelist1" valign="top" bgcolor={if (user_entry.user_flags.is == UserEntryFlagEnum.VOTED.toString) "#FF50FF" else ""}>
      <img src={user_icon.icon_filename.is} width={user_icon.icon_width.is.toString} height={user_icon.icon_height.is.toString} border="2" style={"border-color:" + user_icon.color.is} /></td>
    else
      result ++= <td class="table_votelist1" valign="top" bgcolor="#992222"><img src="images/grave.gif"   border="2"  onMouseover={"this.src='" + user_icon.icon_filename.is + "'"} onMouseout="this.src='images/grave.gif'"  style={"border-color:" + user_icon.color.is} /></td>

    result ++= <td class="table_votelist2" width="150px" bgcolor={if (!user_entry.live.is) "#992222" else if (user_entry.user_flags.is == UserEntryFlagEnum.VOTED.toString) "#FF50FF" else ""}>
          {user_entry.handle_name.is}<br/><font color={user_icon.color.is}>◆</font>
          { if (targetable)
             Seq(<input type="radio" id="target" name="target" value={user_entry.id.is.toString} />)
            else
             NodeSeq.Empty}
         </td>
   return result
  }

  def user_choose_cell(user_entry : UserEntry, targetable : Boolean ) : NodeSeq = {
    var result : NodeSeq = Seq()
    val user_icon : UserIcon = user_entry.get_user_icon()
    if (user_entry.live.is)
      result ++= <td class="table_votelist1" valign="top" bgcolor={if (user_entry.user_flags.is == UserEntryFlagEnum.VOTED.toString) "#FF50FF" else ""}>
      <img src={user_icon.icon_filename.is} width={user_icon.icon_width.is.toString} height={user_icon.icon_height.is.toString} border="2" style={"border-color:" + user_icon.color.is} /></td>
    else
      result ++= <td class="table_votelist1" valign="top" bgcolor="#992222"><img src="images/grave.gif"   border="2"  onMouseover={"this.src='" + user_icon.icon_filename.is + "'"} onMouseout="this.src='grave.gif'"  style={"border-color:" + user_icon.color.is} /></td>

    result ++= <td class="table_votelist2" width="150px" bgcolor={if (!user_entry.live.is) "#992222" else if (user_entry.user_flags.is == UserEntryFlagEnum.VOTED.toString) "#FF50FF" else ""}>
          {user_entry.handle_name.is}<br/><font color={user_icon.color.is}>◆</font>
          { if (targetable)
             {Seq(<input type="checkbox" id={"id" + user_entry.user_no.is} name={"id" + + user_entry.user_no.is} />)}
            else
             NodeSeq.Empty}
         </td>
   return result
  }


  // User Table
  def user_table(room:Room, current_user:UserEntry, user_entrys: List[UserEntry], reveal: Boolean) : NodeSeq = {
    val user_groups = JinrouUtil.zipListBySize(5)(user_entrys)

    return <table border="0" cellpadding="0" cellspacing="5" style="font-size:10pt;border-width:1px;border-color:black;border-style:dotted;">    
    { for (val user_group <- user_groups) yield <tr> { 
       for (val user_entry <- user_group) yield user_cell(room, current_user, user_entry, reveal)
    } </tr> } </table> }

  // User Table
  def user_admin_table(room:Room, user_entrys: List[UserEntry]) : NodeSeq = {
    val user_groups = JinrouUtil.zipListBySize(5)(user_entrys)

    return <table border="0" cellpadding="0" cellspacing="5" style="font-size:10pt;border-width:1px;border-color:black;border-style:dotted;">
    { for (val user_group <- user_groups) yield <tr> {
       for (val user_entry <- user_group) yield user_admin_cell(room, user_entry)
    } </tr> } </table> }

  // User Select Table
  def user_select_table(user_entrys : List[UserEntry], targettable : List[UserEntry]) : NodeSeq = {
    val user_groups = JinrouUtil.zipListBySize(5)(user_entrys)

    return <table border="0"><tr>
             <td width="1000">
              <table border="0">
    { for (val user_group <- user_groups) yield <tr> { 
       for (val user_entry <- user_group) yield user_select_cell(user_entry, targettable.contains(user_entry))
    } </tr> } </table></td></tr></table> }
}

