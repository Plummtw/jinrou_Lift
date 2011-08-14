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

object TalkIdCache extends TimedCache[Long, UserEntry](30000)

object MessageHelper {
  // 道具腹語娃娃
  def ventriloquist_talk_tag(room:Room, room_day:RoomDay, talk:Talk, user:UserEntry, heaven_mode:Boolean, user_entrys:List[UserEntry]) : NodeSeq= {
    val style_str = "color:#009999;font-size:17pt;"
    val user_entry = user_entrys.filter(_.id.is == talk.actioner_id.is)(0)
    val user_icon  = user_entry.get_user_icon()

    if (heaven_mode)
      Seq(<tr><td width="200" align="left" valign="middle" style="border-bottom: silver 1px dashed;"><font color={user_icon.color.is}>◆</font>{user_entry.handle_name.is}<small>的腹語術</small></td>
          <td><span style="margin:1px;" align="left"></span></td>
          <td width="1000" valign="middle" style="border-bottom: silver 1px dashed;">
          <span style={style_str}> {Unparsed(talk.message.is)} </span></td></tr>)
      else
      Seq(<tr><td width="200" align="left" valign="middle" style="border-bottom: silver 1px dashed;"><small>腹語</small></td>
          <td><span style="margin:1px;" align="left"></span></td>
          <td width="1000" valign="middle" style="border-bottom: silver 1px dashed;">
          <span style={style_str}> {Unparsed(talk.message.is)} </span></td></tr>)
  }

  // 狂巫密言術
  def secret_talk_tag(room:Room, room_day:RoomDay, talk:Talk, user:UserEntry, heaven_mode:Boolean, user_entrys:List[UserEntry]) : NodeSeq= {
    val style_str = "color:red;font-size:18pt;"
    val user_entry = user_entrys.filter(_.id.is == talk.actioner_id.is)(0)
    val user_icon  = user_entry.get_user_icon()

    if (((user != null) && (!user.test_memoryloss(room, room_day, user_entrys)) && (user.subrole.is != SubroleEnum.FOXBELIEVER.toString) &&
           ((user.current_role == RoleEnum.WEREWOLF) || (user.current_role == RoleEnum.WOLFCUB) ||
            (user.current_role == RoleEnum.MADMAN) || (user.current_role == RoleEnum.SORCEROR)))
        || (heaven_mode)) {
      if ((room.has_flag(RoomFlagEnum.SORCEROR_WHISPER1)) && (!heaven_mode)) {
        val user_index = user_entrys.indexOf(user_entry)
        val hint =
          if (room_day.day_no.is % 4 == 2)
            "橫" + ((user_index/5)+1).toString
          else
            "直" + ((user_index%5)+1).toString
        Seq(<tr><td width="200" align="left" valign="middle" style="border-bottom: silver 1px dashed;"><small>{hint}</small><small>的密言術</small></td>
            <td><span style="margin:1px;" align="left"></span></td>
            <td width="1000" valign="middle" style="border-bottom: silver 1px dashed;">
            <span style={style_str}> {Unparsed(talk.message.is)} </span></td></tr>)
      } else
        Seq(<tr><td width="200" align="left" valign="middle" style="border-bottom: silver 1px dashed;"><font color={user_icon.color.is}>◆</font>{user_entry.handle_name.is}<small>的密言術</small></td>
            <td><span style="margin:1px;" align="left"></span></td>
            <td width="1000" valign="middle" style="border-bottom: silver 1px dashed;">
            <span style={style_str}> {Unparsed(talk.message.is)} </span></td></tr>)
    } else
      Seq(<tr><td width="200" align="left" valign="middle" style="border-bottom: silver 1px dashed;">　</td>
          <td><span style="margin:1px;" align="left"></span></td>
          <td width="1000" valign="middle" style="border-bottom: silver 1px dashed;">
          <span style={style_str}> {Unparsed(talk.message.is)} </span></td></tr>)
  }

  // 背德偽裝發言
  def disguised_talk_tag(talk:Talk, user:UserEntry, heaven_mode:Boolean, user_entrys:List[UserEntry]) : NodeSeq= {
    val font_size =
      try {talk.font_type.is.toInt}
      catch { case e:Exception => 0}
    val style_str = if (font_size >= 20) "font-size:" + talk.font_type.is +"pt;font-weight:bold;color:"
                    else "font-size:" + talk.font_type.is +"pt;"
    val user_entry = user_entrys.filter(_.id.is == talk.actioner_id.is)(0)
    val user_icon  = user_entry.get_user_icon()
    val user_target_list = user_entrys.filter(_.id.is == talk.actionee_id.is)
    val user_target =
      if (user_target_list.length != 0)
        user_target_list(0)
      else
        user_entry

    val user_target_icon  = user_target.get_user_icon()

    if (((user != null) && (user_entry.id.is == user.id.is)) || (heaven_mode))
      Seq(<tr><td width="200" align="left" valign="middle" style="border-bottom: silver 1px dashed;"><font color={user_icon.color.is}>◆</font>{user_entry.handle_name.is}<small>的偽裝發言({user_target.handle_name.is})</small></td>
          <td><span style="margin:1px;" align="left"></span></td>
          <td width="1000" valign="middle" style="border-bottom: silver 1px dashed;">
          <span style={style_str}> {Unparsed(talk.message.is)} </span></td></tr>)
    else
      Seq(<tr><td width="200" align="left" valign="middle" style={"border-bottom: silver 1px dashed;"}>
        <font color={user_target_icon.color.is}>◆</font>{user_target.handle_name.is} </td>
        <td><span style="margin:1px;" align="left"></span></td>
        <td width="1000" valign="middle" style={"border-bottom: silver 1px dashed;"}>
        <span style={style_str}> {Unparsed(talk.message.is)} </span></td></tr>)
  }

  // 被封印的言論
  def sealed_talk_tag(talk:Talk, user:UserEntry, heaven_mode:Boolean, user_entrys:List[UserEntry]) : NodeSeq= {
    val font_size =
      try {talk.font_type.is.toInt}
      catch { case e:Exception => 0}
    val style_str = if (font_size >= 20) "font-size:" + talk.font_type.is +"pt;font-weight:bold;color:"
                    else "font-size:" + talk.font_type.is +"pt;"
    val user_entry = user_entrys.filter(_.id.is == talk.actioner_id.is)(0)
    val user_icon  = user_entry.get_user_icon()

    if (((user != null) && (user_entry.id.is == user.id.is)) || (heaven_mode))
      Seq(<tr><td width="200" align="left" valign="middle" style="border-bottom: silver 1px dashed;">
        <font color={user_icon.color.is}>◆</font>{user_entry.handle_name.is}<small>(被封印)</small> </td>
        <td><span style="margin:1px;" align="left"></span></td>
        <td width="1000" valign="middle" style="border-bottom: silver 1px dashed;">
        <span style={style_str}> {Unparsed(talk.message.is)} </span></td></tr>)
    else
      NodeSeq.Empty
  }


  // 一般言論
  def simple_talk_tag(talk:Talk, user_entrys:List[UserEntry], grey_out:Boolean) : NodeSeq= {
    val font_size =
      try {talk.font_type.is.toInt}
      catch { case e:Exception => 0}
    val grey_out_str = if (grey_out)  "color:#FFFFFF;background-color:#777777;" else ""
    val style_str = if (font_size >= 20) "font-size:" + talk.font_type.is +"pt;font-weight:bold;"
                    else "font-size:" + talk.font_type.is +"pt;"
    val user_entry_list  = user_entrys.filter(_.id.is == talk.actioner_id.is)
    val user_entry       = 
      if( user_entry_list.length == 0) {
        if (talk.actioner_id.is != 0)
          UserEntry.findAll(By(UserEntry.id, talk.actioner_id.is))(0)
        else
          AdminUserEntry //user_entrys(0)
      }
      else
        user_entry_list(0) 
    val user_icon  = user_entry.get_user_icon()
    
    Seq(<tr><td width="200" align="left" valign="middle" style={grey_out_str+"border-bottom: silver 1px dashed;"}>
        <font color={user_icon.color.is}>◆</font>{user_entry.handle_name.is} </td>
        <td><span style="margin:1px;" align="left"></span></td>
        <td width="1000" valign="middle" style={grey_out_str+"border-bottom: silver 1px dashed;"}>
        <span style={style_str}> {Unparsed(talk.message.is)} </span></td></tr>)
  }

  // 一般言論
  def fog_talk_tag(talk:Talk, user: UserEntry, heaven_mode:Boolean, user_entrys:List[UserEntry]) : NodeSeq= {
    val font_size =
      try {talk.font_type.is.toInt}
      catch { case e:Exception => 0}
    val style_str = if (font_size >= 20) "font-size:" + talk.font_type.is +"pt;font-weight:bold;"
                    else "font-size:" + talk.font_type.is +"pt;"
    val user_entry_list  = user_entrys.filter(_.id.is == talk.actioner_id.is)
    val user_entry       =
      if( user_entry_list.length == 0) {
        if (talk.actioner_id.is != 0)
          UserEntry.findAll(By(UserEntry.id, talk.actioner_id.is))(0)
        else
          null
      }
      else
        user_entry_list(0)
    val user_icon  = user_entry.get_user_icon()

    if (((user != null) && (user_entry.id.is == user.id.is)) || (heaven_mode))
      Seq(<tr><td width="200" align="left" valign="middle" style="border-bottom: silver 1px dashed;">
        <font color={user_icon.color.is}>◆</font>{user_entry.handle_name.is} </td>
        <td><span style="margin:1px;" align="left"></span></td>
        <td width="1000" valign="middle" style="border-bottom: silver 1px dashed;">
        <span style={style_str}> {Unparsed(talk.message.is)} </span></td></tr>)
    else
      Seq(<tr><td width="200" align="left" valign="middle" style="border-bottom: silver 1px dashed;">　</td>
        <td><span style="margin:1px;" align="left"></span></td>
        <td width="1000" valign="middle" style="border-bottom: silver 1px dashed;">
        <span style={style_str}> {Unparsed(talk.message.is)} </span></td></tr>)
  }

  // 其他類型訊息
  def simple_talk_tag(room:Room, room_day:RoomDay, talk: Talk, user: UserEntry, heaven_mode: Boolean, user_entrys:List[UserEntry]) : NodeSeq= {
    val font_size =
      try {talk.font_type.is.toInt}
      catch { case e:Exception => 0}
    val style_str = if (font_size >= 20) "font-size:" + talk.font_type.is +"pt;font-weight:bold;color:"
                    else "font-size:" + talk.font_type.is +"pt;color:"
    val mtype : MTypeEnum.Value = MTypeEnum.valueOf(talk.mtype.is) getOrElse(null)

    val user_entry = (if ((mtype == MTypeEnum.TALK_ADMIN) || (mtype == MTypeEnum.TALK_ADMIN_PRIVATE))
                        null
                        else get_user_entry(user_entrys.filter(_.id.is == talk.actioner_id.is), talk.actioner_id.is))
    val user_icon  = (if ((mtype == MTypeEnum.TALK_ADMIN) || (mtype == MTypeEnum.TALK_ADMIN_PRIVATE))
                        null
                        else user_entry.get_user_icon())
  
    mtype match {
      case MTypeEnum.TALK_ADMIN    =>
        Seq(<tr><td width="200" align="left" valign="middle" style={"border-bottom: silver 1px dashed;"}>
            <font color="#FF7700;">管理員</font></td>
            <td><span style="margin:1px;" align="left"></span></td>
            <td width="1000" valign="middle" style={"border-bottom: silver 1px dashed;"}>
            <span style={"color:#FF7700;" + style_str}> {Unparsed(talk.message.is)} </span></td></tr>)

      case MTypeEnum.TALK_ADMIN_PRIVATE    =>
        val user_target_list = user_entrys.filter(_.id.is == talk.actionee_id.is)
        val user_target =
          if (user_target_list.length != 0)
            user_target_list(0)
          else
            user_entrys(0)

        if (((user != null) && (user.id.is == talk.actionee_id.is)) || (heaven_mode))
          Seq(<tr><td width="200" align="left" valign="middle" style={"border-bottom: silver 1px dashed;"}>
              <font color="#FF7700;">管理員<small>的私人訊息({user_target.handle_name.is})</small></font></td>
              <td><span style="margin:1px;" align="left"></span></td>
              <td width="1000" valign="middle" style={"border-bottom: silver 1px dashed;"}>
              <span style={"color:#FF7700;" + style_str}> {Unparsed(talk.message.is)} </span></td></tr>)
        else
          NodeSeq.Empty

      case MTypeEnum.TALK_NIGHT    => 
        if (((user != null) && (user_entry.id.is == user.id.is)) || (heaven_mode))
          Seq(<tr><td width="200" align="left" valign="middle" style="border-bottom: silver 1px dashed;color:snow;background-color:#000030">
          <font color={user_icon.color.is}>◆</font>{user_entry.handle_name.is}<small>的自言自語</small></td>
          <td><span style="margin:1px;" align="left"></span></td>
          <td width="1000" valign="middle" style="border-bottom: silver 1px dashed;background-color:#000030">
          <span style={style_str+"snow"}> {Unparsed(talk.message.is)} </span></td></tr>)
        else
          NodeSeq.Empty
                                                      
      case MTypeEnum.TALK_WEREWOLF =>
        if (((user != null) && (!user.test_memoryloss(room, room_day, user_entrys)) 
            && (user.role.is.substring(0,1) == RoleEnum.WEREWOLF.toString)) || (heaven_mode))
          Seq(<tr><td width="200" align="left" valign="middle" style="border-bottom: silver 1px dashed;color:#FFCCFF;background-color:#000030">
          <font color={user_icon.color.is}>◆</font>{user_entry.handle_name.is}<small>(人狼)</small></td>
          <td><span style="margin:1px;" align="left"></span></td>
          <td width="1000" valign="middle" style="border-bottom: silver 1px dashed;background-color:#000030">
          <span style={style_str+"#FFCCFF"}> {Unparsed(talk.message.is)} </span></td></tr>)
        else
          Seq(<tr><td width="200" align="left" valign="middle" style="border-bottom: silver 1px dashed;color:#FFCCFF;background-color:#000030">
          狼的叫聲 </td><td><span style="margin:1px;" align="left"></span></td>
          <td width="1000" valign="middle" style="border-bottom: silver 1px dashed;background-color:#000030">
          <span style={style_str+"#FFCCFF"}> 似乎在遠處聽見了狼叫聲･･･ </span></td></tr>)

     case MTypeEnum.TALK_WOLFCUB =>
        if (((user != null) && (!user.test_memoryloss(room, room_day, user_entrys))
            && (user.role.is.substring(0,1) == RoleEnum.WOLFCUB.toString)) || (heaven_mode))
          Seq(<tr><td width="200" align="left" valign="middle" style="border-bottom: silver 1px dashed;color:#EE0000;background-color:#000030">
          <font color={user_icon.color.is}>◆</font>{user_entry.handle_name.is}<small>(幼狼)</small></td>
          <td><span style="margin:1px;" align="left"></span></td>
          <td width="1000" valign="middle" style="border-bottom: silver 1px dashed;background-color:#000030">
          <span style={style_str+"#EE0000"}> {Unparsed(talk.message.is)} </span></td></tr>)
        else
          Seq(<tr><td width="200" align="left" valign="middle" style="border-bottom: silver 1px dashed;color:#EE0000;background-color:#000030">
          狼的叫聲 </td><td><span style="margin:1px;" align="left"></span></td>
          <td width="1000" valign="middle" style="border-bottom: silver 1px dashed;background-color:#000030">
          <span style={style_str+"#EE0000"}> 似乎在遠處聽見了狼叫聲･･･ </span></td></tr>)

      case MTypeEnum.TALK_GEMINI   =>
        if (((user != null) && (!user.test_memoryloss(room, room_day, user_entrys))
             && (!(user.test_fake(room_day)))
             && (user.role.is.substring(0,1) == RoleEnum.GEMINI.toString)) || (heaven_mode))
          Seq(<tr><td width="200" align="left" valign="middle" style="border-bottom: silver 1px dashed;color:#DDAA77;background-color:#000030">
          <font color={user_icon.color.is}>◆</font>{user_entry.handle_name.is}<small>(共有者)</small></td>
          <td><span style="margin:1px;" align="left"></span></td>
          <td width="1000" valign="middle" style="border-bottom: silver 1px dashed;background-color:#000030">
          <span style={style_str+"#DDAA77"}> {Unparsed(talk.message.is)} </span></td></tr>)
        else if (room.has_flag(RoomFlagEnum.GEMINI_TALK))
          Seq(<tr><td width="200" align="left" valign="middle" style="border-bottom: silver 1px dashed;font-size:8pt;color:#DDAA77;background-color:#000030">
          共有者的聲音 </td><td><span style="margin:1px;" align="left"></span></td>
          <td width="1000" valign="middle" style="border-bottom: silver 1px dashed;background-color:#000030">
          <span style="font-size:8pt;color:#DDAA77"> 悄悄話･･･ </span></td></tr>)
        else
          NodeSeq.Empty

      case MTypeEnum.TALK_GEMINI_DAY   =>
        if (((user != null) && (!user.test_memoryloss(room, room_day, user_entrys))
             && (!(user.test_fake(room_day)))
             && (user.role.is.substring(0,1) == RoleEnum.GEMINI.toString)) || (heaven_mode))
          Seq(<tr><td width="200" align="left" valign="middle" style="border-bottom: silver 1px dashed;color:#AA7744">
          <font color={user_icon.color.is}>◆</font>{user_entry.handle_name.is}<small>(共有者)</small></td>
          <td><span style="margin:1px;" align="left"></span></td>
          <td width="1000" valign="middle" style="border-bottom: silver 1px dashed">
          <span style={style_str+"#AA7744"}> {Unparsed(talk.message.is)} </span></td></tr>)
        else
          NodeSeq.Empty

      case MTypeEnum.TALK_FOX      =>
        if (((user != null) && (!user.test_memoryloss(room, room_day, user_entrys)) && (!(user.test_fake(room_day)))
             && ((user_entry.id.is == user.id.is) || ((user.role.is.substring(0,1) == RoleEnum.BETRAYER.toString) || (user.role.is.substring(0,1) == RoleEnum.GODFAT.toString) ||
                 (user.subrole.is == SubroleEnum.FOXBELIEVER.toString))
             && (user.subrole.is != SubroleEnum.WOLFBELIEVER.toString))) || (heaven_mode))
          Seq(<tr><td width="200" align="left" valign="middle" style="border-bottom: silver 1px dashed;color:#CC0099;background-color:#000030">
          <font color={user_icon.color.is}>◆</font>{user_entry.handle_name.is}<small>(妖狐)</small></td>
          <td><span style="margin:1px;" align="left"></span></td>
          <td width="1000" valign="middle" style="border-bottom: silver 1px dashed;background-color:#000030">
          <span style={style_str+"#CC0099"}> {Unparsed(talk.message.is)} </span></td></tr>)
        else
          NodeSeq.Empty

      case MTypeEnum.TALK_PONTIFF      =>
        if (((user != null) && (!user.test_memoryloss(room, room_day, user_entrys))
             && ((user_entry.id.is == user.id.is) || (user.subrole.is == SubroleEnum.SUBPONTIFF.toString))) || (heaven_mode))
          Seq(<tr><td width="200" align="left" valign="middle" style="border-bottom: silver 1px dashed;color:#EEAA55;background-color:#000030">
          <font color={user_icon.color.is}>◆</font>{user_entry.handle_name.is}<small>(教主)</small></td>
          <td><span style="margin:1px;" align="left"></span></td>
          <td width="1000" valign="middle" style="border-bottom: silver 1px dashed;background-color:#000030">
          <span style={style_str+"#EEAA55"}> {Unparsed(talk.message.is)} </span></td></tr>)
        else
          NodeSeq.Empty

      case MTypeEnum.TALK_LOVER      =>
        if (((user != null) &&  (user.has_flag(UserEntryFlagEnum.LOVER))) || (heaven_mode))
          Seq(<tr><td width="200" align="left" valign="middle" style="border-bottom: silver 1px dashed;color:#FF69B4;background-color:#000030">
          <font color={user_icon.color.is}>◆</font>{user_entry.handle_name.is}<small>(戀人)</small></td>
          <td><span style="margin:1px;" align="left"></span></td>
          <td width="1000" valign="middle" style="border-bottom: silver 1px dashed;background-color:#000030">
          <span style={style_str+"#FF69B4"}> {Unparsed(talk.message.is)} </span></td></tr>)
        else
          NodeSeq.Empty

      case xs                      => 
        NodeSeq.Empty    
    }  
  }
  
  def simple_message_tag(message: String) : NodeSeq= {
    Seq(<tr><td width="1000" colspan="3" align="left" style="background-color:#efefef;color:black;font-weight:bold;border-top: silver 1px dashed;">　　　　　{message} </td></tr>)  
  }
  
  def simple_message_tag(message: String, heaven_mode: Boolean, background_color:String, color:String) : NodeSeq= {
    val style_str = "background-color:" + background_color + ";color:" + color + ";font-weight:bold;border-top: silver 1px dashed;"  
  
    if (heaven_mode)
      Seq(<tr><td width="1000" colspan="3" align="left" style={style_str}>　　　　　　　　　　　　{message} </td></tr>)
    else
      NodeSeq.Empty  
  }

  def get_user_entry(user_entry_list : List[UserEntry], user_id : Long) : UserEntry = {
    // , hash:scala.collection.mutable.Map[Long, UserEntry]
    if( user_entry_list.length == 0) {
      if (user_id != 0) {
        /*
        if (hash != null) {
          if (!hash.contains(user_id))
            hash(user_id) = UserEntry.findAll(By(UserEntry.id, user_id))(0)
          hash(user_id)
        } else
          UserEntry.findAll(By(UserEntry.id, user_id))(0)
        */
        TalkIdCache.getOr(user_id) { () =>
          UserEntry.findAll(By(UserEntry.id, user_id))(0)
        }
      }
      else
        null
    }
    else
       user_entry_list(0)
  }

  def talk_tag(room:Room, room_day:RoomDay, talk: Talk, user: UserEntry, heaven_mode: Boolean, user_entrys:List[UserEntry]): NodeSeq = {
    val mtype : MTypeEnum.Value = MTypeEnum.valueOf(talk.mtype.is) getOrElse(null)
    val user_entry_list  = user_entrys.filter(_.id.is == talk.actioner_id.is)
    val user_target_list = user_entrys.filter(_.id.is == talk.actionee_id.is)

    var user_entry  : UserEntry = null
    var user_target : UserEntry = null

    //println("user_entry_list length : " +user_entry_list.length)
    var generated_message : String = ""

    //println("user_target_list length : " +user_target_list.length)
    if ((mtype == MTypeEnum.MESSAGE_COME) || (mtype == MTypeEnum.MESSAGE_LEAVE) || (mtype == MTypeEnum.MESSAGE_KICKED)) {
      var handle_name : String = ""
      if ((talk.message.is == null) || (talk.message.is == "")) {
        user_entry  = get_user_entry(user_entry_list, talk.actioner_id.is)
        handle_name = user_entry.handle_name.is
      } else
        handle_name = talk.message.is

      generated_message = handle_name + (mtype match {
        case MTypeEnum.MESSAGE_COME      => " 來到村莊大廳"
        case MTypeEnum.MESSAGE_LEAVE     => " 離開這個村莊了"
        case MTypeEnum.MESSAGE_KICKED    => " 人間蒸發、被轉學了"
        case xs                          => ""
      })
    } else if (mtype == MTypeEnum.VOTE_KICK) {
      if ((talk.message.is == null) || (talk.message.is == "")){
        user_entry  = get_user_entry(user_entry_list, talk.actioner_id.is)
        user_target = get_user_entry(user_target_list, talk.actionee_id.is)
        val user_entry_handle_name = (if (user_entry != null) user_entry.handle_name.is else "")
        val user_target_handle_name = (if (user_target != null) user_target.handle_name.is else "")
        generated_message = user_entry_handle_name + " 對 " + user_target_handle_name + " 投票踢出"
      } else
        generated_message = talk.message.is
    } else {
      user_entry  = get_user_entry(user_entry_list, talk.actioner_id.is)
      user_target = get_user_entry(user_target_list, talk.actionee_id.is)
    }

    mtype match {
      case MTypeEnum.TALK_ADMIN         => simple_talk_tag(room, room_day, talk, user, heaven_mode, user_entrys)
      case MTypeEnum.TALK_ADMIN_PRIVATE => simple_talk_tag(room, room_day, talk, user, heaven_mode, user_entrys)

      case MTypeEnum.TALK_DAY        => simple_talk_tag(talk, user_entrys, false)
      case MTypeEnum.TALK_DAY_FOG    => fog_talk_tag(talk, user, heaven_mode, user_entrys)
      case MTypeEnum.TALK_GEMINI_DAY => simple_talk_tag(room, room_day, talk, user, heaven_mode, user_entrys)
      case MTypeEnum.TALK_SECRET     => secret_talk_tag(room, room_day, talk, user, heaven_mode, user_entrys)
      case MTypeEnum.TALK_VENTRILOQUIST => ventriloquist_talk_tag(room, room_day, talk, user, heaven_mode, user_entrys)
      case MTypeEnum.TALK_DISGUISED  => disguised_talk_tag(talk, user, heaven_mode, user_entrys)
      case MTypeEnum.TALK_SEALED     => sealed_talk_tag(talk, user, heaven_mode, user_entrys)
      case MTypeEnum.TALK_NIGHT      => simple_talk_tag(room, room_day, talk, user, heaven_mode, user_entrys)
      case MTypeEnum.TALK_WEREWOLF   => simple_talk_tag(room, room_day, talk, user, heaven_mode, user_entrys)
      case MTypeEnum.TALK_GEMINI     => simple_talk_tag(room, room_day, talk, user, heaven_mode, user_entrys)
      case MTypeEnum.TALK_FOX        => simple_talk_tag(room, room_day, talk, user, heaven_mode, user_entrys)
      case MTypeEnum.TALK_PONTIFF    => simple_talk_tag(room, room_day, talk, user, heaven_mode, user_entrys)
      case MTypeEnum.TALK_LOVER      => simple_talk_tag(room, room_day, talk, user, heaven_mode, user_entrys)
      
      case MTypeEnum.MESSAGE_GENERAL   => simple_message_tag(talk.message.is)
      case MTypeEnum.MESSAGE_COME      => simple_message_tag(generated_message)
      case MTypeEnum.MESSAGE_LEAVE     => simple_message_tag(generated_message)
      case MTypeEnum.MESSAGE_KICKED    => simple_message_tag(generated_message)
      case MTypeEnum.MESSAGE_REVOTE0   => simple_message_tag("＜投票重新開始 請儘速重新投票＞")
      case MTypeEnum.MESSAGE_LAST2MIN  => simple_message_tag("最後 2 分還不投票將會暴斃")
      case MTypeEnum.MESSAGE_DEATHSUDDEN => simple_message_tag(user_entry.handle_name.is + "  突然暴斃死亡")
      case MTypeEnum.MESSAGE_REVOTE    => simple_message_tag("＜投票結果有問題 請重新投票＞")
      case MTypeEnum.MESSAGE_NIGHT     => simple_message_tag("< < 日落、黑暗的夜晚來臨 > >")
      case MTypeEnum.MESSAGE_EVIL      =>
          if (((user != null) && (RoleEnum.get_role(user.role.is.substring(0,1)).role_side != RoomVictoryEnum.VILLAGER_WIN) &&
               (!user.test_memoryloss(room, room_day, user_entrys)) && (!user.test_fake(room_day))) || (heaven_mode))
            simple_message_tag(talk.message.is)
          else
            NodeSeq.Empty
      case MTypeEnum.MESSAGE_FOX      =>
          if ((user != null) && (user.test_foxside(room, room_day, user_entrys)) || (heaven_mode))
            simple_message_tag(talk.message.is)
          else
            NodeSeq.Empty
      case MTypeEnum.MESSAGE_GENERAL_NORUNNER      =>
          if (((user != null) && (user.current_role != RoleEnum.RUNNER))|| (heaven_mode))
            simple_message_tag(talk.message.is)
          else
            NodeSeq.Empty
      case MTypeEnum.OBJECTION_MALE                =>
          Seq(<tr><td width="1000" colspan="3" align="left" style="background-color:#336699;color:white;font-weight:bold;border-top: silver 1px dashed;">　　　　　{user_entry.handle_name.is} 要求廢村</td></tr>)
      case MTypeEnum.OBJECTION_FEMALE              =>
          Seq(<tr><td width="1000" colspan="3" align="left" style="background-color:#FF0099;color:white;font-weight:bold;border-top: silver 1px dashed;">　　　　　{user_entry.handle_name.is} 要求廢村</td></tr>)

      case MTypeEnum.VOTE_KICK             => simple_message_tag(generated_message,true,"#AAAA33","snow")
      case MTypeEnum.VOTE_HANG             => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 投票處死",heaven_mode || ((user != null) && (!user.live.is)),"#AAAA33","snow")

      case MTypeEnum.ITEM_UNLUCKY_PURSE    => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 使用不運錢包",heaven_mode,"#AAAA33","snow")
      case MTypeEnum.ITEM_BLESS_STAFF      => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 使用祝福之杖",heaven_mode,"#AAAA33","snow")
      case MTypeEnum.ITEM_BLACK_FEATHER    => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 使用咒縛黑羽",heaven_mode,"#AAAA33","snow")
      case MTypeEnum.ITEM_THIEF_SECRET     => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 使用盜賊極意",heaven_mode,"#AAAA33","snow")
      case MTypeEnum.ITEM_VENTRILOQUIST    => simple_message_tag(user_entry.handle_name.is + " 使用腹語娃娃",heaven_mode,"#AAAA33","snow")
      case MTypeEnum.ITEM_DMESSAGE_SEAL    => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 使用封印遺書",heaven_mode,"#AAAA33","snow")
      case MTypeEnum.ITEM_MIRROR_SHIELD    => simple_message_tag(user_entry.handle_name.is + " 使用鏡盾捲軸",heaven_mode,"#AAAA33","snow")
      case MTypeEnum.ITEM_SHAMAN_CROWN     => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 使用薩滿冕冠",heaven_mode,"#AAAA33","snow")
      case MTypeEnum.ITEM_WEATHER_ROD      => simple_message_tag(user_entry.handle_name.is + " 使用天候棒 (" + talk.message.is + ")" ,heaven_mode ,"#AAAA33","snow")
      case MTypeEnum.ITEM_DEATH_NOTE       => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 使用死亡筆記",heaven_mode,"#AAAA33","snow")
      case MTypeEnum.ITEM_CUBIC_ARROW      => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 使用邱比特之箭",heaven_mode,"#AAAA33","snow")
      case MTypeEnum.ITEM_POPULATION_CENSUS => simple_message_tag(user_entry.handle_name.is + " 使用人口普查",heaven_mode,"#AAAA33","snow")

      case MTypeEnum.VOTE_VILLAGER         => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 進行推理",heaven_mode,"#AAAA33","black")
      case MTypeEnum.VOTE_BECOMEMOB        => simple_message_tag(user_entry.handle_name.is + " 進入暴民模式",heaven_mode,"#AAAAAA","snow")

      case MTypeEnum.VOTE_HIDE             => simple_message_tag(user_entry.handle_name.is + " 神隱了",heaven_mode,"#888888","snow")
      case MTypeEnum.VOTE_REVERSEVOTE      => simple_message_tag(user_entry.handle_name.is + " 施行逆轉投票",heaven_mode,"#888888","snow")

      case MTypeEnum.VOTE_AUGURER          => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 占卜",heaven_mode,"#9933FF","snow")
      case MTypeEnum.VOTE_HUNTER           => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 進行護衛",heaven_mode,"#3399FF","snow")
      case MTypeEnum.VOTE_CLERIC_BLESS     => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 祝福",heaven_mode,"#CCDD00","blue")
      case MTypeEnum.VOTE_CLERIC_SANCTURE  => simple_message_tag(user_entry.handle_name.is + " 施行聖域術",heaven_mode,"#CCDD00","blue")
      
      case MTypeEnum.VOTE_HERBALIST_ELIXIR => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 使用治療藥",heaven_mode,"#8FCECE","snow")
      case MTypeEnum.VOTE_HERBALIST_POISON => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 使用毒藥",heaven_mode,"#8FCECE","snow")
      case MTypeEnum.VOTE_HERBALIST_MIX    => simple_message_tag(user_entry.handle_name.is + " 調製藥品",heaven_mode,"#8FCECE","snow")
      case MTypeEnum.VOTE_HERBALIST_DROP   => simple_message_tag(user_entry.handle_name.is + " 丟棄藥品",heaven_mode,"#8FCECE","snow")
      case MTypeEnum.VOTE_ALCHEMIST_ELIXIR => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 使用治療藥",heaven_mode,"#8FCECE","snow")
      case MTypeEnum.VOTE_ALCHEMIST_POISON => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 使用毒藥",heaven_mode,"#8FCECE","snow")

      case MTypeEnum.VOTE_RUNNER           => simple_message_tag(user_entry.handle_name.is + " 逃亡至 " + user_target.handle_name.is + " 處",heaven_mode,"#009999","snow")
      case MTypeEnum.VOTE_SCHOLAR_EXAMINE  => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 展開調查",heaven_mode,"#3CB371","snow")
      case MTypeEnum.VOTE_SCHOLAR_EXAMINE2 => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 展開強力調查",heaven_mode,"#3CB371","snow")
      case MTypeEnum.VOTE_SCHOLAR_ANALYZE  => simple_message_tag(user_entry.handle_name.is + " 對前一天晚上進行事件分析",heaven_mode,"#3CB371","snow")
      case MTypeEnum.VOTE_SCHOLAR_REPORT   => simple_message_tag(user_entry.handle_name.is + " 對現況進行瞭解",heaven_mode,"#3CB371","snow")
      case MTypeEnum.VOTE_ARCHMAGE_DISPELL => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 施行解除魔法",heaven_mode,"#7B68EE","snow")
      case MTypeEnum.VOTE_ARCHMAGE_SUMMON  => simple_message_tag(user_entry.handle_name.is + " 召喚水元素",heaven_mode,"#7B68EE","snow")

      case MTypeEnum.VOTE_WEREWOLF         => simple_message_tag(user_entry.handle_name.is + " 人狼對 " + user_target.handle_name.is + " 為鎖定目標",heaven_mode,"#FF0000","snow")
      case MTypeEnum.VOTE_WOLFCUB          => simple_message_tag(user_entry.handle_name.is + " 幼狼對 " + user_target.handle_name.is + " 為鎖定目標",heaven_mode,"#EE0000","snow")
      case MTypeEnum.VOTE_MADMAN_STUN1     => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 擊昏１",heaven_mode,"#DD0000","snow")
      case MTypeEnum.VOTE_MADMAN_STUN3     => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 擊昏３",heaven_mode,"#DD0000","snow")
      case MTypeEnum.VOTE_MADMAN_STUN      => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 擊忘",heaven_mode,"#DD0000","snow")
      case MTypeEnum.VOTE_MADMAN_SUICIDE   => simple_message_tag(user_entry.handle_name.is + " 進行自爆",heaven_mode,"#DD0000","snow")
      case MTypeEnum.VOTE_MADMAN_DUEL      => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 單挑",heaven_mode,"#DD0000","snow")
      case MTypeEnum.VOTE_SORCEROR_AUGURE  => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 占卜",heaven_mode,"#CC0000","snow")
      case MTypeEnum.VOTE_SORCEROR_CONJURE => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 咒殺",heaven_mode,"#CC0000","snow")
      case MTypeEnum.VOTE_SORCEROR_WHISPER => simple_message_tag(user_entry.handle_name.is + " 施放密語術",heaven_mode,"#CC0000","snow")
      case MTypeEnum.VOTE_SORCEROR_SHOUT   => simple_message_tag(user_entry.handle_name.is + " 施放鼓舞術",heaven_mode,"#CC0000","snow")
      case MTypeEnum.VOTE_SORCEROR_BELIEVE => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 施放狼信化",heaven_mode,"#CC0000","snow")
      case MTypeEnum.VOTE_SORCEROR_SEAR    => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 施放灼熱",heaven_mode,"#CC0000","snow")
      case MTypeEnum.VOTE_SORCEROR_SUMMON  => simple_message_tag(user_entry.handle_name.is + " 召喚狼元素",heaven_mode,"#CC0000","snow")
      
      case MTypeEnum.VOTE_FOX              => simple_message_tag(user_entry.handle_name.is + " 妖狐對 " + user_target.handle_name.is + " 為鎖定目標",heaven_mode,"#CC0099","snow")
      case MTypeEnum.VOTE_FOX1             => simple_message_tag(user_entry.handle_name.is + " 妖狐對 " + user_target.handle_name.is + " 為鎖定目標且施展結界",heaven_mode,"#CC0099","snow")
      case MTypeEnum.VOTE_FOX2             => simple_message_tag(user_entry.handle_name.is + " 妖狐施展結界",heaven_mode,"#CC0099","snow")
      case MTypeEnum.VOTE_BETRAYER_DISGUISE=> simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 進行偽裝",heaven_mode,"#DD0088","snow")
      case MTypeEnum.VOTE_BETRAYER_CHANGE  => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 進行變身",heaven_mode,"#DD0088","snow")
      case MTypeEnum.VOTE_BETRAYER_FOG     => simple_message_tag(user_entry.handle_name.is + " 施展粉紅迷霧",heaven_mode,"#DD0088","snow")

      case MTypeEnum.VOTE_GODFAT_SPECIAL1  => simple_message_tag(user_entry.handle_name.is + " 進行咒術特化",heaven_mode,"#BB00AA","snow")
      case MTypeEnum.VOTE_GODFAT_SPECIAL2  => simple_message_tag(user_entry.handle_name.is + " 進行方陣特化",heaven_mode,"#BB00AA","snow")
      case MTypeEnum.VOTE_GODFAT_SPECIAL3  => simple_message_tag(user_entry.handle_name.is + " 進行秘術特化",heaven_mode,"#BB00AA","snow")
      case MTypeEnum.VOTE_GODFAT_SPECIAL4  => simple_message_tag(user_entry.handle_name.is + " 進行預言特化",heaven_mode,"#BB00AA","snow")
      case MTypeEnum.VOTE_GODFAT_DEATHGAZE  => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 使用絕望視線",heaven_mode,"#BB00AA","snow")
      case MTypeEnum.VOTE_GODFAT_HELLWORD   => simple_message_tag(user_entry.handle_name.is + " 使用言咒",heaven_mode,"#BB00AA","snow")
      case MTypeEnum.VOTE_GODFAT_COLORSPRAY  => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 使用七彩噴射",heaven_mode,"#BB00AA","snow")
      case MTypeEnum.VOTE_GODFAT_BLIND      => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 使用眩光",heaven_mode,"#BB00AA","snow")
      case MTypeEnum.VOTE_GODFAT_BLIND2    => simple_message_tag(user_entry.handle_name.is + " 使用眩光",heaven_mode,"#BB00AA","snow")
      case MTypeEnum.VOTE_GODFAT_EXCHANGE  => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 使用秘術換身",heaven_mode,"#BB00AA","snow")
      case MTypeEnum.VOTE_GODFAT_NECROMANCER  => simple_message_tag(user_entry.handle_name.is + " 預言 " + user_target.handle_name.is + " 為靈能者",heaven_mode,"#BB00AA","snow")
      case MTypeEnum.VOTE_GODFAT_HUNTER  => simple_message_tag(user_entry.handle_name.is + " 預言 " + user_target.handle_name.is + " 為獵人",heaven_mode,"#BB00AA","snow")
      case MTypeEnum.VOTE_GODFAT_HERBALIST  => simple_message_tag(user_entry.handle_name.is + " 預言 " + user_target.handle_name.is + " 為藥師",heaven_mode,"#BB00AA","snow")
      case MTypeEnum.VOTE_GODFAT_POISONER  => simple_message_tag(user_entry.handle_name.is + " 預言 " + user_target.handle_name.is + " 為埋毒者",heaven_mode,"#BB00AA","snow")
      case MTypeEnum.VOTE_GODFAT_SCHOLAR  => simple_message_tag(user_entry.handle_name.is + " 預言 " + user_target.handle_name.is + " 為學者",heaven_mode,"#BB00AA","snow")

      case MTypeEnum.VOTE_DEMON_CHAOS      => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 使用混沌術",heaven_mode,"#666666","#FF0000")
      case MTypeEnum.VOTE_DEMON_DOMINATE   => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 支配",heaven_mode,"#666666","#FF0000")
      case MTypeEnum.VOTE_DEMON_CURSE      => simple_message_tag(user_entry.handle_name.is + " 使用詛咒術",heaven_mode,"#666666","#FF0000")
      case MTypeEnum.VOTE_DEMON_CURSE2     => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is +" 使用詛咒術",heaven_mode,"#666666","#FF0000")
      case MTypeEnum.VOTE_DEMON_VORTEX     => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is +" 使用斗轉星移",heaven_mode,"#666666","#FF0000")
      case MTypeEnum.VOTE_FALLENANGEL_FALLEN => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is +" 使用墮落",heaven_mode,"#666666","#FF0000")

      case MTypeEnum.VOTE_PENGUIN_ICE     => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is +" 冰凍",heaven_mode,"#CCFFFF","#0000FF")
      case MTypeEnum.VOTE_PENGUIN_CHILL   => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is +" 冰凍且圍繞著寒冰",heaven_mode,"#CCFFFF","#0000FF")

      case MTypeEnum.VOTE_PONTIFF          => simple_message_tag(user_entry.handle_name.is + " 拉 " + user_target.handle_name.is + " 入教",heaven_mode,"#EEAA55","snow")
      case MTypeEnum.VOTE_PONTIFF_COMMAND  => simple_message_tag(user_entry.handle_name.is + " 指定 " + user_target.handle_name.is + " 為投票對象",heaven_mode,"#EEAA55","snow")
      case MTypeEnum.VOTE_PONTIFF_AURA     => simple_message_tag(user_entry.handle_name.is + " 的身邊突然圍繞起光環",heaven_mode,"#EEAA55","snow")

      case MTypeEnum.VOTE_SHIFTER          => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 模仿",heaven_mode,"#FF7700","snow")
      case MTypeEnum.VOTE_SHIFTER2         => simple_message_tag(user_entry.handle_name.is + " 模仿惡魔",heaven_mode,"#FF7700","snow")

      case MTypeEnum.VOTE_INHERITER        => simple_message_tag(user_entry.handle_name.is + " 準備繼承 " + user_target.handle_name.is, heaven_mode,"#AAAA00","snow")

      case MTypeEnum.VOTE_CARD_FOOL        => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 使用愚者卡",heaven_mode,"#DAA520","snow")
      case MTypeEnum.VOTE_CARD_MAGICIAN    => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 使用魔術師卡",heaven_mode,"#DAA520","snow")
      case MTypeEnum.VOTE_CARD_CHARIOT     => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 使用戰車卡",heaven_mode,"#DAA520","snow")
      case MTypeEnum.VOTE_CARD_HERMIT      => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 使用隱者卡",heaven_mode,"#DAA520","snow")
      case MTypeEnum.VOTE_CARD_STRENGTH    => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 使用力卡",heaven_mode,"#DAA520","snow")
      case MTypeEnum.VOTE_CARD_JUSTICE     => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 使用正義卡",heaven_mode,"#DAA520","snow")
      case MTypeEnum.VOTE_CARD_TOWER       => simple_message_tag(user_entry.handle_name.is + " 對 " + user_target.handle_name.is + " 使用塔卡",heaven_mode,"#DAA520","snow")
      case MTypeEnum.VOTE_CARD_SUN         => simple_message_tag(user_entry.handle_name.is + " 使用太陽卡",heaven_mode,"#DAA520","snow")

      case MTypeEnum.VOTE_NO_ACTION        => simple_message_tag(user_entry.handle_name.is + " 放棄行動",heaven_mode,"#AAAA33","snow")
      
      case xs => NodeSeq.Empty
    }
  }

  def heaven_talk_tag(room:Room, talk: Talk, user: UserEntry, user_entrys:List[UserEntry]): NodeSeq = {
    val mtype : MTypeEnum.Value = MTypeEnum.valueOf(talk.mtype.is) getOrElse(null)
    mtype match {
      case MTypeEnum.TALK_HEAVEN   => simple_talk_tag(talk, user_entrys, true)
      case xs => NodeSeq.Empty
    }
  }

  def gameend_talk_tag(room:Room, talk: Talk, user: UserEntry, user_entrys:List[UserEntry]): NodeSeq = {
    val mtype : MTypeEnum.Value = MTypeEnum.valueOf(talk.mtype.is) getOrElse(null)
    mtype match {
      case MTypeEnum.TALK_END      => simple_talk_tag(talk, user_entrys, false)
      case xs => NodeSeq.Empty
    }
  }
  
  // Message Table
  def messages_normal(room:Room, room_day:RoomDay, user_entry:UserEntry, heaven_mode:Boolean, blind_mode:Boolean, user_entrys:List[UserEntry]) : NodeSeq = {
    val hash         =
      if (room_day.day_no.is == 0 )
        scala.collection.mutable.Map[Long, UserEntry]()
      else
        null
    var talks        =  Talk.findAll(By(Talk.roomday_id, room_day.id.is), OrderBy(Talk.id, Descending))
    if (room_day.day_no.is == 0) {
      val revotes = talks.filter(_.mtype.is == MTypeEnum.MESSAGE_REVOTE0.toString)

      // 新增 投票重新開始 50 次時廢村
      if ((revotes.length >= 50) || (talks.length >= 1000)) {
         room.status(RoomStatusEnum.ENDED.toString)
         room.victory(RoomVictoryEnum.ABANDONED.toString)
         room.save
      }
    }
    if (blind_mode) {
      val user_entry_id : Long =
        if (user_entry == null)
          0
        else
          (user_entry.id.is)

      if (user_entry_id == 0) 
        talks = List()
      else {
        val live_users = user_entrys.filter(x => (x.live.is) && (x.id.is != user_entry_id))
        talks = talks.filter(x=> ((x.mtype.is != MTypeEnum.TALK_DAY.toString) && (x.mtype.is != MTypeEnum.TALK_DAY_FOG.toString)) ||
                                  (user_entry_id == x.actioner_id.is) ||
                                  (user_entry_id % 5 != x.id.is % 5)) // == x.actioner_id.is % 2))
        talks.foreach { talk =>
          if (((talk.mtype.is != MTypeEnum.TALK_DAY.toString) || (talk.mtype.is != MTypeEnum.TALK_DAY_FOG.toString)) &&
              (user_entry_id != talk.actioner_id.is) && (user_entry_id % 3 == talk.id.is % 3)) {
            val new_user =  live_users(((talk.id.is + user_entry_id) % live_users.length).asInstanceOf[Int])
            talk.actioner_id(new_user.id.is)
            talk
          }
        }
      }
    }
    
    Seq(<table border="0" cellpadding="0" cellspacing="0" style="font-family:新細明體;"> {
        for (val talk <- talks) yield talk_tag(room, room_day, talk, user_entry, heaven_mode, user_entrys)
      } </table>)
  }

  // Message Table
  def messages_heaven(room:Room, room_day:RoomDay, user_entry:UserEntry, user_entrys:List[UserEntry]) : NodeSeq = {
    //val talks        =  Talk.findAll(By(Talk.roomday_id, room_day.id.is), OrderBy(Talk.id, Descending))

    //val room_days    =  RoomDay.findAll(By(RoomDay.room_id, room.id.is), OrderBy(RoomDay.id, Descending))
    //val talks        =  Talk.findAll(ByList(Talk.roomday_id, room_days.map(_.id.is)), OrderBy(Talk.id, Descending))

    val talks        =  Talk.findAllByPreparedStatement({ superconn =>
      val statement = superconn.connection.prepareStatement(
      "select * from Talk join RoomDay on Talk.roomday_id = RoomDay.id where RoomDay.room_id = ? and Talk.mtype = 'TH' order by Talk.id desc")
      statement.setString(1, room.id.is.toString)
      statement
    }) // , IHaveValidatedThisSQL("plummtw","20090530")

    Seq(<table border="0" cellpadding="0" cellspacing="0" style="font-family:新細明體;"> {
        for (val talk <- talks) yield heaven_talk_tag(room, talk, user_entry, user_entrys)
      } </table>)
  }

  def messages_gameend(room:Room, room_day:RoomDay, user_entry:UserEntry, user_entrys:List[UserEntry]) : NodeSeq = {
    val talks        =  Talk.findAll(By(Talk.roomday_id, room_day.id.is), OrderBy(Talk.id, Descending))

    Seq(<table border="0" cellpadding="0" cellspacing="0" style="font-family:新細明體;"> {
        for (val talk <- talks) yield gameend_talk_tag(room, talk, user_entry, user_entrys)
      } </table>)
  }

  def messages_all(room:Room, room_day:RoomDay, user_entry:UserEntry, user_entrys:List[UserEntry]) : NodeSeq = {
    val hash         =
      if (room_day.day_no.is == 0 )
        scala.collection.mutable.Map[Long, UserEntry]()
      else
        null
    val talks        =  Talk.findAll(By(Talk.roomday_id, room_day.id.is), OrderBy(Talk.id, Descending))

    Seq(<table border="0" cellpadding="0" cellspacing="0" style="font-family:新細明體;"> {
       for (val talk <- talks) yield 
         if (talk.mtype.is == MTypeEnum.TALK_END.toString) gameend_talk_tag(room, talk, user_entry, user_entrys)
         else if (talk.mtype.is == MTypeEnum.TALK_HEAVEN.toString) heaven_talk_tag(room, talk, user_entry, user_entrys)
         else talk_tag(room, room_day, talk, user_entry, true, user_entrys)
      } </table>)
  }
}