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

object MiscHelper {

  def get_lastwords_tag(system_messages: List[SystemMessage], user_entrys: List[UserEntry]) = {
    var result : NodeSeq = NodeSeq.Empty
    system_messages.foreach { system_message =>
      val actioner_list = user_entrys.filter(_.id.is == system_message.actioner_id.is)
      val actioner =
        if (actioner_list.length != 0)
          actioner_list(0)
        else
          null
      if ((actioner != null) && (actioner.last_words.is != "") && (actioner.current_role != RoleEnum.RUNNER) &&
          (actioner.hasnt_flag(UserEntryFlagEnum.DMESSAGE_SEALED))) {
        if (result == NodeSeq.Empty)
          result = result ++ Seq(<table border="0" cellpadding="0" cellspacing="0" width="100%">
                                   <tr style="background-color:#ccddff;color:black;font-weight:bold;">
                                   <td valign="middle" align="left" colspan="3" width="100%" style="background-color:#ccddff;color:black;font-weight:bold;">　　　　　　　　　　　・早上發現死者的遺書</td>
                                 </tr></table>)
        result = result ++ Seq(<table border="0" cellpadding="0" cellspacing="0"><tr style="background-color:#eeeeff;">
                     <td width="140" align="left" valign="middle" style="color:black;border-top: silver 1px dashed;">{actioner.handle_name.is} <small>的遺言</small></td>
                     <td><span style="margin:1px;" align="left"></span></td>
                     <td valign="middle" style="border-top: silver 1px dashed;">
                       <table><td width="1000" style="color:black;"><span>{Unparsed(actioner.last_words.is)} </span>
                     </td></table></td>
                     </tr></table>)
      }
    }
    result
  }

  

  def get_dead_tag(system_messages: List[SystemMessage], user_entrys: List[UserEntry]) = {
    var result : NodeSeq= NodeSeq.Empty
    system_messages.foreach { system_message =>
      val actioner_list = user_entrys.filter(_.id.is == system_message.actioner_id.is)
      val actioner_name =
        if (actioner_list.length != 0)
          <span>{actioner_list(0).handle_name.is}</span>
        else
          <font color="#1E90FF">水元素</font>
        
      val (death_message, message_color) =
        if (system_message.mtype.is == MTypeEnum.DEATH_HANGED.toString)
          ("被表決處死", "#666600")
        else if (system_message.mtype.is == MTypeEnum.DEATH_SUDDEN.toString)
          ("暴斃死亡",   "#990000")
        else if (system_message.mtype.is == MTypeEnum.DEATH_PENGUIN_ICE.toString)
          ("被冰凍成冰棒",   "#AADDDD")
        else if (system_message.mtype.is == MTypeEnum.DEATH_WOLFCUB_EATEN.toString)
          ("有點慘的死狀被發現", "#990000")
        else
          ("悽慘的死狀被發現", "#990000")

      result = result ++ Seq(<table border="0" cellpadding="0" cellspacing="5" width="100%">
                   <tr height="35" style={"background-color:snow;color:" + message_color + ";font-weight:bold;"}>
                   <td valign="middle" align="left" width="100%">　　　　　　　　　　　　{actioner_name} {death_message}</td>
                   </tr></table>)
    }
    result
  }

  def get_self_lastwords_tag(user: UserEntry) = {
   if ((user != null) && (user.last_words.is != "") && (user.live.is))
     Seq(<table border="0" cellpadding="0" cellspacing="5" width="100%">
      <tr style="color:black;background-color:#ddeeff;">
      <td valign="middle" align="right" width="140">自己的遺言</td>
      <td valign="top" align="left"> {Unparsed(user.last_words.is)} </td></tr></table>)
   else
     NodeSeq.Empty
  }

  /*

  public static generateActionString(votes) {
    def result = ""
    def death_message = ""

    votes.each {
      if (it.type == 'DH')
        death_message = '被表決處死'
      else
        death_message = '悽慘的死狀被發現'

      result += """<table border=0 cellpadding=0 cellspacing=5 width=100%>
                   <tr height=35 style="background-color:snow;color:#990000;font-weight:bold;">
                   <td valign=middle align=left width=100%>　　　　　　　　　　　　${it.actioner.handle_name} ${death_message}</td>
                   </tr></table>"""
    }
    return result
  }
  */

}