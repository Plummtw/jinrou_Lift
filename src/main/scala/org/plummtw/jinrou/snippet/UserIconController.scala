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
import org.plummtw.jinrou.util._

class UserIconController {
  def list : NodeSeq = {
    var result : NodeSeq = Seq()
    val icon_list  = UserIcon.findAll(By(UserIcon.icon_group, 1)) ++
                     UserIcon.findAll(By(UserIcon.icon_group, 2))
    val icon_groups = JinrouUtil.zipListBySize(5)(icon_list)
    
    for (val icon_group <- icon_groups) result ++= Seq(<tr> { 
       for (val icon <- icon_group) yield
         <td valign="top"><img src={icon.icon_filename.is} width={icon.icon_width.is.toString} height={icon.icon_height.is.toString} border="2" style={"border-color:" + icon.color.is}/></td>
         <td width="150px">{icon.icon_name.is}<br/><font color={icon.color.is}>◆</font><span style="font-family:新細明體;">{icon.color.is}</span></td>
    } </tr>)
    
    return result
  }

}

/*
<tr>
      <% def icon_index = 0
         iconInstanceList.each { icon ->
           if (icon_index % 5 == 0) { %>
      </tr>
      <tr>
      <%   } %>
       <td valign=top><img src=${icon.icon_filename} width=${icon.icon_width} height=${icon.icon_height} border=2 style="border-color:${icon.color};"><td>
       <td width=150px>${icon.icon_name}<br><font color=${icon.color}>◆</font><span style="font-family:新細明體;">${icon.color}</span></td>
      <% icon_index += 1
         } %>
      </tr>
*/