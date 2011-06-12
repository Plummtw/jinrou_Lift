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
import org.plummtw.jinrou.util._

import java.security._
//import sun.misc.BASE64Encoder
import org.apache.commons.codec.digest.DigestUtils
import org.apache.commons.codec.binary.Base64

object JinrouUtil {
  def zipListBySize[T](size: Int)(list: List[T]) : List[List[T]] = { 
    var (first, second) = list.splitAt(size) 
    if (second == Nil) 
      List(first) 
    else 
      List(first) ::: zipListBySize(size)(second) 
  }

  /*
  def  encodeHtml(string: String) : String = {
    // .replaceAll("\'","&apos;")
    string.replaceAll("&","&amp;").replaceAll("<","&lt;").replaceAll(">","&gt;")
          .replaceAll("\"","&quot;").
           replaceAll("\r\n","\n").replaceAll("\r","\n").replaceAll("\n","<br/>")
  } */
  val html_encode_hash = Map (
    '&' -> "&amp;",
    '<' -> "&lt;",
    '>' -> "&gt;",
    '"' -> "&quot;",
    '\r' -> "<br/>",
    '\n' -> "<br/>"
  )

  val april_fool_hash = Map[Any, Any] (
    '真' -> '偽', '偽' -> '真',
	'人' -> '狼', '狼' -> '人',
	'白' -> '黑', '黑' -> '白',
	'○' -> '●', '●' -> '○',
	'吊' -> '咬', '咬' -> '吊',
	'男' -> '女', '女' -> '男',
  )
  val april_fool_hash2 = Map (
    "村人" -> "人狼", "人狼" -> "村人",
    "CO" -> "隱", "co" -> "隱", "ＣＯ" -> "隱", "ｃｏ" -> "隱",
    "救村" -> "滅村", "滅村" -> "救村"
  )


  def  hasHtmlCode(string: String) : Boolean = {
    val count          = string.length - 1

    for (i <- 0 to count) {
      val c = string.charAt(i)
      if (html_encode_hash.isDefinedAt(c))
        return true
    }
    return false
  }

  def encodeHtml(string: String) : String = {
    // .replaceAll("\'","&apos;")

    /*
    string.replaceAll("&","&amp;").replaceAll("<","&lt;").replaceAll(">","&gt;")
          .replaceAll("\"","&quot;").
           replaceAll("\r\n","\n").replaceAll("\r","\n").replaceAll("\n","<br/>")
    */
    val string_builder = new StringBuilder
    val count          = string.length - 1

    var is_escape_r    = false
    for (i <- 0 to count) {
      val c = string.charAt(i)
      val h = html_encode_hash.get(c).getOrElse(c)
      if (is_escape_r && (c == '\n')) {
        is_escape_r = false
      } else {
        string_builder.append(h)
        is_escape_r = ( c == '\r')
      }
    }
    return string_builder.toString
  }

  def encodeHtml_ap(string: String) : String = {
    // .replaceAll("\'","&apos;")

    /*
    string.replaceAll("&","&amp;").replaceAll("<","&lt;").replaceAll(">","&gt;")
          .replaceAll("\"","&quot;").
           replaceAll("\r\n","\n").replaceAll("\r","\n").replaceAll("\n","<br/>")
    */
    val string_builder = new StringBuilder
    val count          = string.length - 1
    var is_skip        = false

    var is_escape_r    = false
    for (i <- 0 to count) {
      if (is_skip)
        is_skip = false
      else {
        val c = string.charAt(i)
        val h = html_encode_hash.get(c).getOrElse(c)
        val ap1 = april_fool_hash.get(h).getOrElse(h)

        val ap2 = april_fool_hash2.get(string.substring(i, Math.min(count+1, i+2)))

        ap2 match {
          case Some(x) => string_builder.append(x); is_skip = true
          case _       => if (is_escape_r && (c == '\n')) {
                            is_escape_r = false
                          } else {
                            string_builder.append(ap1)
                            is_escape_r = ( c == '\r')
                          }
        }
      }
    }

    return string_builder.toString
  }
  
  def getIpAddress(request: net.liftweb.http.Req) : String = {

    val ip_from_header = request.headers.find{_._1 == "X-Forwarded-For"}

    val ip = ip_from_header match {
      case Some(a) => a._2
      case x       => "unknown"
      }
    /*
    {
      ip = request.getHeader( "Proxy-Client-IP" )
    } 
    if ((ip == null) || (ip.length() == 0) || ("unknown".equalsIgnoreCase(ip)))   {
      ip = request.getHeader( "WL-Proxy-Client-IP" )
    } 
    if ((ip == null) || (ip.length() == 0) || ("unknown".equalsIgnoreCase(ip)))   {
      ip = request.getRemoteAddr()
    }
    */
      
    return ( if (ip == "unknown") request.remoteAddr else ip )
  }

  def generateSHA1_Bytes(string: String) : Array[Byte] = {
    val sha = MessageDigest.getInstance("SHA-1")
    sha.update(string.getBytes())

    return sha.digest()
  }
  
  def generateSHA1(string: String) : String = {
    val sha = MessageDigest.getInstance("SHA-1")
    sha.update(string.getBytes())
      
    val digest = sha.digest()
    //return new BASE64Encoder().encode(digest)
    return new String(Base64.encodeBase64(digest))
  }

  def byte2string(byte: Byte) : String = {
    val posivite_byte : Int =
      if (byte >= 0)
        byte
      else
        256 + (byte.asInstanceOf[Int])
    val high_value = posivite_byte / 16
    val low_value  = posivite_byte % 16
    return high_value.toHexString + low_value.toHexString
  }

  def bytes2string(bytes: Array[Byte]) : String = {
    val result = new StringBuffer("")
    bytes.foreach { byte => result.append(byte2string(byte)) }
    return result.toString
  }

  def generateSHA1_php(string: String) : String = {
    //val sha = MessageDigest.getInstance("SHA-1");
    //sha.update(string.getBytes())
      
    //val digest = sha.digest();
    //return new BASE64Encoder().encode(bytes2string(digest).getBytes)
    //generateSHA1(string)
    val sha_string = DigestUtils.shaHex(string)
    return new String(Base64.encodeBase64(sha_string.getBytes))
  }

  def css_style(background_color:String, text_color:String)= {
    "body{background-color:" + background_color + ";color:" + text_color + """;}
      A:link { color: #3333FF; }
      A:visited { color: #3333FF; }
      A:active { color: red; } 
      A:hover { color: red; } 
      .left_real_time{background-color:""" + background_color + ";color:" + text_color + """;font-size:11pt;border-width:0px;border-style:solid;}
    """    
  }
  
  def color_helper(room:Room, room_day:RoomDay) = {
    var background_color = "#000030"
    var text_color       = "snow"
    
    if (room.status.is == RoomStatusEnum.ENDED.toString) {
      background_color = "aliceblue"
      text_color       = "black"
    } else if (room_day.day_no.is == 0) {
      background_color = "seashell"
      text_color       = "black"
    } else if ((room_day.day_no.is) % 2 == 0 ) {
      background_color = "floralwhite"
      text_color       = "black"
    }
    
    (background_color, text_color)
  }
}

class RichString2IntBox(start: Box[String]) {
  def getOrZeroInt: Int = 
    try {start.getOrElse("0").toInt; }
    catch { case e: Exception => 0 }
}

class RichString2LongBox(start: Box[String]) {
  def getOrZeroLong: Long = 
    try {start.getOrElse("0").toLong; }
    catch { case e: Exception => 0 }
}


