package org.plummtw.jinrou.view

import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import org.plummtw.jinrou.util._

import net.liftweb.http.{LiftResponse, InMemoryResponse}

import java.io.ByteArrayOutputStream
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

object IdenticonViewCache extends TimedCache[String, Array[Byte]](30000)

object IdenticonView {
  def calc_code(str : String) = {
    val bytes = JinrouUtil.generateSHA1_Bytes(str)
    //for (i <- 0 until 4)
    //  println(bytes(i))

    val code = extract_code(JinrouUtil.generateSHA1_Bytes(str))
    //println(code)
    code
  }

  def extract_code(list : Array[Byte]) = {
    def to_int(byte : Byte) : Int = {
      val result = byte.asInstanceOf[Int]
      if (result < 0)
        (result + 256)
      else
        result
    }

    val tmp = Array(to_int(list(0)) << 24,
                    to_int(list(1)) << 16,
                    to_int(list(2)) << 8,
                    to_int(list(3)))
    /*
    for (i <- 0 until 4) {
      println(tmp(i))
      if (i==0)
        println((if (tmp(0) < 0) -(tmp(0)&0x7fffffff) else tmp(0)))
    } */
    (if (tmp(0) < 0) -(tmp(0)&0x7fffffff) else tmp(0)) | tmp(1) | tmp(2) | tmp(3)
  }


  def render (identicon_id : String) : Box[LiftResponse] = {
    // Query, set up chart, etc...
    val outImage = IdenticonViewCache.getOr(identicon_id) {() =>
      val img = IdenticonRenderer.render(calc_code(identicon_id), 15)
      val out = new ByteArrayOutputStream
      ImageIO.write(img, "png", ImageIO.createImageOutputStream(out))
      out.toByteArray
    }
   // InMemoryResponse is a subclass of LiftResponse
    // it takes an Array of Bytes, a List[(String,String)] of
    // headers, a List[Cookie] of Cookies, and an integer
    // return code (here 200 for HTTP 200: OK)
    Full(InMemoryResponse(outImage,
      ("Content-Type" -> "image/png") :: ("Cache-Control" -> "private,no-cache,no-store") :: Nil,
      Nil,
      200))
  }

}