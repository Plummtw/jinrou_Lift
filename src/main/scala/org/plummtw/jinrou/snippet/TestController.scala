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

class TestController {
  def all_enum = List(
    RoomFlagEnum.TEST_MODE,
    RoomFlagEnum.WISH_ROLE,
    RoomFlagEnum.DUMMY_REVEAL,
    RoomFlagEnum.VOTE_REVEAL,
    RoomFlagEnum.DEATH_LOOK,
    RoomFlagEnum.GEMINI_TALK,
    RoomFlagEnum.AUTO_VOTE,

    RoomFlagEnum.ROLE_CLERIC,
    RoomFlagEnum.ROLE_HERBALIST,
    RoomFlagEnum.ROLE_POISONER,
    RoomFlagEnum.ROLE_RUNNER,
    RoomFlagEnum.ROLE_SCHOLAR,
    RoomFlagEnum.ROLE_AUGHUNTER,
    RoomFlagEnum.ROLE_ARCHMAGE,

    RoomFlagEnum.ROLE_SORCEROR,
    RoomFlagEnum.ROLE_WOLFCUB,

    RoomFlagEnum.ROLE_BETRAYER,
    RoomFlagEnum.ROLE_GODFAT,

    RoomFlagEnum.ROLE_DEMON,
    RoomFlagEnum.ROLE_PONTIFF,
    RoomFlagEnum.ROLE_INHERITER,
    RoomFlagEnum.ROLE_SHIFTER,
    RoomFlagEnum.ROLE_CARDMASTER,


    RoomFlagEnum.SUBROLE_MEMORYLOSS4,
    RoomFlagEnum.SUBROLE_MEMORYLOSS6,
    RoomFlagEnum.SUBROLE_MEMORYLOSS8,
    RoomFlagEnum.SUBROLE_SUDDENDEATH,
    RoomFlagEnum.SUBROLE_AVENGER,
    RoomFlagEnum.SUBROLE_WOLFBELIEVER,
    RoomFlagEnum.SUBROLE_SUBPONTIFF,
    RoomFlagEnum.SUBROLE_PLUS,

    RoomFlagEnum.VILLAGER_DETECT,
    RoomFlagEnum.GEMINI_DAYTALK,
    RoomFlagEnum.RUNNER_OPTION1,
    RoomFlagEnum.RUNNER_OPTION2,
    RoomFlagEnum.RUNNER_OPTION3,
    RoomFlagEnum.MADMAN_KNOWLEDGE,
    RoomFlagEnum.MADMAN_SUICIDE,
    RoomFlagEnum.MADMAN_STUN,
    RoomFlagEnum.FOX_OPTION1,
    RoomFlagEnum.FOX_OPTION2,
    RoomFlagEnum.BETRAYER_OPTION1,
    RoomFlagEnum.BETRAYER_OPTION2,
    RoomFlagEnum.INHERITER_REVEAL,
    RoomFlagEnum.SHIFTER_REVEAL)

  // 新增 25 人測試用村
  def test1 (xhtml : Group) : NodeSeq = {
    val room = Room.create.room_name("測試用").room_comment("25人測試用村")
                   .max_user(25)
                   .day_minutes(5).night_minutes(3)
                   .room_flags(all_enum.map(_.toString).mkString("",",",""))
                   .status(RoomStatusEnum.WAITING.toString).victory("")
      
    room.validate match {
      case Nil => ;
      case xs  => S.error(xs); return redirectTo("main.html")
    }
      
    // 加入替身君
    val dummy_boy = UserEntry.create.uname("dummy_boy").handle_name("替身君").sex("M").user_icon_id(1)
                    .password("dummy_boy").last_words("替身君的遺言測試").role(RoleEnum.NONE.toString)
                    .user_flags(UserEntryFlagEnum.VOTED.toString)
                    .ip_address(S.request.map{x=>JinrouUtil.getIpAddress(x)}.openOr(""))
                      
    dummy_boy.validate match { 
      case Nil => ;
      case xs  => S.error(xs); return redirectTo("main.html")
    }                
      
    // 加入大廳
    val game_hall = RoomDay.create.day_no(0).vote_time(0)
      
    room.save()
    dummy_boy.room_id(room.id.is)
    dummy_boy.save()
    game_hall.room_id(room.id.is)
    game_hall.save()
    
    // 加入24人
    for (i <- 'a' to 'x') {
      val voted  = if (i=='a') "" else UserEntryFlagEnum.VOTED.toString
      val i_str  = i.toString
      val player = UserEntry.create
                    .room_id(room.id.is)
                    .uname(i_str).handle_name(i_str).sex("M").user_icon_id(2)
                    .password(JinrouUtil.generateSHA1("aaaaaa").substring(0,20))
                    .last_words("").role(RoleEnum.NONE.toString)
                    .user_flags(voted)
                    .ip_address(S.request.map{x=>JinrouUtil.getIpAddress(x)}.openOr(""))
                      
      player.validate match { 
        case Nil => player.save()
        case xs  => S.error(xs); return redirectTo("main.html")
      }
    }
        
    S.redirectTo("main.html")
  }
  
  // 新增 8 人測試用村
  def test2 (xhtml : Group) : NodeSeq = {
    val room = Room.create.room_name("測試用").room_comment("8人測試用村")
                   .max_user(8)
                   .day_minutes(5).night_minutes(3)
                   .room_flags(all_enum.map(_.toString).mkString("",",",""))
                   .status(RoomStatusEnum.WAITING.toString).victory("")
      
    room.validate match {
      case Nil => ;
      case xs  => S.error(xs); return redirectTo("main.html")
    }
      
    // 加入替身君
    val dummy_boy = UserEntry.create.uname("dummy_boy").handle_name("替身君").sex("M").user_icon_id(1)
                    .password("dummy_boy").last_words("替身君的遺言測試").role(RoleEnum.NONE.toString)
                    .user_flags(UserEntryFlagEnum.VOTED.toString)
                    .ip_address(S.request.map{x=>JinrouUtil.getIpAddress(x)}.openOr(""))
                      
    dummy_boy.validate match { 
      case Nil => ;
      case xs  => S.error(xs); return redirectTo("main.html")
    }                
      
    // 加入大廳
    val game_hall = RoomDay.create.day_no(0).vote_time(0)
      
    room.save()
    dummy_boy.room_id(room.id.is)
    dummy_boy.save()
    game_hall.room_id(room.id.is)
    game_hall.save()
    
    // 加入7人
    for (i <- 'a' to 'g') {
      val voted  = if (i=='a') "" else UserEntryFlagEnum.VOTED.toString
      val i_str  = i.toString
      val player = UserEntry.create
                    .room_id(room.id.is)
                    .uname(i_str).handle_name(i_str).sex("M").user_icon_id(2)
                    .password(JinrouUtil.generateSHA1("aaaaaa").substring(0,20))
                    .last_words("").role(RoleEnum.NONE.toString)
                    .user_flags(voted)
                    .ip_address(S.request.map{x=>JinrouUtil.getIpAddress(x)}.openOr(""))
                      
      player.validate match { 
        case Nil => player.save()
        case xs  => S.error(xs); return redirectTo("main.html")
      }
    }
        
    S.redirectTo("main.html")
  }

}