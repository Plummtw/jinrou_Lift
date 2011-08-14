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

class StatController {
  def stat (xhtml : Group) : NodeSeq = {
    val stat_counts        = List(10,25,50,100,200,300,400,500)

    var villager_wins = 0
    var mob_wins      = 0
    var werewolf_wins = 0
    var fox_wins      = 0
    var demon_wins    = 0
    var penguin_wins  = 0
    var pontiff_wins  = 0
    var lover_wins    = 0
    var draws         = 0
    var others        = 0
    var total_wins    = 0

    def stat_count (rooms : List[Room], count : Int)  {
      villager_wins = 0
      mob_wins      = 0
      werewolf_wins = 0
      fox_wins      = 0
      demon_wins    = 0
      penguin_wins  = 0
      pontiff_wins  = 0
      lover_wins    = 0
      draws         = 0
      others        = 0
      total_wins    = 0

      rooms.foreach { room =>
        if (!room.has_flag(RoomFlagEnum.TEST_MODE)) {
          if (room.victory.is == RoomVictoryEnum.VILLAGER_WIN.toString) villager_wins += 1
          else if (room.victory.is == RoomVictoryEnum.MOB_WIN.toString) mob_wins += 1
          else if (room.victory.is == RoomVictoryEnum.MOB_WIN2.toString) mob_wins += 1
          else if (room.victory.is == RoomVictoryEnum.WEREWOLF_WIN.toString) werewolf_wins += 1
          else if (room.victory.is == RoomVictoryEnum.FOX_WIN.toString) fox_wins += 1
          else if (room.victory.is == RoomVictoryEnum.FOX_WIN2.toString) fox_wins += 1
          else if (room.victory.is == RoomVictoryEnum.DEMON_WIN.toString) demon_wins += 1
          else if (room.victory.is == RoomVictoryEnum.PENGUIN_WIN.toString) penguin_wins += 1
          else if (room.victory.is == RoomVictoryEnum.PONTIFF_WIN.toString) pontiff_wins += 1
          else if (room.victory.is == RoomVictoryEnum.LOVER_WIN.toString) lover_wins += 1
          else if (room.victory.is == RoomVictoryEnum.DRAW.toString) draws += 1
          else  others += 1

          total_wins += 1
          if (total_wins >= count)
            return
        }
      }

      return
    }


    def stat_table (rooms : List[Room], count : Int) : scala.xml.Node = {
      stat_count(rooms, count)
      <table>
        <tr><td colspan="3">近 {total_wins} 場資料</td></tr>
        <tr><td>村勝</td><td>{villager_wins}</td><td>{villager_wins*100.0/total_wins}%</td></tr>
        <tr><td>暴勝</td><td>{mob_wins}</td><td>{mob_wins*100.0/total_wins}%</td></tr>
        <tr><td>狼勝</td><td>{werewolf_wins}</td><td>{werewolf_wins*100.0/total_wins}%</td></tr>
        <tr><td>狐勝</td><td>{fox_wins}</td><td>{fox_wins*100.0/total_wins}%</td></tr>
        <tr><td>惡勝</td><td>{demon_wins}</td><td>{demon_wins*100.0/total_wins}%</td></tr>
        <tr><td>教勝</td><td>{pontiff_wins}</td><td>{pontiff_wins*100.0/total_wins}%</td></tr>
        <tr><td>企勝</td><td>{penguin_wins}</td><td>{penguin_wins*100.0/total_wins}%</td></tr>
        <tr><td>戀勝</td><td>{lover_wins}</td><td>{lover_wins*100.0/total_wins}%</td></tr>
        <tr><td>平手</td><td>{draws}</td><td>{draws*100.0/total_wins}%</td></tr>
        <tr><td>其他</td><td>{others}</td><td>{others*100.0/total_wins}%</td></tr>
      </table>
    }

    val rooms = Room.findAll(NotBy(Room.victory, ""),
                             NotBy(Room.victory, RoomVictoryEnum.NONE.toString),
                             NotBy(Room.victory, RoomVictoryEnum.ABANDONED.toString),
                             MaxRows(stat_counts.last * 2),
                             OrderBy(Room.id, Descending))
    var result : NodeSeq = Seq()

    stat_counts.foreach { stat_count =>
      result ++= stat_table(rooms, stat_count)
      result ++= <br/>
    }

    bind("entry", xhtml,
      "stat"               -> result
    )
  }

  
}