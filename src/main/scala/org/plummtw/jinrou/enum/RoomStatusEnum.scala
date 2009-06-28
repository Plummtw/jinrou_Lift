package org.plummtw.jinrou.enum

object RoomStatusEnum extends Enumeration {
  type RoomStatusEnum = Value
  
  val WAITING = Value("W")
  val PLAYING = Value("P")
  val ENDED   = Value("E")
}