package org.plummtw.jinrou.enum

object RoomVictoryEnum extends Enumeration {
  type RoomVictoryEnum = Value
  
  val NONE         = Value("N")
  
  val VILLAGER_WIN = Value("V")
  val WEREWOLF_WIN = Value("W")
  val FOX_WIN      = Value("F")
  val FOX_WIN2     = Value("G")
  val DEMON_WIN    = Value("D")
  val FALLENANGEL_WIN = Value("f")
  val PENGUIN_WIN  = Value("K")
  val PONTIFF_WIN  = Value("J")
  val MOB_WIN      = Value("M")
  val MOB_WIN2      = Value("P")
  val LOVER_WIN     = Value("L")

  val DRAW         = Value("0")
  val ABANDONED    = Value("1")  
}