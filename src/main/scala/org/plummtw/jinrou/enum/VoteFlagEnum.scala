package org.plummtw.jinrou.enum

import org.plummtw.jinrou.data._

object VoteFlagEnum extends Enumeration {
  type VoteFlagEnum = Value
  
  val AUTO        = Value("A")
  val BLESSED     = Value("B")
  val SHOUTED     = Value("S")
  val CURSED      = Value("C")
  val INVALID     = Value("I")
  val FAKE         = Value("F")

  val POWER       = Value("P")
  val VICTIM      = Value("V")
  val DOMINATE    = Value("D")
  val COMMAND      = Value("J")
  val VORTEX        = Value("R")

  val COLORSPRAY    = Value("T")
}