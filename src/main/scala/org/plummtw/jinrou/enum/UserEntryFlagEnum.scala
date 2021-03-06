package org.plummtw.jinrou.enum

import org.plummtw.jinrou.data._

object UserEntryFlagEnum extends Enumeration {
  type UserEntryFlagEnum = Value
  
  val AUTOVOTED   = Value("A")
  val BITED       = Value("B")
  val VORTEX_USED  = Value("C")
  val AUGURED      = Value("Q")

  val HIDED        = Value("H")
  val REVERSE_USED = Value("Y")

  val SHOUTED      = Value("S")

  val RELIGION    = Value("R")
  val NORELIGION  = Value("N")
  val VOTED       = Value("V")
  
  val ELIXIR_USED = Value("E")
  val POISON_USED = Value("P")

  val EARTH         = Value("地")
  val WATER         = Value("水")
  val AIR           = Value("風")
  val FIRE          = Value("火")

  val ANALYZED      = Value("Z")
  val REPORTED      = Value("O")

  val BECAME_MOB    = Value("M")
  val WATER_ELEM_USED  = Value("W")

  val PONTIFF_STUNNED   = Value("J")
  val PONTIFF_COMMAND_USED = Value("K")
  val PONTIFF_AURA        = Value("U")

  val STUNNED_1          = Value("1")
  val STUNNED_2          = Value("2")
  val STUNNED_3           = Value("3")

  val LINKS               = Value("L")

  val CARD_FOOL           = Value("a")
  val CARD_MAGICIAN       = Value("b")
  val CARD_CHARIOT        = Value("h")
  val CARD_STRENGTH       = Value("i")
  val CARD_HERMIT         = Value("j")
  val CARD_JUSTICE        = Value("l")
  val CARD_TOWER          = Value("q")
  val CARD_SUN            = Value("t")

  val FOX_SPECIAL         = Value("f")

  val GODFAT_TARGETED     = Value("+")
  val GODFAT_PREDICTED     = Value("-")

  val GODFAT_SPECIAL1      = Value("!")
  val GODFAT_SPECIAL2      = Value("@")
  val GODFAT_SPECIAL3      = Value("#")
  val GODFAT_SPECIAL4      = Value("$")
  val GODFAT_SPECIAL_USED  = Value("G")
  val GODFAT_SPECIAL2_USED  = Value("=")
  
  val ICED_1           = Value("4")
  val ICED_2           = Value("5")
  val ICED_3           = Value("6")
  val CHILL_USED      = Value("7")

  val DEATH_2             = Value("8")
  val DEATH_1              = Value("9")
  val DEATH_0              = Value("0")

  val ALTERNATE            = Value("z")
  val ALCHEMIST_ALTERNATE  = Value("y")

  val FALLEN               = Value("*")
  val SUMMON               = Value("/")
  val SEAR                 = Value("^")

  val LOVER                = Value("&")

  val DMESSAGE_SEALED     = Value("%")
}