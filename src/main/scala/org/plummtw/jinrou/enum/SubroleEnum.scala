package org.plummtw.jinrou.enum

import org.plummtw.jinrou.data._

object SubroleEnum extends Enumeration {
  type SubroleEnum = Value

  val NONE          = Value("0")
  val DECIDER      = Value("D")
  val AUTHORITY    = Value("A")
  val MEMORYLOSS4  = Value("4")
  val MEMORYLOSS6  = Value("6")
  val MEMORYLOSS8  = Value("8")
  val FAKEAUGURER  = Value("F")
  val SUDDENDEATH  = Value("I")
  val AVENGER       = Value("V")
  val WOLFBELIEVER = Value("W")
  val FOXBELIEVER  = Value("B")
  val ALPHAWOLF    = Value("X")
  val WISEWOLF     = Value("Y")
  val WOLFSTAMP    = Value("Z")
  val SUBPONTIFF   = Value("P")
  val NORELIGION    = Value("N")
  val HASHIHIME     = Value("H")
  
  def SUBROLE_MAP   =scala.collection.immutable.TreeMap(
     NONE        -> SubroleNone,
     DECIDER     -> SubroleDecider,
     AUTHORITY   -> SubroleAuthority,
     MEMORYLOSS4 -> SubroleMemoryLoss4,
     MEMORYLOSS6 -> SubroleMemoryLoss6,
     MEMORYLOSS8 -> SubroleMemoryLoss8,
     FAKEAUGURER -> SubroleFakeAugurer,
     SUDDENDEATH -> SubroleSuddenDeath,
     AVENGER     -> SubroleAvenger,
     WOLFBELIEVER -> SubroleWolfBeliever,
     FOXBELIEVER -> SubroleFoxBeliever,
     ALPHAWOLF   -> SubroleAlphaWolf,
     WISEWOLF    -> SubroleWiseWolf,
     WOLFSTAMP   -> SubroleWolfStamp,
     SUBPONTIFF  -> SubroleSubpontiff,
     NORELIGION  -> SubroleNoreligion,
     HASHIHIME   -> SubroleHashihime
  )
  
  def get_subrole(subrole : SubroleEnum.Value) : SubroleData= {
    val result = SUBROLE_MAP.get(subrole)
    //if (result.isEmpty)
    //  println(subrole.toString + "is null")
    return result.getOrElse(SubroleNone)
  }

  def get_subrole(role_string : String) : SubroleData = {
    return get_subrole(valueOf(role_string).getOrElse(NONE))
  }
}