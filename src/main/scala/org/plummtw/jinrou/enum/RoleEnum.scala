package org.plummtw.jinrou.enum

import org.plummtw.jinrou.data._

object RoleEnum extends Enumeration {
  type RoleEnum = Value
  
  val NONE        = Value("0")
  
  // VILLAGER SIDE
  val VILLAGER    = Value("V")
  val HERMIT      = Value("Y")
  val AUGURER     = Value("A")
  val NECROMANCER = Value("N")
  val HUNTER      = Value("H")
  val GEMINI      = Value("G")
  
  val CLERIC      = Value("C")
  val HERBALIST   = Value("L")
  val ALCHEMIST   = Value("l")
  val POISONER    = Value("P")
  val RUNNER      = Value("R")
  
  val AUGHUNTER   = Value("E")
  val SCHOLAR     = Value("O")
  val ARCHMAGE    = Value("Z")
  val CARDMASTER   = Value("Q")
  
  // WEREWOLF SIDE
  val WEREWOLF    = Value("W")
  val WOLFCUB     = Value("X")
  val MADMAN      = Value("M")
  val SORCEROR    = Value("U")
  
  // FOX SIDE
  val FOX         = Value("F")
  val BETRAYER    = Value("B")
  val GODFAT      = Value("T")
  
  // DEMON SIDE
  val DEMON       = Value("D")
  val FALLEN_ANGEL = Value("f")
  
  // PENGUIN SIDE
  val PENGUIN     = Value("K")

  val PONTIFF     = Value("J")
  
  // NON-SPECIFIC-SIDE
  val INHERITER   = Value("I")
  val SHIFTER     = Value("S")
  
  
  def ROLE_MAP   = scala.collection.immutable.TreeMap(
     NONE        -> RoleNone,
  
     VILLAGER    -> RoleVillager,
     HERMIT      -> RoleHermit,
     AUGURER     -> RoleAugurer,
     NECROMANCER -> RoleNecromancer,
     HUNTER      -> RoleHunter,
     GEMINI      -> RoleGemini,
     
     CLERIC      -> RoleCleric,
     HERBALIST   -> RoleHerbalist,
     ALCHEMIST   -> RoleAlchemist,
     POISONER    -> RolePoisoner,
     RUNNER      -> RoleRunner,
     
     WEREWOLF    -> RoleWerewolf,
     WOLFCUB     -> RoleWolfcub,
     MADMAN      -> RoleMadman,
     SORCEROR    -> RoleSorceror,
     
     AUGHUNTER   -> RoleAugHunter,
     SCHOLAR     -> RoleScholar,
     ARCHMAGE    -> RoleArchmage,
  
     FOX         -> RoleFox,
     BETRAYER    -> RoleBetrayer,
     GODFAT      -> RoleGodfat,  

     DEMON       -> RoleDemon,
     FALLEN_ANGEL-> RoleFallenAngel,

     PENGUIN     -> RolePenguin,

     PONTIFF     -> RolePontiff,

     INHERITER   -> RoleInheriter,
     SHIFTER     -> RoleShifter,
     
     CARDMASTER  -> RoleCardMaster
  )

  def HIDDEN_ROLE_LIST = List(
      HERMIT, ALCHEMIST, AUGHUNTER, ARCHMAGE,
      FALLEN_ANGEL, PENGUIN, CARDMASTER
  )
  
  def get_role(role : RoleEnum.Value) : RoleData = {
    val result = ROLE_MAP.get(role) 
    if (result.isEmpty)
      println(role.toString + "is null")
    return result.getOrElse(RoleNone)
  }
  
  def get_role(role_string : String) : RoleData = {
    return get_role(valueOf(role_string).getOrElse(NONE))
  }
}

object RoleSpecialEnum extends Enumeration {
  type RoleSpecialEnum = Value

  val NONE        = Value("")

  val POISON      = Value("p")
  val RESIST      = Value("r")
  val CONJURE     = Value("c")
  val WHITE       = Value("w")
  val TEN         = Value("t")

  def ROLESPECIAL_MAP   = scala.collection.immutable.TreeMap(
     NONE        -> "",

     POISON      -> "毒",
     RESIST      -> "抗",
     CONJURE     -> "咒",
     WHITE       -> "白",
     TEN         -> "天"
  )

  def get_string(rolespecial_string : String) : String = {
    val result = ROLESPECIAL_MAP.get(valueOf(rolespecial_string).getOrElse(NONE))
    if (result.isEmpty)
      println(rolespecial_string + "is null")
    return result.getOrElse("")
  }
}