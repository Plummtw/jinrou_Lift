package org.plummtw.jinrou.enum

import org.plummtw.jinrou.data._

object RoleEnum extends Enumeration {
  type RoleEnum = Value
  
  val NONE        = Value("0")
  
  // VILLAGER SIDE
  val VILLAGER    = Value("V")
  val AUGURER     = Value("A")
  val NECROMANCER = Value("N")
  val HUNTER      = Value("H")
  val GEMINI      = Value("G")
  
  val CLERIC      = Value("C")
  val HERBALIST   = Value("L")
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
  
  // PENGUIN SIDE
  val PENGUIN     = Value("K")

  val PONTIFF     = Value("J")
  
  // NON-SPECIFIC-SIDE
  val INHERITER   = Value("I")
  val SHIFTER     = Value("S")
  
  
  def ROLE_MAP   = scala.collection.immutable.TreeMap(
     NONE        -> RoleNone,
  
     VILLAGER    -> RoleVillager,
     AUGURER     -> RoleAugurer,
     NECROMANCER -> RoleNecromancer,
     HUNTER      -> RoleHunter,
     GEMINI      -> RoleGemini,
     
     CLERIC      -> RoleCleric,
     HERBALIST   -> RoleHerbalist,
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
     PENGUIN     -> RolePenguin,

     PONTIFF     -> RolePontiff,

     INHERITER   -> RoleInheriter,
     SHIFTER     -> RoleShifter,
     
     CARDMASTER  -> RoleCardMaster
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