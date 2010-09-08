package org.plummtw.jinrou.enum

object RoomFlagEnum extends Enumeration {
  type RoomFlagEnum = Value
  
  // Game Option
  val TEST_MODE    = Value("TM")
  val WISH_ROLE    = Value("WR")
  val NO_DUMMY     = Value("ND")
  val DUMMY_REVEAL = Value("DR")
  val VOTE_REVEAL  = Value("VR")
  val DEATH_LOOK   = Value("DL")
  val GEMINI_TALK  = Value("GT")
  val AUTO_VOTE    = Value("AV")
  val WEATHER       = Value("WE")
  val MOB_MODE      = Value("MM")
  val MOB_MODE1     = Value("MN")
  
  // Optional Role

  val ROLE_CLERIC    = Value("RC")
  val ROLE_HERBALIST = Value("RL")
  val ROLE_POISONER  = Value("RP")
  val ROLE_RUNNER    = Value("RR")
  val ROLE_AUGHUNTER = Value("RE")
  val ROLE_SCHOLAR   = Value("RO")
  val ROLE_ARCHMAGE  = Value("RZ")

  val ROLE_WOLFCUB   = Value("RX")
  val ROLE_SORCEROR  = Value("RU")
  
  val ROLE_BETRAYER  = Value("RB")
  val ROLE_GODFAT    = Value("RT")
  
  val ROLE_DEMON     = Value("RD")
  val ROLE_PONTIFF   = Value("RJ")

  val ROLE_INHERITER = Value("RI")  
  val ROLE_SHIFTER   = Value("RS")
  val ROLE_CARDMASTER = Value("RQ")
  
  val SUBROLE_MEMORYLOSS4  = Value("S4")
  val SUBROLE_MEMORYLOSS4_2  = Value("S_4")
  val SUBROLE_MEMORYLOSS6  = Value("S6")
  val SUBROLE_MEMORYLOSS8  = Value("S8")
  val SUBROLE_FAKEAUGURER  = Value("SF")
  val SUBROLE_SUDDENDEATH  = Value("SS")
  val SUBROLE_AVENGER        = Value("SV")
  val SUBROLE_WOLFBELIEVER = Value("SW")
  val SUBROLE_ALPHAWOLF      = Value("SX")
  val SUBROLE_WISEWOLF         = Value("SY")
  val SUBROLE_SUBPONTIFF     = Value("SP")
  val SUBROLE_PLUS           = Value("S+")
  
  // Role Adjustment
  val VILLAGER_DETECT  = Value("V1")
  val NECROMANCER_OPTION1 = Value("N1")
  val GEMINI_DAYTALK    = Value("G1")
  val GEMINI_BALANCE    = Value("G2")
  val HUNTER_OPTION1    = Value("H1")
  val HUNTER_OPTION2     = Value("H2")
  val CLERIC_OPTION1    = Value("C1")
  val CLERIC_OPTION2    = Value("C2")
  val HERBALIST_MIX        = Value("L1")
  val SCHOLAR_OPTION1    = Value("O1")
  val SCHOLAR_OPTION2    = Value("O2")
  val SCHOLAR_OPTION3    = Value("O3")
  val SCHOLAR_OPTION4    = Value("O4")
  val WOLFCUB_OPTION1  = Value("X1")
  val MADMAN_KNOWLEDGE = Value("M1")
  val MADMAN_SUICIDE   = Value("M2")
  val MADMAN_STUN      = Value("M3")
  val SORCEROR_BELIEVE = Value("U1")
  val SORCEROR_WHISPER1 = Value("U2")
  val SORCEROR_SHOUT1    = Value("U3")
  val RUNNER_OPTION1   = Value("R1")
  val RUNNER_OPTION2   = Value("R2")
  val RUNNER_OPTION3   = Value("R3")
  val FOX_OPTION1       = Value("F1")
  val FOX_OPTION2       = Value("F2")
  val FOX_OPTION3       = Value("F3")
  val BETRAYER_OPTION1  = Value("B1")
  val BETRAYER_OPTION2   = Value("B2")
  val BETRAYER_OPTION3   = Value("B3")
  val GODFAT_SPECIAL1  = Value("T1")
  val GODFAT_SPECIAL2   = Value("T2")
  val GODFAT_SPECIAL3   = Value("T3")
  val DEMON_OPTION1    = Value("D1")
  val DEMON_OPTION2    = Value("D2")
  val DEMON_OPTION3    = Value("D3")
  val PENGUIN_OPTION1    = Value("K1")
  val PENGUIN_OPTION2    = Value("K2")
  val PONTIFF_OPTION1  = Value("J1")
  val PONTIFF_OPTION2    = Value("J2")
  val PONTIFF_OPTION3    = Value("J3")
  val INHERITER_REVEAL = Value("I1")
  val INHERITER_NEUTRAL = Value("I2")
  val SHIFTER_REVEAL   = Value("S1")
  val SHIFTER_LOVER    = Value("S2")
  
  val FLAGNAME_MAP   = Map(
    TEST_MODE    -> "(測)",
    WISH_ROLE    -> "(希)",
    NO_DUMMY     -> "(無替)",
    DUMMY_REVEAL -> "(替)",
    VOTE_REVEAL  -> "(票)",
    DEATH_LOOK   -> "(靈)",
    GEMINI_TALK  -> "(共)",
    AUTO_VOTE    -> "(自投)",
    WEATHER      -> "(天候)",
    MOB_MODE     -> "(暴)",
    MOB_MODE1    -> "(暴1)",
  
    // Optional Role

    ROLE_CLERIC    -> "[牧]",
    ROLE_HERBALIST -> "[藥]",
    ROLE_POISONER  -> "[毒]",
    ROLE_RUNNER    -> "[逃]",
    ROLE_AUGHUNTER -> "[占獵]",
    ROLE_SCHOLAR   -> "[學]",
    ROLE_ARCHMAGE  -> "[大]",

    ROLE_SORCEROR  -> "[巫]",
    ROLE_WOLFCUB   -> "[幼]",
  
    ROLE_BETRAYER  -> "[背]",
    ROLE_GODFAT    -> "[哥]",
  
    ROLE_DEMON     -> "[惡]",
    ROLE_PONTIFF   -> "[教]",

    ROLE_INHERITER -> "[繼]",
    ROLE_SHIFTER   -> "[模]",
    ROLE_CARDMASTER -> "[卡]",

    SUBROLE_MEMORYLOSS4  -> "[忘4]",
    SUBROLE_MEMORYLOSS4_2-> "[忘4+]",
    SUBROLE_MEMORYLOSS6  -> "[忘6]",
    SUBROLE_MEMORYLOSS8  -> "[忘8]",
    SUBROLE_FAKEAUGURER  -> "[冒]",
    SUBROLE_SUDDENDEATH  -> "[絕]",
    SUBROLE_AVENGER      -> "[復]",
    SUBROLE_WOLFBELIEVER  -> "[狼信]",
    SUBROLE_ALPHAWOLF     -> "[大狼]",
    SUBROLE_WISEWOLF      -> "[智狼]",
    SUBROLE_SUBPONTIFF    -> "[副&無]",
    SUBROLE_PLUS          -> "[副+]",
  
    // Role Adjustment
    VILLAGER_DETECT  -> "<村>",
    NECROMANCER_OPTION1 -> "<靈>",
    GEMINI_DAYTALK   -> "<共1>",
    GEMINI_BALANCE   -> "<共2>",
    HUNTER_OPTION1   -> "<獵1>",
    HUNTER_OPTION2   -> "<獵2>",
    CLERIC_OPTION1   -> "<牧1>",
    CLERIC_OPTION2   -> "<牧2>",
    HERBALIST_MIX    -> "<藥>",
    SCHOLAR_OPTION1  -> "<學1>",
    SCHOLAR_OPTION2  -> "<學2>",
    SCHOLAR_OPTION3  -> "<學3>",
    SCHOLAR_OPTION4  -> "<學4>",
    WOLFCUB_OPTION1  -> "<幼>",
    MADMAN_KNOWLEDGE -> "<狂1>",
    MADMAN_SUICIDE   -> "<狂2>",
    MADMAN_STUN      -> "<狂3>",
    SORCEROR_BELIEVE -> "<巫1>",
    SORCEROR_WHISPER1 -> "<巫2>",
    SORCEROR_SHOUT1   -> "<巫3>",
    RUNNER_OPTION1   -> "<逃1>",
    RUNNER_OPTION2   -> "<逃2>",
    RUNNER_OPTION3   -> "<逃3>",
    FOX_OPTION1      -> "<狐1>",
    FOX_OPTION2      -> "<狐2>",
    FOX_OPTION3      -> "<狐3>",
    BETRAYER_OPTION1 -> "<背1>",
    BETRAYER_OPTION2 -> "<背2>",
    BETRAYER_OPTION3 -> "<背3>",
    GODFAT_SPECIAL1  -> "<哥1>",
    GODFAT_SPECIAL2  -> "<哥2>",
    GODFAT_SPECIAL3  -> "<哥3>",
    DEMON_OPTION1    -> "<惡1>",
    DEMON_OPTION2    -> "<惡2>",
    DEMON_OPTION3    -> "<惡3>",
    PENGUIN_OPTION1  -> "<企1>",
    PENGUIN_OPTION2  -> "<企2>",
    PONTIFF_OPTION1  -> "<教1>",
    PONTIFF_OPTION2  -> "<教2>",
    PONTIFF_OPTION3  -> "<教3>",
    INHERITER_REVEAL -> "<繼1>",
    INHERITER_NEUTRAL -> "<繼2>",
    SHIFTER_REVEAL   -> "<模1>",
    SHIFTER_LOVER    -> "<模2>"
  )
  
  def flag_name(flag : RoomFlagEnum.Value) = {
    FLAGNAME_MAP.get(flag)
  }
}