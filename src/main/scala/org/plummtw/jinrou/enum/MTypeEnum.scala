package org.plummtw.jinrou.enum

import org.plummtw.jinrou.data._

object MTypeEnum extends Enumeration {
  type MTypeEnum = Value
  
  val TALK             = Value("T")
  val TALK_ADMIN        = Value("TA")
  val TALK_ADMIN_PRIVATE = Value("TAP")
  val TALK_DAY         = Value("TD")
  val TALK_DAY_FOG    = Value("TDF")
  val TALK_SECRET      = Value("TS")
  val TALK_SEALED      = Value("TZ")
  val TALK_NIGHT       = Value("TN")  
  val TALK_WEREWOLF    = Value("TW")
  val TALK_WOLFCUB     = Value("TX")
  val TALK_GEMINI      = Value("TG")
  val TALK_GEMINI_DAY = Value("TGD")
  val TALK_FOX         = Value("TF")
  val TALK_DISGUISED  = Value("TB")
  val TALK_PONTIFF     = Value("TJ")
  val TALK_HEAVEN      = Value("TH")
  val TALK_END         = Value("TE")
  
  val MESSAGE          = Value("S")
  val MESSAGE_GENERAL  = Value("S0")
  val MESSAGE_COME     = Value("S1")
  val MESSAGE_LEAVE    = Value("S2")
  val MESSAGE_KICKED   = Value("S3")
  val MESSAGE_REVOTE0  = Value("S4")
  val MESSAGE_LAST2MIN = Value("S5")
  val MESSAGE_DEATHSUDDEN = Value("S6")
  val MESSAGE_REVOTE   = Value("S7")
  val MESSAGE_NIGHT    = Value("S8")
  val MESSAGE_EVIL     = Value("S9")
  val MESSAGE_FOX       = Value("SF")
  
  val BITED_FOX      = Value("BF")
  val BITED_DEMON    = Value("BD")

  val DEATH            = Value("D")
  val DEATH_BETRAYER = Value("DB")
  val DEATH_CLERIC   = Value("DC")
  val DEATH_DEMON    = Value("DD")
  val DEATH_FOX      = Value("DF")
  val DEATH_SORCEROR = Value("DU")
  val DEATH_HANGED   = Value("DH")
  val DEATH_HUNTER   = Value("DHH")
  val DEATH_HUNTER_KILL = Value("DHK")
  val DEATH_MADMAN    = Value("DM")
  val DEATH_POISON_D = Value("DP")
  val DEATH_POISON_N = Value("DQ")
  val DEATH_POISON_H = Value("DJ")
  val DEATH_RUNNER   = Value("DR")
  val DEATH_SUDDEN   = Value("DS")
  val DEATH_EATEN    = Value("DE")
  val DEATH_GODFAT    = Value("DT")
  val DEATH_WOLFCUB    = Value("DX")
  val DEATH_WOLFCUB_EATEN = Value("DXE")
  val DEATH_SUBPONTIFF = Value("DSP")
  val DEATH_LOVER        = Value("DL")

  val VOTE             = Value("V")  
  val VOTE_STARTGAME   = Value("V_")

  val VOTE_BECOMEMOB  = Value("VBM")

  val VOTE_HANG        = Value("VV")
  val VOTE_KICK        = Value("VK")
  
  val VOTE_VILLAGER    = Value("VE")
  
  val VOTE_AUGURER     = Value("VA")
  val VOTE_HUNTER      = Value("VH")
  
  val VOTE_CLERIC_BLESS     = Value("VCB")
  val VOTE_CLERIC_SANCTURE  = Value("VCS")

  val VOTE_HERBALIST_ELIXIR = Value("VLE")
  val VOTE_HERBALIST_POISON = Value("VLP")
  val VOTE_HERBALIST_MIX  = Value("VLM")
  
  val VOTE_RUNNER       = Value("VR")

  val VOTE_SCHOLAR_EXAMINE      = Value("VOE")
  val VOTE_SCHOLAR_ANALYZE      = Value("VOA")
  val VOTE_SCHOLAR_REPORT       = Value("VOR")

  val VOTE_ARCHMAGE_DISPELL     = Value("VZD")
  val VOTE_ARCHMAGE_SUMMON      = Value("VZS")
  
  val VOTE_WEREWOLF    = Value("VW")
  val VOTE_WOLFCUB      = Value("VX")

  val VOTE_MADMAN_STUN1     = Value("VM1")
  val VOTE_MADMAN_STUN3     = Value("VM3")
  val VOTE_MADMAN_STUN     = Value("VMT")
  val VOTE_MADMAN_SUICIDE  = Value("VMS")

  val VOTE_SORCEROR_AUGURE  = Value("VSA")
  val VOTE_SORCEROR_WHISPER = Value("VSW")
  val VOTE_SORCEROR_CONJURE = Value("VSC")
  val VOTE_SORCEROR_SHOUT   = Value("VSS")
  val VOTE_SORCEROR_BELIEVE = Value("VSB")

  val VOTE_FOX         = Value("VF")
  val VOTE_FOX2        = Value("VF2")

  val VOTE_BETRAYER_DISGUISE = Value("VBD")
  val VOTE_BETRAYER_CHANGE   = Value("VBC")
  val VOTE_BETRAYER_FOG   = Value("VBF")

  val VOTE_GODFAT_SPECIAL1  = Value("VT1")
  val VOTE_GODFAT_SPECIAL2  = Value("VT2")
  val VOTE_GODFAT_SPECIAL3  = Value("VT3")
  val VOTE_GODFAT_DEATHGAZE  = Value("VTD")
  val VOTE_GODFAT_COLORSPRAY = Value("VTC")
  val VOTE_GODFAT_BLIND       = Value("VTB")
  val VOTE_GODFAT_BLIND2       = Value("VTL")
  val VOTE_GODFAT_EXCHANGE    = Value("VTE")
  
  val VOTE_DEMON_CHAOS = Value("VDV")
  val VOTE_DEMON_CURSE = Value("VDC")
  val VOTE_DEMON_CURSE2 = Value("VDO")
  val VOTE_DEMON_DOMINATE = Value("VDD")
  val VOTE_DEMON_VORTEX = Value("VDR")
  val VOTE_INHERITER   = Value("VI")
  val VOTE_SHIFTER     = Value("VS")

  val VOTE_PONTIFF     = Value("VJ")
  val VOTE_PONTIFF_COMMAND = Value("VJC")
  val VOTE_PONTIFF_AURA    = Value("VJA")

  val VOTE_CARD_FOOL      = Value("Vr")
  val VOTE_CARD_MAGICIAN = Value("Va")
  val VOTE_CARD_CHARIOT  = Value("Vh")
  val VOTE_CARD_STRENGTH = Value("Vm3")
  val VOTE_CARD_HERMIT   = Value("Vbd")
  val VOTE_CARD_JUSTICE  = Value("Voe")
  val VOTE_CARD_TOWER     = Value("Vdv")
  val VOTE_CARD_SUN       = Value("VXS")

  val VOTE_NO_ACTION   = Value("VN")

  def DEATH_MAP   = scala.collection.immutable.TreeMap(
    DEATH          -> "死因不明",
    DEATH_BETRAYER -> "跟隨妖狐死亡",
    DEATH_CLERIC   -> "牧師聖域術犧牲",
    DEATH_FOX      -> "被占卜師咒殺",
    DEATH_SORCEROR -> "被狂巫咒殺",
    DEATH_HANGED   -> "被吊死",
    DEATH_HUNTER   -> "獵人特殊死亡",
    DEATH_HUNTER_KILL  -> "被獵人擊殺",
    DEATH_MADMAN   -> "狂人自爆死亡",
    DEATH_POISON_D -> "被埋毒者毒死",
    DEATH_POISON_N -> "被埋毒者毒死",
    DEATH_POISON_H -> "被藥師毒死",
    DEATH_RUNNER   -> "逃亡者特殊死亡",
    DEATH_SUDDEN   -> "暴斃死亡",
    DEATH_EATEN    -> "被人狼襲擊",
    DEATH_GODFAT   -> "被哥德法逆咒殺",
    DEATH_WOLFCUB  -> "跟隨人狼死亡",
    DEATH_WOLFCUB_EATEN  -> "被幼狼襲擊",
    DEATH_SUBPONTIFF -> "跟隨教主死亡",
    DEATH_LOVER    -> "跟隨生命連繫者死亡"
  )

  def get_death_text(death : MTypeEnum.Value) : String = {
    val result = DEATH_MAP.get(death)
    //if (result.isEmpty)
    //  println(role.toString + "is null")
    return result.getOrElse("死因不明")
  }

  def get_death_text(mtype_string : String) : String = {
    return get_death_text(valueOf(mtype_string).getOrElse(DEATH))
  }
}