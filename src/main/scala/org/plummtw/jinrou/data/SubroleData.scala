package org.plummtw.jinrou.data

import org.plummtw.jinrou.enum._

class SubroleData(role: SubroleEnum.Value, name: String, color : String) {
  def subrole_enum         = role
  def subrole_name         = name 
  def subrole_color        = color
  
  def subrole_intro: scala.xml.Elem = <span></span>
  def subrole_pic = <img src="images/subrolepic_no.gif" />
  def subrole_pic_true = subrole_pic
  
  def ctext = <font color={subrole_color}>[{subrole_name}]</font>
    
  override def toString(): String = subrole_name
}

object SubroleNone   extends SubroleData(SubroleEnum.NONE,  "無副職", "#999999") {
  //override def subrole_intro = <span>無副職</span>
  override def subrole_pic = <img src="images/subrolepic_no.gif" />
}

object SubroleDecider   extends SubroleData(SubroleEnum.DECIDER,  "決定者", "#999999") {
  override def subrole_pic = <img src="images/subrolepic_decider.gif" />
}

object SubroleAuthority extends SubroleData(SubroleEnum.AUTHORITY, "權力者", "#999999") {
  override def subrole_intro = <img src="images/role_authority.gif"/>
  override def subrole_pic = <img src="images/subrolepic_authority.gif" />
}

object SubroleMemoryLoss4 extends SubroleData(SubroleEnum.MEMORYLOSS4,  "忘卻者4", "#999999") {
  override def subrole_pic = <img src="images/subrolepic_memoryloss4.gif" />
}

object SubroleMemoryLoss6 extends SubroleData(SubroleEnum.MEMORYLOSS6, "忘卻者6", "#999999") {
  override def subrole_pic = <img src="images/subrolepic_memoryloss6.gif" />
}

object SubroleMemoryLoss8 extends SubroleData(SubroleEnum.MEMORYLOSS8, "忘卻者8", "#999999") {
  override def subrole_pic = <img src="images/subrolepic_memoryloss8.gif" />
}

object SubroleFakeAugurer extends SubroleData(SubroleEnum.FAKEAUGURER, "冒牌占", "#9933FF") {
  override def subrole_pic = <img src="images/subrolepic_no.gif" />
  override def subrole_pic_true = <img src="images/subrolepic_fakeaugurer.gif" />
  override def toString(): String = "無副職"
}


object SubroleSuddenDeath extends SubroleData(SubroleEnum.SUDDENDEATH, "絕望者", "#999999") {
  //override def subrole_intro = <span>[副職]你身有隱族，於七日投票之後暴斃身亡。</span>
  override def subrole_intro = <img src="images/subrole_suddendeath.gif"/>
  override def subrole_pic = <img src="images/subrolepic_suddendeath.gif" />
}

object SubroleAvenger extends SubroleData(SubroleEnum.AVENGER, "復仇者", "#999999") {
  //override def subrole_intro = <span>[副職]你帶有狂烈的復仇之心，被吊死時會同時讓你投票的人暴斃身亡。</span>
  override def subrole_intro = <img src="images/subrole_avenger.gif"/>
  override def subrole_pic = <img src="images/subrolepic_avenger.gif" />
}

object SubroleWolfBeliever extends SubroleData(SubroleEnum.WOLFBELIEVER, "狼信者", "#FF0000") {
  //override def subrole_intro = <span>[副職]你是狼的追隨者，人狼的勝利是你所希望的。</span>
  override def subrole_intro = <img src="images/subrole_wolfbeliever.gif"/>
  override def subrole_pic = <img src="images/subrolepic_wolfbeliever.gif" />
}

object SubroleFoxBeliever extends SubroleData(SubroleEnum.FOXBELIEVER, "狐信者", "#CC0099") {
  //override def subrole_intro = <span>[副職]你是妖狐的追隨者，妖狐的勝利是你所希望的。</span>
  override def subrole_intro = <img src="images/subrole_foxbeliever.gif"/>
  override def subrole_pic = <img src="images/subrolepic_no.gif" />
  override def subrole_pic_true = <img src="images/subrolepic_foxbeliever.gif" />

  //override def ctext = <font color={subrole_color}>[狐信者]</font>
  override def toString(): String = "無副職"
}

object SubroleAlphaWolf extends SubroleData(SubroleEnum.ALPHAWOLF, "大狼", "#FF0000") {
  //override def subrole_intro = <span>[副職]你是狼的追隨者，人狼的勝利是你所希望的。</span>
  //override def subrole_intro = <img src="images/subrole_wolfbeliever.gif"/>
  //override def ctext = <font color={subrole_color}>[大狼]</font>
  override def subrole_pic = <img src="images/subrolepic_no.gif" />
  override def subrole_pic_true = <img src="images/subrolepic_alphawolf.gif" />

  override def toString(): String = "無副職"
}

object SubroleWiseWolf extends SubroleData(SubroleEnum.WISEWOLF, "智狼", "#FF0000") {
  //override def subrole_intro = <span>[副職]你是狼的追隨者，人狼的勝利是你所希望的。</span>
  //override def subrole_intro = <img src="images/subrole_wolfbeliever.gif"/>
  //override def ctext = <font color={subrole_color}>[智狼]</font>
  override def subrole_pic = <img src="images/subrolepic_no.gif" />
  override def subrole_pic_true = <img src="images/subrolepic_wisewolf.gif" />
  override def toString(): String = "無副職"
}

object SubroleSubpontiff extends SubroleData(SubroleEnum.SUBPONTIFF, "副教主", "#EEAA55") {
  //override def subrole_intro = <span>[副職]你是副教主，你知道教主同樣的資訊，但是教主死亡時會跟著離去，可以代替教主指揮。</span>
  override def subrole_intro = <img src="images/subrole_subpontiff.gif"/>
  override def subrole_pic = <img src="images/subrolepic_subpontiff.gif" />
}

object SubroleNoreligion extends SubroleData(SubroleEnum.NORELIGION, "無神論者", "#999999") {
  //override def subrole_intro = <span>[副職]你是無神論者，你無法入教。若教主拉你入教則會暫停一回合無法拉人入教。</span>
  override def subrole_intro = <img src="images/subrole_noreligion.gif"/>
  override def subrole_pic = <img src="images/subrolepic_noreligion.gif" />
}

object SubroleHashihime extends SubroleData(SubroleEnum.HASHIHIME, "橋姬", "#FF69B4") {
  override def subrole_pic = <img src="images/subrolepic_no.gif" />
}

