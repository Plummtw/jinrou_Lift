/*
 * ItemEnum.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package org.plummtw.jinrou.enum

import org.plummtw.jinrou.data._

object ItemEnum extends Enumeration {
  type ItemEnum = Value

  val ITEM_NO_ITEM = Value("")

  val UNLUCKY_PURSE = Value("UP")
  val BLESS_STAFF  = Value("BS")
  val BLACK_FEATHER = Value("BF")
  val THIEF_SECRET = Value("TS")
  val VENTRILOQUIST = Value("VE")
  val DMESSAGE_SEAL = Value("DS")
  val MIRROR_SHIELD = Value("MS")
  val SHAMAN_CROWN = Value("SC")
  val WEATHER_ROD = Value("WR")
  val DEATH_NOTE = Value("DN")
  val PANDORA_BOX = Value("PB")
  val CUBIC_ARROW = Value("CA")
  val POPULATION_CENSUS = Value("PC")


  def ITEM_MAP   = scala.collection.immutable.TreeMap(
     ITEM_NO_ITEM  -> ItemNoItem,

     UNLUCKY_PURSE -> ItemUnluckyPurse,
     BLESS_STAFF -> ItemBlessStaff,
     BLACK_FEATHER -> ItemBlackFeather,
     THIEF_SECRET  -> ItemThiefSecret,
     VENTRILOQUIST -> ItemVentriloquist,
     DMESSAGE_SEAL -> ItemDMessageSeal,
     MIRROR_SHIELD -> ItemMirrorShield,
     SHAMAN_CROWN -> ItemShamanCrown,
     WEATHER_ROD  -> ItemWeatherRod,
     DEATH_NOTE -> ItemDeathNote,
     PANDORA_BOX -> ItemPandoraBox,
     CUBIC_ARROW -> ItemCubicArrow,
     POPULATION_CENSUS -> ItemPopulationCensus
  )

  def get_item(item : ItemEnum.Value) : ItemData = {
    val result = ITEM_MAP.get(item)
    if (result.isEmpty)
      println(item.toString + "is null")
    return result.getOrElse(ItemNoItem)
  }

  def get_item(item_string : String) : ItemData = {
    return get_item(valueOf(item_string).getOrElse(ITEM_NO_ITEM))
  }
}