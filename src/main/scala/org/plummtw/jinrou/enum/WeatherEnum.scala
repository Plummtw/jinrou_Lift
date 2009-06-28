package org.plummtw.jinrou.enum

import org.plummtw.jinrou.data._

object WeatherEnum extends Enumeration {
  type WeatherEnum = Value
  
  val SUNNY             = Value("S")
  val CLOUDY            = Value("C")
  val RAINY             = Value("R")
  val SNOWY             = Value("N")
  val MISTY             = Value("M")

   def WEATHER_MAP   = scala.collection.immutable.TreeMap(
     SUNNY        -> "晴",
     CLOUDY       -> "陰",
     RAINY        -> "雨",
     SNOWY        -> "雪",
     MISTY        -> "霧"
   )

   def get_weather(role : WeatherEnum.Value) : String = {
    val result = WEATHER_MAP.get(role)
    return result.getOrElse(SUNNY.toString)
  }

  def get_weather(weather_string : String) : String = {
    return get_weather(valueOf(weather_string).getOrElse(SUNNY))
  }
}