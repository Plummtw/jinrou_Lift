package org.plummtw.jinrou.util

import scala.xml._
import net.liftweb._
import net.liftweb.mapper._
import http._
import js._
import util._
import S._
import SHtml._
import Helpers._


import org.plummtw.jinrou.model._
import org.plummtw.jinrou.enum._
import org.plummtw.jinrou.data._
import org.plummtw.jinrou.snippet._


object GameProcesser {
  // 死亡及死亡訊息
  def process_death(room_day:RoomDay, user: UserEntry, mtype: MTypeEnum.Value)  {
    if (user.live.is) {
      user.live(false)
      user.save

      val sys_mes = SystemMessage.create.roomday_id(room_day.id.is)
                    .actioner_id(user.id.is).mtype(mtype.toString)
      sys_mes.save
    } else {
      Log.warn("RoomDay : " + room_day.id.is.toString + " UserEntry " + user.id.is.toString +
                " Already Dead. New Death : " + mtype.toString)
    }
  }

  // 跟隨死亡
  def process_followers(room:Room, room_day:RoomDay, user_entrys: List[UserEntry]) = {
    // 若教主全掛，副教主連帶死亡
    val live_pontiff    = user_entrys.filter(x=>(x.current_role == RoleEnum.PONTIFF) && (x.live.is))
    val live_subpontiff = user_entrys.filter(x=>(x.subrole.is == SubroleEnum.SUBPONTIFF.toString) &&
                                                 (x.live.is))
    if (live_pontiff.length == 0) {
      live_subpontiff.foreach { subpontiff =>
        process_death(room_day, subpontiff, MTypeEnum.DEATH_SUBPONTIFF)
      }
    }

    // 若狐全掛，背德連帶死亡
    val live_fox      = user_entrys.filter(x=>(x.current_role == RoleEnum.FOX) && (x.live.is))
    val live_betrayer = user_entrys.filter(x=>((x.current_role == RoleEnum.BETRAYER) ||
                                               (x.current_role == RoleEnum.GODFAT) ||
                                               (x.subrole.is == SubroleEnum.FOXBELIEVER.toString)) &&
                                               (x.live.is))
    if (live_fox.length == 0) {
      live_betrayer.foreach { betrayer =>
        process_death(room_day, betrayer, MTypeEnum.DEATH_BETRAYER)
      }
    }

    // 人狼全掛，幼狼連帶死亡
    val live_werewolf = user_entrys.filter(x=>(x.current_role == RoleEnum.WEREWOLF) && (x.live.is))
    val live_wolfcub  = user_entrys.filter(x=>(x.current_role == RoleEnum.WOLFCUB)  && (x.live.is))
    if (live_werewolf.length == 0) {
      live_wolfcub.foreach { wolfcub =>
        process_death(room_day, wolfcub, MTypeEnum.DEATH_WOLFCUB)
      }
    }

    // 惡魔靈魂檢測
    val live_demons = user_entrys.filter(x=>(x.current_role == RoleEnum.DEMON) && (x.live.is) &&
                                           (x.action_point.is + user_entrys.filter(x=> !x.live.is).length > 35))
    if (live_demons.length != 0) {
      live_demons.foreach { live_demon =>
        process_death(room_day, live_demon, MTypeEnum.DEATH_SUDDEN)
      }
    }

    if ((room.room_flags.is.indexOf(RoomFlagEnum.SHIFTER_LOVER.toString) != -1) &&
        (room_day.day_no.is < 12)) {
      // 戀人檢測
      val live_lover = user_entrys.filter(x=>(x.user_flags.is.indexOf(UserEntryFlagEnum.LOVER.toString) != -1 ) && (x.live.is))
      if (live_lover.length == 1)
        process_death(room_day, live_lover(0), MTypeEnum.DEATH_LOVER)

       // 若教主全掛，副教主連帶死亡
      val live_pontiff2    = user_entrys.filter(x=>(x.current_role == RoleEnum.PONTIFF) && (x.live.is))
      val live_subpontiff2 = user_entrys.filter(x=>(x.subrole.is == SubroleEnum.SUBPONTIFF.toString) &&
                                                 (x.live.is))
      if (live_pontiff2.length == 0) {
        live_subpontiff2.foreach { subpontiff =>
          process_death(room_day, subpontiff, MTypeEnum.DEATH_SUBPONTIFF)
        }
      }

      // 若狐全掛，背德連帶死亡
      val live_fox2      = user_entrys.filter(x=>(x.current_role == RoleEnum.FOX) && (x.live.is))
      val live_betrayer2 = user_entrys.filter(x=>((x.current_role == RoleEnum.BETRAYER) ||
                                                 (x.current_role == RoleEnum.GODFAT)||
                                                 (x.subrole.is == SubroleEnum.FOXBELIEVER.toString)) &&
                                                 (x.live.is))
      if (live_fox2.length == 0) {
        live_betrayer2.foreach { betrayer =>
          process_death(room_day, betrayer, MTypeEnum.DEATH_BETRAYER)
        }
      }

      // 人狼全掛，幼狼連帶死亡
      val live_werewolf2 = user_entrys.filter(x=>(x.current_role == RoleEnum.WEREWOLF) && (x.live.is))
      val live_wolfcub2  = user_entrys.filter(x=>(x.current_role == RoleEnum.WOLFCUB)  && (x.live.is))
      if (live_werewolf2.length == 0) {
        live_wolfcub2.foreach { wolfcub =>
          process_death(room_day, wolfcub, MTypeEnum.DEATH_WOLFCUB)
        }
      }

      // 戀人檢測
      val live_lover2 = user_entrys.filter(x=>(x.user_flags.is.indexOf(UserEntryFlagEnum.LOVER.toString) != -1 ) && (x.live.is))
      if (live_lover2.length == 1)
        process_death(room_day, live_lover2(0), MTypeEnum.DEATH_LOVER)

      // 惡魔靈魂檢測
      val live_demons2 = user_entrys.filter(x=>(x.current_role == RoleEnum.DEMON) && (x.live.is) &&
                                               (x.action_point.is + user_entrys.filter(x=> !x.live.is).length > 35))
      if (live_demons2.length != 0) {
        live_demons2.foreach { live_demon =>
          process_death(room_day, live_demon, MTypeEnum.DEATH_SUDDEN)
        }
      }

    }
  }

  // 分配職業
  def dispatch_role(room: Room, user_entrys:List[UserEntry]) {
  
    // 這邊我為了方便處理起見，不使用 Scala 的 List，改用 Java 的 List
    val user_entrys_size = user_entrys.length
    
    // 先產生職業清單
    var role_array : java.util.LinkedList[String] = new java.util.LinkedList()
    if (user_entrys_size >=  8) role_array.add(RoleEnum.AUGURER.toString)
    if (user_entrys_size >=  9) role_array.add(RoleEnum.NECROMANCER.toString)
    if (user_entrys_size >= 10) role_array.add(RoleEnum.MADMAN.toString)
    if (user_entrys_size >= 11) role_array.add(RoleEnum.HUNTER.toString)
    if (user_entrys_size >= 13) {
      role_array.add(RoleEnum.GEMINI.toString)
      role_array.add(RoleEnum.GEMINI.toString)
    }
    if ((user_entrys_size >= 14) && (room.room_flags.is.indexOf(RoomFlagEnum.ROLE_INHERITER.toString) != -1))
      role_array.add(RoleEnum.INHERITER.toString)
    if (user_entrys_size >= 15) role_array.add(RoleEnum.FOX.toString)
    if ((user_entrys_size >= 17) && (room.room_flags.is.indexOf(RoomFlagEnum.ROLE_CLERIC.toString) != -1))
      role_array.add(RoleEnum.CLERIC.toString)
    if ((user_entrys_size >= 18) && (room.room_flags.is.indexOf(RoomFlagEnum.ROLE_HERBALIST.toString) != -1))
      role_array.add(RoleEnum.HERBALIST.toString)
    if ((user_entrys_size >= 20) && (room.room_flags.is.indexOf(RoomFlagEnum.ROLE_POISONER.toString) != -1))
      role_array.add(RoleEnum.POISONER.toString)
    if ((user_entrys_size >= 20) && (room.room_flags.is.indexOf(RoomFlagEnum.ROLE_BETRAYER.toString) != -1) &&
                                    (room.room_flags.is.indexOf(RoomFlagEnum.FOX_OPTION1.toString) == -1) )
      role_array.add(RoleEnum.BETRAYER.toString)
    if ((user_entrys_size >= 21) && (room.room_flags.is.indexOf(RoomFlagEnum.ROLE_RUNNER.toString) != -1))
      role_array.add(RoleEnum.RUNNER.toString)
    if ((user_entrys_size >= 22) && (room.room_flags.is.indexOf(RoomFlagEnum.ROLE_SORCEROR.toString) != -1))
      role_array.add(RoleEnum.SORCEROR.toString)
    if ((user_entrys_size >= 23) && (room.room_flags.is.indexOf(RoomFlagEnum.ROLE_WOLFCUB.toString) != -1))
      role_array.add(RoleEnum.WOLFCUB.toString)
    if ((user_entrys_size >= 23) && (room.room_flags.is.indexOf(RoomFlagEnum.ROLE_DEMON.toString) != -1))
      role_array.add(RoleEnum.DEMON.toString)
    if ((user_entrys_size >= 24) && (room.room_flags.is.indexOf(RoomFlagEnum.ROLE_SCHOLAR.toString) != -1))
      role_array.add(RoleEnum.SCHOLAR.toString)
    if ((user_entrys_size >= 25) && (room.room_flags.is.indexOf(RoomFlagEnum.ROLE_GODFAT.toString) != -1))
      role_array.add(RoleEnum.GODFAT.toString)
    if ((user_entrys_size >= 25) && (room.room_flags.is.indexOf(RoomFlagEnum.ROLE_SHIFTER.toString) != -1))
      role_array.add(RoleEnum.SHIFTER.toString)
    if ((user_entrys_size >= 25) && (room.room_flags.is.indexOf(RoomFlagEnum.ROLE_PONTIFF.toString) != -1))
      role_array.add(RoleEnum.PONTIFF.toString)

    val werewolf_number = ((user_entrys_size + 2)/5) -
      (if ((user_entrys_size >= 23) && (room.room_flags.is.indexOf(RoomFlagEnum.ROLE_WOLFCUB.toString) != -1)) 1 else 0)
    for (i <- 1 to werewolf_number)   // 狼數公式
      role_array.add(RoleEnum.WEREWOLF.toString)
    
    for (i <- 1 to (user_entrys_size - role_array.size())) // 剩下的補村民
      role_array.add(RoleEnum.VILLAGER.toString)

      
    // 設定玩家優先順位
    var user_no_array : java.util.LinkedList[Int] = new java.util.LinkedList()
    for (i <- 1 to user_entrys_size)
      user_no_array.add(i)
      
    java.util.Collections.shuffle(user_no_array)
    
    user_entrys.foreach(user_entry =>
      user_entry.user_no(user_no_array.removeFirst()).user_flags(user_entry.role.is).role(""))

    val user_entrys_ordered = user_entrys.sort(_.user_no.is < _.user_no.is)

    // 第一次先看看有沒有希望職業
    val random = new Random()
    user_entrys_ordered.foreach(user_entry =>
      if (role_array.contains(user_entry.user_flags.is) && (random.nextInt(4) != 0)) {
        user_entry.role(user_entry.user_flags.is).user_flags("")
        role_array.remove(user_entry.role.is)
      }
    )

    java.util.Collections.shuffle(role_array)
    
    // 然後設定剩下的職業
    user_entrys_ordered.foreach(user_entry =>
      if (user_entry.role.is == "") {
        user_entry.role(role_array.removeFirst()).user_flags("")
      }
    )

    // 先設定人狼專用副職業
    var subrole_array_werewolf : java.util.LinkedList[Int] = new java.util.LinkedList()
    val user_werewolf = user_entrys.filter(x=>(x.role.is == RoleEnum.WEREWOLF.toString))
    user_werewolf.foreach{i =>
      subrole_array_werewolf.add(i.user_no.is)
    }
    java.util.Collections.shuffle(subrole_array_werewolf)

    if ((user_entrys_size >= 20) && (room.room_flags.is.indexOf(RoomFlagEnum.SUBROLE_ALPHAWOLF.toString) != -1)) {
      val sub_alphawolf = subrole_array_werewolf.removeFirst()
      user_entrys.filter(_.user_no.is == sub_alphawolf)(0).subrole(SubroleEnum.ALPHAWOLF.toString)
    }
    if ((user_entrys_size >= 20) && (room.room_flags.is.indexOf(RoomFlagEnum.SUBROLE_WISEWOLF.toString) != -1)) {
      val sub_wisewolf = subrole_array_werewolf.removeFirst()
      user_entrys.filter(_.user_no.is == sub_wisewolf)(0).subrole(SubroleEnum.WISEWOLF.toString)
    }

    // 設定人側專用副職業
    if (room.room_flags.is.indexOf(RoomFlagEnum.SUBROLE_FAKEAUGURER.toString) != -1) {
      var subrole_array_villager : java.util.LinkedList[Int] = new java.util.LinkedList()
      val user_villager_side = user_entrys.filter(x=>(RoleEnum.get_role(x.role.is).role_side == RoomVictoryEnum.VILLAGER_WIN))
      user_villager_side.foreach{i =>
        subrole_array_villager.add(i.user_no.is)
      }
      java.util.Collections.shuffle(subrole_array_villager)
      val sub_fakeaugurer = subrole_array_villager.removeFirst()
      user_entrys.filter(_.user_no.is == sub_fakeaugurer)(0).subrole(SubroleEnum.FAKEAUGURER.toString)
    }
    
    // 設定副職業
    var subrole_array : java.util.LinkedList[Int] = new java.util.LinkedList()
    val user_normal_subrole = user_entrys.filter(x=>(x.subrole.is == ""))
    user_normal_subrole.foreach{i =>
      subrole_array.add(i.user_no.is)
    }
    //for (i <- 1 to user_entrys_size)
    //  subrole_array.add(i)
    java.util.Collections.shuffle(subrole_array)

    var subrole_array_plus : java.util.LinkedList[String] = new java.util.LinkedList()
    if ((user_entrys_size >= 16) && (room.room_flags.is.indexOf(RoomFlagEnum.SUBROLE_PLUS.toString) != -1)) {
      subrole_array_plus.add(SubroleEnum.AUTHORITY.toString)
      subrole_array_plus.add(SubroleEnum.DECIDER.toString)

      if ((user_entrys_size >= 16) && (room.room_flags.is.indexOf(RoomFlagEnum.SUBROLE_AVENGER.toString) != -1))
         subrole_array_plus.add(SubroleEnum.AVENGER.toString)

      if ((user_entrys_size >= 20) && 
          ((room.room_flags.is.indexOf(RoomFlagEnum.SUBROLE_MEMORYLOSS4.toString) != -1) ||
           (room.room_flags.is.indexOf(RoomFlagEnum.SUBROLE_MEMORYLOSS4_2.toString) != -1)))
         subrole_array_plus.add(SubroleEnum.MEMORYLOSS4.toString)

      if ((user_entrys_size >= 24) && (room.room_flags.is.indexOf(RoomFlagEnum.SUBROLE_MEMORYLOSS6.toString) != -1))
         subrole_array_plus.add(SubroleEnum.MEMORYLOSS6.toString)

      if ((user_entrys_size >= 25) && (room.room_flags.is.indexOf(RoomFlagEnum.SUBROLE_MEMORYLOSS8.toString) != -1))
         subrole_array_plus.add(SubroleEnum.MEMORYLOSS8.toString)

      if ((user_entrys_size >= 22) && (room.room_flags.is.indexOf(RoomFlagEnum.SUBROLE_SUDDENDEATH.toString) != -1))
         subrole_array_plus.add(SubroleEnum.SUDDENDEATH.toString)

      //if ((user_entrys_size >= 18) && (room.room_flags.is.indexOf(RoomFlagEnum.SUBROLE_WOLFBELIEVER.toString) != -1))
      //   subrole_array_plus.add(SubroleEnum.WOLFBELIEVER.toString)
    }

    val subrole_plus =
      if (subrole_array_plus.size() > 0)
        subrole_array_plus.get(new Random().nextInt(subrole_array_plus.size()))
      else
        null


    if (user_entrys_size >= 16) {
      val subrole_authority = subrole_array.removeFirst()
      user_entrys.filter(_.user_no.is == subrole_authority)(0).subrole(SubroleEnum.AUTHORITY.toString)
      
      val subrole_decider = subrole_array.removeFirst()
      user_entrys.filter(_.user_no.is == subrole_decider)(0).subrole(SubroleEnum.DECIDER.toString)

      if (room.room_flags.is.indexOf(RoomFlagEnum.SUBROLE_AVENGER.toString) != -1) {
        val avenger = subrole_array.removeFirst()
        user_entrys.filter(_.user_no.is == avenger)(0).subrole(SubroleEnum.AVENGER.toString)
      }
    }

    if ((user_entrys_size >= 20) && (room.room_flags.is.indexOf(RoomFlagEnum.SUBROLE_MEMORYLOSS4.toString) != -1)) {
      val memory_loss4 = subrole_array.removeFirst()
      user_entrys.filter(_.user_no.is == memory_loss4)(0).subrole(SubroleEnum.MEMORYLOSS4.toString)
    }

    if ((user_entrys_size >= 20) && (room.room_flags.is.indexOf(RoomFlagEnum.SUBROLE_MEMORYLOSS4_2.toString) != -1)) {
      val memory_loss4 = subrole_array.removeFirst()
      user_entrys.filter(_.user_no.is == memory_loss4)(0).subrole(SubroleEnum.MEMORYLOSS4.toString)
    }

    if ((user_entrys_size >= 24) && (room.room_flags.is.indexOf(RoomFlagEnum.SUBROLE_MEMORYLOSS6.toString) != -1)) {
      val memory_loss6 = subrole_array.removeFirst()
      user_entrys.filter(_.user_no.is == memory_loss6)(0).subrole(SubroleEnum.MEMORYLOSS6.toString)
    }

    if ((user_entrys_size >= 25) && (room.room_flags.is.indexOf(RoomFlagEnum.SUBROLE_MEMORYLOSS8.toString) != -1)) {
      val memory_loss8 = subrole_array.removeFirst()
      user_entrys.filter(_.user_no.is == memory_loss8)(0).subrole(SubroleEnum.MEMORYLOSS8.toString)
    }

    if ((subrole_plus != null) && (subrole_plus != SubroleEnum.SUDDENDEATH.toString) &&
        (subrole_plus != SubroleEnum.WOLFBELIEVER.toString)) {
      val subrole_plus_no = subrole_array.removeFirst()
      user_entrys.filter(_.user_no.is == subrole_plus_no)(0).subrole(subrole_plus)
    }

    // 移除 Fox Demon Pontiff
    val user_no_for_remove = user_entrys.filter(x=>(x.role.is == RoleEnum.FOX.toString) ||
                                 (x.role.is == RoleEnum.DEMON.toString) ||
                                 (x.role.is == RoleEnum.PONTIFF.toString)).map(_.user_no.is)
    var subrole_array2 : java.util.LinkedList[Int] = new java.util.LinkedList()
    user_no_for_remove.foreach(subrole_array2.add(_))
    subrole_array.removeAll(subrole_array2)

    if ((user_entrys_size >= 22) && (room.room_flags.is.indexOf(RoomFlagEnum.SUBROLE_SUDDENDEATH.toString) != -1)) {
      val sudden_death = subrole_array.removeFirst()
      user_entrys.filter(_.user_no.is == sudden_death)(0).subrole(SubroleEnum.SUDDENDEATH.toString)
    }

    if ((user_entrys_size >= 18) && (room.room_flags.is.indexOf(RoomFlagEnum.SUBROLE_WOLFBELIEVER.toString) != -1)) {
      val wolf_believer = subrole_array.removeFirst()
      user_entrys.filter(_.user_no.is == wolf_believer)(0).subrole(SubroleEnum.WOLFBELIEVER.toString)
    }

    if ((subrole_plus != null) && ((subrole_plus == SubroleEnum.SUDDENDEATH.toString) ||
        (subrole_plus == SubroleEnum.WOLFBELIEVER.toString))) {
      val subrole_plus_no = subrole_array.removeFirst()
      user_entrys.filter(_.user_no.is == subrole_plus_no)(0).subrole(subrole_plus)
    }

    if ((user_entrys_size >= 25) &&
        (room.room_flags.is.indexOf(RoomFlagEnum.ROLE_PONTIFF.toString) != -1) &&
        (room.room_flags.is.indexOf(RoomFlagEnum.SUBROLE_SUBPONTIFF.toString) != -1)) {
      // 設定副教主
      var subrole_array_subpontiff : java.util.LinkedList[Int] = new java.util.LinkedList()
      val user_no_scholar = user_entrys.filter(x=>(x.role.is != RoleEnum.PONTIFF.toString) &&
                                                 (x.role.is != RoleEnum.SCHOLAR.toString) &&
                                                 (x.role.is != RoleEnum.FOX.toString) &&
                                                 (x.role.is != RoleEnum.DEMON.toString) &&
                                                 (x.subrole.is == "")).map(_.user_no.is)
      user_no_scholar.foreach{i =>
        subrole_array_subpontiff.add(i)
      }
      java.util.Collections.shuffle(subrole_array_subpontiff)
      val sub_pontiff = subrole_array_subpontiff.removeFirst()
      user_entrys.filter(_.user_no.is == sub_pontiff)(0).subrole(SubroleEnum.SUBPONTIFF.toString).user_flags(UserEntryFlagEnum.RELIGION.toString)

      // 設定無神論者
      var subrole_array_noreligion : java.util.LinkedList[Int] = new java.util.LinkedList()
      val user_no_cleric = user_entrys.filter(x=>(x.role.is != RoleEnum.PONTIFF.toString) &&
                                                 (x.role.is != RoleEnum.CLERIC.toString) &&
                                                 (x.subrole.is == "")).map(_.user_no.is)
      user_no_cleric.foreach{i =>
        subrole_array_noreligion.add(i)
      }
      java.util.Collections.shuffle(subrole_array_noreligion)
      val sub_noreligion = subrole_array_noreligion.removeFirst()
      user_entrys.filter(_.user_no.is == sub_noreligion)(0).subrole(SubroleEnum.NORELIGION.toString).user_flags(UserEntryFlagEnum.NORELIGION.toString)
    }
    
    // 如果替身君是 狼 狐 毒 惡魔 則要換掉
    if ((user_entrys(0).role.is == RoleEnum.WEREWOLF.toString)||
        (user_entrys(0).role.is == RoleEnum.WOLFCUB.toString)||
        (user_entrys(0).role.is == RoleEnum.FOX.toString)||
        (user_entrys(0).role.is == RoleEnum.POISONER.toString)||
        (user_entrys(0).role.is == RoleEnum.DEMON.toString) ||
        ((user_entrys(0).role.is == RoleEnum.PONTIFF.toString) &&
         (room.room_flags.is.indexOf(RoomFlagEnum.SUBROLE_SUBPONTIFF.toString) != -1))) {
      val temp_user = user_entrys_ordered.filter(x => 
        (x.role.is != RoleEnum.WEREWOLF.toString) &&
        (x.role.is != RoleEnum.WOLFCUB.toString) &&
        (x.role.is != RoleEnum.FOX.toString) &&
        (x.role.is != RoleEnum.POISONER.toString) &&
        (x.role.is != RoleEnum.DEMON.toString) &&
        ((x.role.is != RoleEnum.PONTIFF.toString) ||
         (room.room_flags.is.indexOf(RoomFlagEnum.SUBROLE_SUBPONTIFF.toString) == -1))
      )
      val temp_role = temp_user(0).role.is
      val temp_subrole = temp_user(0).subrole.is
      val temp_user_flags = temp_user(0).user_flags.is
      temp_user(0).role(user_entrys(0).role.is)
      temp_user(0).subrole(user_entrys(0).subrole.is)
      temp_user(0).user_flags(user_entrys(0).user_flags.is)
      user_entrys(0).role(temp_role)
      user_entrys(0).subrole(temp_subrole)
      user_entrys(0).user_flags(temp_user_flags)
    }

    // 教主選項 2
    if ((room.room_flags.is.indexOf(RoomFlagEnum.ROLE_PONTIFF.toString) != -1) &&
        (room.room_flags.is.indexOf(RoomFlagEnum.PONTIFF_OPTION2.toString) != -1 )) {
      user_entrys.filter(x=>(x.role.is == RoleEnum.CLERIC.toString)).foreach(_.user_flags(UserEntryFlagEnum.RELIGION.toString))
      user_entrys.filter(x=>(x.role.is == RoleEnum.SCHOLAR.toString)).foreach(_.user_flags(UserEntryFlagEnum.NORELIGION.toString))
    }

    // 教主選項 3
    if ((room.room_flags.is.indexOf(RoomFlagEnum.ROLE_PONTIFF.toString) != -1) &&
        (room.room_flags.is.indexOf(RoomFlagEnum.PONTIFF_OPTION3.toString) != -1 )) {
      user_entrys.filter(x=>(x.role.is == RoleEnum.PONTIFF.toString)).foreach(_.user_flags(UserEntryFlagEnum.PONTIFF_AURA.toString))
    }

    // 背德初期票數
    user_entrys.filter(x=>(x.role.is == RoleEnum.BETRAYER.toString)).foreach(_.action_point(1))
  }

  def process_start_game(room:Room, user_entrys:List[UserEntry]) = {
    GameProcesser.dispatch_role(room, user_entrys)
    user_entrys.foreach(_.save)

    // 加入大廳
    val new_day = RoomDay.create.room_id(room.id.is).day_no(1).vote_time(1)
    new_day.save()

    // 產生人數字串
    val role_list = RoleEnum.ROLE_MAP.keys.toList.filter(_ != RoleNone)
    var role_text = new StringBuffer("")
    role_list.foreach{role =>
      var role_number = user_entrys.filter(_.current_role == role).length

      // 這邊要處理妖狐指定背德時的特殊處理
      if ((user_entrys.length >= 20) &&
          (room.room_flags.is.indexOf(RoomFlagEnum.ROLE_BETRAYER.toString) != -1) &&
          (room.room_flags.is.indexOf(RoomFlagEnum.FOX_OPTION1.toString) != -1)) {
        if (role == RoleEnum.BETRAYER)
          role_number = role_number + 1
        else if (role == RoleEnum.VILLAGER)
          role_number = role_number - 1
      }

      if (role_number > 0) {
        role_text.append("　")
        role_text.append(RoleEnum.get_role(role).role_name)
        role_text.append(" ")
        role_text.append(role_number.toString)
      }
    }

    val subrole_list = SubroleEnum.SUBROLE_MAP.keys.toList.filter(_ != SubroleNone)
    subrole_list.foreach{subrole =>
      val subrole_number = user_entrys.filter(_.subrole.is == subrole.toString).length
      if (subrole_number > 0) {
        role_text.append("　(")
        val subrole_data = SubroleEnum.get_subrole(subrole)
        val subrole_name = subrole_data.subrole_name
        /*  if (subrole_data == SubroleAlphaWolf)
            "大狼"
          else if (subrole_data == SubroleFakeAugurer)
            "冒牌占"
          else
            subrole_data.subrole_name */
        role_text.append(subrole_name)
        role_text.append(" ")
        role_text.append(subrole_number.toString)
        role_text.append(")")
      }
    }

    val talk = Talk.create.roomday_id(new_day.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                           .message(role_text.toString).font_type("12")
    talk.save()

    room.status(RoomStatusEnum.PLAYING.toString)
    room.save()
  }
  
  def process_day(room:Room, room_day:RoomDay, user_entrys:List[UserEntry], votes:List[Vote], voted_player:UserEntry) : RoomVictoryEnum.Value = {
    //var users_for_save : List[UserEntry] = List()
    var talks_for_save : List[Talk]      = List()

    // 投哥德法的人
    votes.foreach {vote =>
      val actioner_list =  user_entrys.filter(_.id.is == vote.actioner_id.is)
      val actionee_list =  user_entrys.filter(_.id.is == vote.actionee_id.is)
      if ((actioner_list.length != 0) && (actionee_list.length != 0)) {
        val actioner = actioner_list(0)
        val actionee = actionee_list(0)

        if ((actionee.current_role == RoleEnum.GODFAT) &&
            ((actionee.user_flags.is.indexOf(UserEntryFlagEnum.GODFAT_SPECIAL1.toString) != -1) ||
             (actionee.user_flags.is.indexOf(UserEntryFlagEnum.GODFAT_SPECIAL3.toString) != -1)) &&
            (actioner.user_flags.is.indexOf(UserEntryFlagEnum.GODFAT_TARGETED.toString) == -1)) {
          actioner.user_flags(actioner.user_flags.is + UserEntryFlagEnum.GODFAT_TARGETED.toString)
          actioner.save
        }
      }
    }
    
    // 被投的人吊死
    if (voted_player == WaterElemental) {
      val sys_mes = SystemMessage.create.roomday_id(room_day.id.is)
                    .actioner_id(0)
      sys_mes.mtype(MTypeEnum.DEATH_HANGED.toString)
      sys_mes.save

      user_entrys.filter(_.current_role == RoleEnum.ARCHMAGE).foreach { archmage =>
        archmage.user_flags(archmage.user_flags.is + UserEntryFlagEnum.WATER_ELEM_USED.toString)
        archmage.save()
      }
    } else
      process_death(room_day, voted_player, MTypeEnum.DEATH_HANGED)

    // 絕望 Counter 下降
    val death0s  =  user_entrys.filter(_.user_flags.is.indexOf(UserEntryFlagEnum.DEATH_0.toString) != -1)
    death0s.foreach { suddendeath  =>
       if (suddendeath.live.is)
         process_death(room_day, suddendeath, MTypeEnum.DEATH_SUDDEN)
    }
    val death1s  =  user_entrys.filter(_.user_flags.is.indexOf(UserEntryFlagEnum.DEATH_1.toString) != -1)
    death1s.foreach { suddendeath =>
       suddendeath.user_flags(suddendeath.user_flags.is.replace(UserEntryFlagEnum.DEATH_1.toString,UserEntryFlagEnum.DEATH_0.toString))
       suddendeath.save
    }
    val death2s  =  user_entrys.filter(_.user_flags.is.indexOf(UserEntryFlagEnum.DEATH_2.toString) != -1)
    death2s.foreach { suddendeath =>
       suddendeath.user_flags(suddendeath.user_flags.is.replace(UserEntryFlagEnum.DEATH_2.toString,UserEntryFlagEnum.DEATH_1.toString))
       suddendeath.save
    }

    if (voted_player != WaterElemental) {
      // 如果被吊死的人是幼狼的話
      if (voted_player.current_role == RoleEnum.WOLFCUB) {
        val talk = Talk.create.mtype(MTypeEnum.MESSAGE_GENERAL.toString).message("＜＜幼狼被吊，人狼狂暴＞＞").font_type("12")
        talks_for_save = talks_for_save ::: List(talk)
      }

      // 如果被吊死的人是哥德法的話
      else if ((voted_player.current_role == RoleEnum.GODFAT) && 
               (voted_player.user_flags.is.indexOf(UserEntryFlagEnum.GODFAT_SPECIAL2.toString) == -1) &&
               (voted_player.user_flags.is.indexOf(UserEntryFlagEnum.GODFAT_SPECIAL3.toString) == -1)) {
        val avenger_votes = votes.filter(_.actioner_id.is == voted_player.id.is)
        if (avenger_votes.length != 0) {
          val avenger_vote = avenger_votes(0)
          val target_list  = user_entrys.filter(_.id.is == avenger_vote.actionee_id.is)
          val target =
            if (target_list.length == 0)
              user_entrys.filter(x=>(x.live.is) && (x.current_role == RoleEnum.ARCHMAGE))(0)
            else
              target_list(0)
          //if ((target.live.is) && (target.subrole.is == "")) {
          if (target.live.is) {
             // target.subrole(SubroleEnum.SUDDENDEATH.toString)
             target.user_flags(target.user_flags.is + UserEntryFlagEnum.DEATH_2.toString)
             target.save
          }
        }
      }

      // 如果被吊死的人是復仇者的話
      if ((room_day.weather.is != WeatherEnum.RAINY.toString) && (voted_player.subrole.is == SubroleEnum.AVENGER.toString)) {
        val avenger_votes = votes.filter(_.actioner_id.is == voted_player.id.is)
        if (avenger_votes.length != 0) {
          val avenger_vote = avenger_votes(0)
          val target_list  = user_entrys.filter(_.id.is == avenger_vote.actionee_id.is)
          if (target_list.length == 0) {
            val target = user_entrys.filter(x=>(x.live.is) && (x.current_role == RoleEnum.ARCHMAGE))(0)
            target.user_flags( target.user_flags.is + UserEntryFlagEnum.WATER_ELEM_USED.toString )
            target.save

            val sys_mes = SystemMessage.create.roomday_id(room_day.id.is)
                         .actioner_id(0).mtype(MTypeEnum.DEATH_SUDDEN.toString)
            sys_mes.save

          } else {
            val target = target_list(0)
            if (target.live.is) {
               process_death(room_day, target, MTypeEnum.DEATH_SUDDEN)
            }
          }
        }
      }
    
      // 繼承者繼承
      val inheriter_vote = SystemMessage.findAll(By(SystemMessage.roomday_id, room_day.id.is),
                                                 By(SystemMessage.mtype, MTypeEnum.VOTE_INHERITER.toString))
      //val inheriter_vote = votes.filter(_.mtype.is == MTypeEnum.VOTE_INHERITER.toString)
      inheriter_vote.foreach { vote =>
        val actioner = user_entrys.filter(_.id.is == vote.actioner_id.is)(0)
        val target   = user_entrys.filter(_.id.is == vote.actionee_id.is)(0)
        val target_role_str = target.role.is.substring(0,1)
        val target_role_str2 =
          if ((room.room_flags.is.indexOf(RoomFlagEnum.ROLE_AUGHUNTER.toString) != -1) &&
              ((target_role_str == RoleEnum.AUGURER.toString) ||
               (target_role_str == RoleEnum.HUNTER.toString)))
            RoleEnum.AUGHUNTER.toString
          else
            target_role_str
      
        if ((actioner.live.is) && (target.id.is == voted_player.id.is)) {
          if (target_role_str2 != RoleEnum.INHERITER.toString) {
            actioner.role(target_role_str2 + actioner.role.is)
            actioner.save
          }
      
          val target_role_enum = RoleEnum.get_role(target_role_str2)
          val talk_sentence =
            if (room.room_flags.is.indexOf(RoomFlagEnum.INHERITER_REVEAL.toString) != -1)
              "＜＜繼承者繼承成功＞＞ (" + target_role_enum.toString + ")"
            else
              "＜＜繼承者繼承成功＞＞"

          val talk = Talk.create.mtype(MTypeEnum.MESSAGE_GENERAL.toString).message(talk_sentence).font_type("12")
          talks_for_save = talks_for_save ::: List(talk)
        }
      }
    
      // 吊到毒？
      if (voted_player.current_role == RoleEnum.POISONER) {
        val live_player = user_entrys.filter(x=>(x.live.is))
        val random_player = live_player((new Random()).nextInt(live_player.length))
        process_death(room_day, random_player, MTypeEnum.DEATH_POISON_D)
      }
    }

    // 處理自投者
    //val votes_auto = votes.filter(_.vote_flags.is.indexOf(VoteFlagEnum.AUTO.toString) != -1)
    val votes_auto = votes.filter(_.vote_flags.is.indexOf(VoteFlagEnum.AUTO.toString) != -1)
    votes_auto.foreach { vote_auto =>
      val auto_player = user_entrys.filter(_.id.is == vote_auto.actioner_id.is)(0)
      if (auto_player.user_flags.is.indexOf(UserEntryFlagEnum.AUTOVOTED.toString) == -1)  {
        auto_player.user_flags(auto_player.user_flags.is + UserEntryFlagEnum.AUTOVOTED.toString)
        auto_player.save
      }
      else if (auto_player.live.is) {
        process_death(room_day, auto_player, MTypeEnum.DEATH_SUDDEN)
      }
      
    }
    
    // 處理暴斃者
    if ((room_day.weather.is != WeatherEnum.RAINY.toString) && (room_day.day_no.is == 12)) {
      val live_suddendeaths = user_entrys.filter(x=>(x.subrole.is == SubroleEnum.SUDDENDEATH.toString) && (x.live.is))
      live_suddendeaths.foreach { live_suddendeath =>
        process_death(room_day, live_suddendeath, MTypeEnum.DEATH_SUDDEN)
      }
    }
    
    // 若狐全掛，背德連帶死亡
    // 若狼全掛，幼狼連帶死亡
    process_followers(room, room_day, user_entrys)

    // 計算背德票數
    if ((room.room_flags.is.indexOf(RoomFlagEnum.BETRAYER_OPTION1.toString) != -1) ||
        (room.room_flags.is.indexOf(RoomFlagEnum.BETRAYER_OPTION2.toString) != -1) ||
        (room.room_flags.is.indexOf(RoomFlagEnum.BETRAYER_OPTION3.toString) != -1)) {
      votes.foreach{ vote=>
         val actioner = user_entrys.filter(_.id.is == vote.actioner_id.is)(0)
         if ((actioner.current_role == RoleEnum.BETRAYER) && actioner.live.is) {
           actioner.action_point(actioner.action_point.is + (vote.vote_number.is /2 ))
           actioner.save
         }
      }
    }
    
    //users_for_save.removeDuplicates.foreach( _.save )

    //計算天氣變化
    var new_weather = room_day.weather.is
    if ((room.room_flags.is.indexOf(RoomFlagEnum.WEATHER.toString) != -1) && (new Random().nextInt(2) == 0)) {
      val weather_int = new Random().nextInt(5)
      new_weather = weather_int match
      {
        case 0 => WeatherEnum.SUNNY.toString
        case 1 => WeatherEnum.CLOUDY.toString
        case 2 => WeatherEnum.RAINY.toString
        case 3 => WeatherEnum.SNOWY.toString
        case 4 => WeatherEnum.MISTY.toString
        case x => WeatherEnum.SUNNY.toString
      }

    }

    // 新的一日
    val new_room_day  = RoomDay.create.room_id(room.id.is).day_no(room_day.day_no.is + 1)
                        .vote_time(1).weather(new_weather)
    new_room_day.save                    

    // 白天到晚上時不用加入 SystemMessage
    //votes.foreach { vote =>
    //  val sys_mes = SystemMessage.create.roomday_id(new_room_day.id.is)
    //                .actioner_id(vote.actioner_id.is).actionee_id(vote.actionee_id.is)
    //                .mtype(vote.mtype.is)
    //  sys_mes.save
    //}
    
    // 加入繼承者訊息
    talks_for_save.foreach { talk_for_save =>
      talk_for_save.roomday_id(new_room_day.id.is)
      talk_for_save.save
    }
    
    // 進入下一天
    val talk = Talk.create.roomday_id(new_room_day.id.is).mtype(MTypeEnum.MESSAGE_NIGHT.toString)
    talk.save
    //room.addToRoom_days(new_day)
    //room.save(flush:true)
    
    // 吊到惡魔？
    if ((voted_player != WaterElemental) && (voted_player.current_role == RoleEnum.DEMON) &&
        (voted_player.user_flags.is.indexOf(UserEntryFlagEnum.BITED.toString) != -1)) {
      // 惡魔直接獲勝
      return RoomVictoryEnum.DEMON_WIN
    }
    
    return RoomVictoryEnum.NONE
  }
  
  def process_night(room:Room, room_day:RoomDay, user_entrys:List[UserEntry], votes:List[Vote]) = {
    //var users_for_save : List[UserEntry] = List()
    var talks_for_save : List[Talk]      = List()
    var votes_for_save : List[Vote]      = List()
    
    // 妖狐指定背德
    val fox_votes = votes.filter(_.mtype.is == MTypeEnum.VOTE_FOX.toString)
    fox_votes.foreach { vote =>
      val target = user_entrys.filter(_.id.is == vote.actionee_id.is)(0)
      target.role(RoleEnum.BETRAYER.toString)
      target.action_point(1)
      target.save
    }

    // 模仿師模仿
    val shifter_votes = votes.filter(_.mtype.is == MTypeEnum.VOTE_SHIFTER.toString)
    shifter_votes.foreach { vote =>
      val actioner = user_entrys.filter(_.id.is == vote.actioner_id.is)(0)
      val target   = user_entrys.filter(_.id.is == vote.actionee_id.is)(0)
      val target_role_str = target.role.is.substring(0,1)
      val target_role_str2 =
        if ((room.room_flags.is.indexOf(RoomFlagEnum.ROLE_AUGHUNTER.toString) != -1) &&
            ((target_role_str == RoleEnum.AUGURER.toString) ||
             (target_role_str == RoleEnum.HUNTER.toString)))
          RoleEnum.AUGHUNTER.toString
        else
          target_role_str
      if (room.room_flags.is.indexOf(RoomFlagEnum.SHIFTER_LOVER.toString) != -1) {
        target.user_flags(target.user_flags.is + UserEntryFlagEnum.LOVER.toString)
        target.save
        actioner.user_flags(actioner.user_flags.is + UserEntryFlagEnum.LOVER.toString)
      }
      actioner.role(target_role_str2 + actioner.role.is)
      actioner.save

      val target_role_enum =
        RoleEnum.get_role(target_role_str2)

      val talk_sentence =
        if (room.room_flags.is.indexOf(RoomFlagEnum.SHIFTER_REVEAL.toString) != -1)
          "＜＜模仿師模仿成功＞＞ (" + target_role_enum.toString + ")"
        else
          "＜＜模仿師模仿成功＞＞"

      val talk = Talk.create.mtype(MTypeEnum.MESSAGE_GENERAL.toString).message(talk_sentence).font_type("12")
      talks_for_save = talks_for_save ::: List(talk)
    }


    // 背德變化
    val betrayer_changes_votes = votes.filter(_.mtype.is == MTypeEnum.VOTE_BETRAYER_CHANGE.toString)
    betrayer_changes_votes.foreach { vote =>
      val actioner = user_entrys.filter(_.id.is == vote.actioner_id.is)(0)
      val target   = user_entrys.filter(_.id.is == vote.actionee_id.is)(0)

      if ((actioner.live) && (actioner.subrole.is == "") &&
          (target.current_role != RoleEnum.ARCHMAGE) &&
          (target.current_role != RoleEnum.WEREWOLF) &&
          (target.current_role != RoleEnum.WOLFCUB) &&
          (target.current_role != RoleEnum.FOX) &&
          (target.current_role != RoleEnum.DEMON) &&
          (target.current_role != RoleEnum.PONTIFF)) {
        actioner.role(target.role.is.substring(0,1))
        actioner.subrole(SubroleEnum.FOXBELIEVER.toString)
        actioner.action_point(0)
        actioner.save
        
        val target_role_str = target.role.is.substring(0,1)
        val target_role_enum =  RoleEnum.get_role(target_role_str)

        val talk_sentence = "＜＜背德變化成功＞＞ (" + target_role_enum.toString + ")"
        val talk = Talk.create.mtype(MTypeEnum.MESSAGE_FOX.toString).message(talk_sentence).font_type("12")
        talks_for_save = talks_for_save ::: List(talk)
      }
    }

    // 哥德法特化
    val godfat_special1_votes = votes.filter(_.mtype.is == MTypeEnum.VOTE_GODFAT_SPECIAL1.toString)
    godfat_special1_votes.foreach { vote =>
      val actioner = user_entrys.filter(_.id.is == vote.actioner_id.is)(0)

      actioner.user_flags(actioner.user_flags.is + UserEntryFlagEnum.GODFAT_SPECIAL1.toString)
      actioner.save

      val talk_sentence = "＜＜哥德法特化成功＞＞ (咒術)"
      val talk = Talk.create.mtype(MTypeEnum.MESSAGE_GENERAL.toString).message(talk_sentence).font_type("12")
      talks_for_save = talks_for_save ::: List(talk)
    }

    val godfat_special2_votes = votes.filter(_.mtype.is == MTypeEnum.VOTE_GODFAT_SPECIAL2.toString)
    godfat_special2_votes.foreach { vote =>
      val actioner = user_entrys.filter(_.id.is == vote.actioner_id.is)(0)

      actioner.user_flags(actioner.user_flags.is + UserEntryFlagEnum.GODFAT_SPECIAL2.toString)
      actioner.save

      val talk_sentence = "＜＜哥德法特化成功＞＞ (方陣)"
      val talk = Talk.create.mtype(MTypeEnum.MESSAGE_GENERAL.toString).message(talk_sentence).font_type("12")
      talks_for_save = talks_for_save ::: List(talk)
    }

    val godfat_special3_votes = votes.filter(_.mtype.is == MTypeEnum.VOTE_GODFAT_SPECIAL3.toString)
    godfat_special3_votes.foreach { vote =>
      val actioner = user_entrys.filter(_.id.is == vote.actioner_id.is)(0)

      actioner.user_flags(actioner.user_flags.is + UserEntryFlagEnum.GODFAT_SPECIAL3.toString)
      actioner.save

      val talk_sentence = "＜＜哥德法特化成功＞＞ (秘術)"
      val talk = Talk.create.mtype(MTypeEnum.MESSAGE_GENERAL.toString).message(talk_sentence).font_type("12")
      talks_for_save = talks_for_save ::: List(talk)
    }

     // 哥德法特化技能
    val godfat_deathgaze_votes = votes.filter(_.mtype.is == MTypeEnum.VOTE_GODFAT_DEATHGAZE.toString)
    godfat_deathgaze_votes.foreach { vote =>
      val actioner = user_entrys.filter(_.id.is == vote.actioner_id.is)(0)
      val actionee = user_entrys.filter(_.id.is == vote.actionee_id.is)(0)

      actioner.user_flags(actioner.user_flags.is + UserEntryFlagEnum.GODFAT_SPECIAL_USED.toString)
      actioner.save

      actionee.user_flags(actionee.user_flags.is + UserEntryFlagEnum.DEATH_2.toString)
      actionee.save
    }

    val godfat_colorspray_votes = votes.filter(_.mtype.is == MTypeEnum.VOTE_GODFAT_COLORSPRAY.toString)
    godfat_colorspray_votes.foreach { vote =>
      val actioner = user_entrys.filter(_.id.is == vote.actioner_id.is)(0)

      actioner.user_flags(actioner.user_flags.is + UserEntryFlagEnum.GODFAT_SPECIAL_USED.toString)
      actioner.save

      val talk_sentence = "＜＜哥德法使用七彩噴射＞＞"
      val talk = Talk.create.mtype(MTypeEnum.MESSAGE_FOX.toString).message(talk_sentence).font_type("12")
      talks_for_save = talks_for_save ::: List(talk)
    }

    val godfat_blind_votes = votes.filter(x=>(x.mtype.is == MTypeEnum.VOTE_GODFAT_BLIND.toString) ||
                                             (x.mtype.is == MTypeEnum.VOTE_GODFAT_BLIND2.toString))
    godfat_blind_votes.foreach { vote =>
      val actioner = user_entrys.filter(_.id.is == vote.actioner_id.is)(0)

      actioner.user_flags(actioner.user_flags.is + UserEntryFlagEnum.GODFAT_BLIND_USED.toString)
      actioner.save

      val talk_sentence = "＜＜哥德法使用眩光＞＞"
      val talk = Talk.create.mtype(MTypeEnum.MESSAGE_FOX.toString).message(talk_sentence).font_type("12")
      talks_for_save = talks_for_save ::: List(talk)
    }

    val godfat_exchange_votes = votes.filter(_.mtype.is == MTypeEnum.VOTE_GODFAT_EXCHANGE.toString)
    godfat_exchange_votes.foreach { vote =>
      val actioner = user_entrys.filter(_.id.is == vote.actioner_id.is)(0)
      val actionee = user_entrys.filter(_.id.is == vote.actionee_id.is)(0)

      actioner.role(RoleEnum.FOX.toString + actioner.role.is.substring(1))
      actioner.save

      actionee.role(RoleEnum.GODFAT.toString + actionee.role.is.substring(1))
      actionee.save
    }

    

    // 學者能力
    val scholar_analyzes = votes.filter(_.mtype.is == MTypeEnum.VOTE_SCHOLAR_ANALYZE.toString)
    scholar_analyzes.foreach { scholar_analyze =>
      val actioner = user_entrys.filter(_.id.is == scholar_analyze.actioner_id.is)(0)
      if (actioner.live.is) {
        actioner.user_flags(actioner.user_flags.is + UserEntryFlagEnum.ANALYZED.toString)
        actioner.save
      }
    }
    val scholar_reports = votes.filter(_.mtype.is == MTypeEnum.VOTE_SCHOLAR_REPORT.toString)
    scholar_reports.foreach { scholar_report =>
      val actioner = user_entrys.filter(_.id.is == scholar_report.actioner_id.is)(0)
      if (actioner.live.is) {
        actioner.user_flags(actioner.user_flags.is + UserEntryFlagEnum.REPORTED.toString)
        actioner.save
      }
    }

    // 惡魔能力
    val demon_curses = votes.filter(x=>(x.mtype.is == MTypeEnum.VOTE_DEMON_CURSE.toString) ||
                                       (x.mtype.is == MTypeEnum.VOTE_DEMON_CURSE2.toString))
    demon_curses.foreach { demon_curse =>
      val actioner = user_entrys.filter(_.id.is == demon_curse.actioner_id.is)(0)
      if (actioner.live.is) {
        if ((demon_curse.actionee_id.is != 0) && (demon_curse.actionee_id.is != demon_curse.actioner_id.is))
          actioner.action_point(actioner.action_point.is + 1)
        else
          actioner.action_point(actioner.action_point.is + 2)
        //actioner.user_flags(actioner.user_flags.is + UserEntryFlagEnum.CURSE_USED.toString)
        actioner.save
      }
    }

    val demon_chaoss = votes.filter(_.mtype.is == MTypeEnum.VOTE_DEMON_CHAOS.toString)
    demon_chaoss.foreach { demon_chaos =>
      val actioner = user_entrys.filter(_.id.is == demon_chaos.actioner_id.is)(0)
      if (actioner.live.is) {
        actioner.action_point(actioner.action_point.is + 2)
        actioner.save
      }
    }

    val demon_dominates = votes.filter(_.mtype.is == MTypeEnum.VOTE_DEMON_DOMINATE.toString)
    demon_dominates.foreach { demon_dominate =>
      val actioner = user_entrys.filter(_.id.is == demon_dominate.actioner_id.is)(0)
      if (actioner.live.is) {
        actioner.action_point(actioner.action_point.is + 1)
        actioner.save
      }
    }

    val demon_vortexes = votes.filter(_.mtype.is == MTypeEnum.VOTE_DEMON_VORTEX.toString)
    demon_vortexes.foreach { demon_vortex =>
      val actioner = user_entrys.filter(_.id.is == demon_vortex.actioner_id.is)(0)
      if (actioner.live.is) {
        actioner.action_point(actioner.action_point.is + 3)
        actioner.user_flags(actioner.user_flags.is + UserEntryFlagEnum.VORTEX_USED.toString)
        actioner.save
      }
    }


    // 背德偽裝能力
    val betrayer_disguises = votes.filter(_.mtype.is == MTypeEnum.VOTE_BETRAYER_DISGUISE.toString)
    betrayer_disguises.foreach { betrayer_disguise =>
      val actioner = user_entrys.filter(_.id.is == betrayer_disguise.actioner_id.is)(0)
      if (actioner.live.is) {
        if (room.room_flags.is.indexOf(RoomFlagEnum.CLERIC_OPTION2.toString) == -1)
          actioner.action_point(actioner.action_point.is - 3)
        else
          actioner.action_point(actioner.action_point.is - 2)
        actioner.user_flags(actioner.user_flags.is.replace(UserEntryFlagEnum.CARD_HERMIT.toString, ""))
        actioner.save
      }
    }

    // 狼人咬的一系列處理
    var bite_successful = false
    val last_day    = RoomDay.findAll(By(RoomDay.room_id, room.id.is), By(RoomDay.day_no, room_day.day_no.is - 1))(0)
    val last_hanged_vote = SystemMessage.findAll(By(SystemMessage.roomday_id, last_day.id.is),
                                                By(SystemMessage.mtype, MTypeEnum.DEATH_HANGED.toString))
    var last_hanged : UserEntry = null
    if (last_hanged_vote.length != 0) {
      val last_hanged_list = user_entrys.filter(_.id.is == last_hanged_vote(0).actioner_id.is)
      if (last_hanged_list.length != 0) 
        last_hanged = last_hanged_list(0)
    }

    val werewolf_votes  = votes.filter(_.mtype.is == MTypeEnum.VOTE_WEREWOLF.toString)
    var werewolf_biter  = user_entrys.filter(_.id.is == werewolf_votes(0).actioner_id.is)(0)
    var werewolf_target = user_entrys.filter(_.id.is == werewolf_votes(0).actionee_id.is)(0)
    val demon_victims = votes.filter(x=>(x.mtype.is == MTypeEnum.VOTE_DEMON_CHAOS.toString) &&
                                        (x.actioner_id.is != werewolf_target.id.is) &&
                                        ((x.actionee_id.is == werewolf_biter.id.is) ||
                                         (x.actionee_id.is == werewolf_target.id.is)))

    if (demon_victims.length != 0) {
      val demon_victimer = user_entrys.filter(_.id.is == demon_victims(0).actioner_id.is)(0)
      val demon_victimee = user_entrys.filter(_.id.is == demon_victims(0).actioner_id.is)(0)

      //if ((werewolf_votes(0).actionee_id.is == demon_victimee.id.is) ||
      //    (werewolf_target.id.is == demon_victimee.id.is)) {
        // 惡魔目標轉換
        werewolf_votes(0).actionee_id(demon_victimer.id.is)
        werewolf_votes(0).vote_flags(werewolf_votes(0).vote_flags.is + VoteFlagEnum.VICTIM.toString)
        werewolf_votes(0).save

        // 轉換成功視同詛咒被用掉
        //demon_victimer.user_flags(demon_victimer.user_flags.is + UserEntryFlagEnum.CURSE_USED.toString)
        demon_victimer.user_flags(demon_victimer.user_flags.is.replace(UserEntryFlagEnum.CARD_TOWER.toString, ""))
        demon_victimer.save

        werewolf_target = user_entrys.filter(_.id.is == demon_victimer.id.is)(0)
      //}

    }

    val archmage_dispell_votes   = votes.filter(_.mtype.is == MTypeEnum.VOTE_ARCHMAGE_DISPELL.toString)
    val hunter_votes             = votes.filter(_.mtype.is == MTypeEnum.VOTE_HUNTER.toString)
    hunter_votes.foreach { vote =>
      val actioner = user_entrys.filter(_.id.is == vote.actioner_id.is)(0)
      if (actioner.current_role == RoleEnum.CARDMASTER) {
        actioner.user_flags(actioner.user_flags.is.replace(UserEntryFlagEnum.CARD_CHARIOT.toString, ""))
        actioner.save
      }
    }

    val herbalist_elixir_votes   = votes.filter(_.mtype.is == MTypeEnum.VOTE_HERBALIST_ELIXIR.toString)
    val cleric_sancture_votes    = votes.filter(_.mtype.is == MTypeEnum.VOTE_CLERIC_SANCTURE.toString)
    val runner_votes             = votes.filter(_.mtype.is == MTypeEnum.VOTE_RUNNER.toString)
    //val runners                  = runner_votes.map{x=>user_entrys.filter(_.id.is == x.actioner_id.is)(0).id.is}
    val runners                  = runner_votes.map{x=> x.actioner_id.is}

    // 是否逃亡者死亡
    runner_votes.foreach { vote =>
      val actioner = user_entrys.filter(_.id.is == vote.actioner_id.is)(0)
      val target   = user_entrys.filter(_.id.is == vote.actionee_id.is)(0)
      
      var runner_death = false
      if (vote.actionee_id.is == werewolf_votes(0).actionee_id.is) 
        runner_death = true
      if (target.current_role == RoleEnum.WOLFCUB)
        runner_death = true
      if ((target.current_role == RoleEnum.WEREWOLF) &&
          ((room.room_flags.is.indexOf(RoomFlagEnum.RUNNER_OPTION1.toString) == -1) || // 處理逃亡者選項一
           (target.id.is != werewolf_votes(0).actioner_id.is)))
        runner_death = true
      hunter_votes.foreach { hunter_vote =>
        if ((room.room_flags.is.indexOf(RoomFlagEnum.RUNNER_OPTION2.toString) != -1) && // 處理逃亡者選項二
            (target.id.is == hunter_vote.actionee_id.is))
          runner_death = true
      }
      
      if (runner_death) {
        process_death(room_day, actioner, MTypeEnum.DEATH_RUNNER)
      }
    }
 
  
    // 判斷狼人咬的結果，順序 1.獵人守 2.治療藥 3.聖域術 4.咬到
    //val werewolf_power  = (werewolf_votes(0).vote_flags.is.indexOf(VoteFlagEnum.POWER.toString) != -1)
    val werewolf_power  = (last_hanged != null) && (last_hanged.current_role == RoleEnum.WOLFCUB)
    if (werewolf_power) {
      werewolf_votes(0).vote_flags(werewolf_votes(0).vote_flags.is + VoteFlagEnum.POWER.toString)
      werewolf_votes(0).save
    }

    //
    //println("werewolf target : " + werewolf_target.handle_name.is)
    
    //if ((werewolf_target.current_role != RoleEnum.RUNNER) || (werewolf_target.test_memoryloss(room, room_day, user_entrys)) ||
    //    (werewolf_target.test_fake(room_day)) || (room_day.day_no.is == 1)){
    if (!runners.contains(werewolf_target.id.is)) {
      if ((hunter_votes.filter(_.actionee_id.is == werewolf_target.id.is).length != 0) && (!werewolf_power)) {} // 1. 獵人守
      else if (archmage_dispell_votes.filter(_.actionee_id.is == werewolf_biter.id.is).length != 0) {}          // 2. 大魔導解除法術
      else if ((werewolf_target.current_role == RoleEnum.WOLFCUB) || (werewolf_target.current_role == RoleEnum.WEREWOLF)) {}                                           // 3. 咬到幼狼
      else if ((werewolf_target.current_role == RoleEnum.FOX) && (!werewolf_power)) {}                          // 4. 咬到狐
      else if ((werewolf_target.current_role == RoleEnum.DEMON) && (!werewolf_power)) {                         // 5. 咬到惡魔
        werewolf_target.action_point(werewolf_target.action_point.is + 3)
        if (werewolf_target.user_flags.is.indexOf(UserEntryFlagEnum.BITED.toString) == -1) {
          werewolf_target.user_flags( werewolf_target.user_flags.is + UserEntryFlagEnum.BITED.toString )
        }
        werewolf_target.save
      }
      else if ((werewolf_target.current_role == RoleEnum.ARCHMAGE) &&
               (werewolf_target.user_flags.is.indexOf(UserEntryFlagEnum.WATER_ELEM_USED.toString) == -1) &&
               (!werewolf_power)) {                         // 6. 咬到水元素
        werewolf_target.user_flags( werewolf_target.user_flags.is + UserEntryFlagEnum.WATER_ELEM_USED.toString )
        werewolf_target.save

        val sys_mes = SystemMessage.create.roomday_id(room_day.id.is)
                    .actioner_id(0).mtype(MTypeEnum.DEATH_EATEN.toString)
        sys_mes.save
      }
      else  {
        val herbalist_elixirs = herbalist_elixir_votes.filter(_.actionee_id.is == werewolf_target.id.is)
        if ((herbalist_elixirs.length != 0) && (!werewolf_power)) { // 7.治療藥，只算用掉一個
          val herbalist_elixir = herbalist_elixirs(0)
          val actioner = user_entrys.filter(_.id.is == herbalist_elixir.actioner_id.is)(0)
          actioner.user_flags(actioner.user_flags.is + UserEntryFlagEnum.ELIXIR_USED.toString)
          actioner.save
        }
        else {
          //val cleric_sanctures = cleric_sancture_votes.filter(_.actionee_id.is == werewolf_target.id.is)
          if ((cleric_sancture_votes.length != 0) && (!werewolf_power)) { // 8.聖域術，牧師同時放兩個都會掛
            cleric_sancture_votes.foreach { cleric_sancture =>
              val actioner = user_entrys.filter(_.id.is == cleric_sancture.actioner_id.is)(0)
              process_death(room_day, actioner, MTypeEnum.DEATH_CLERIC)
            } 
          } else { // 9.咬到了
            bite_successful = true
            process_death(room_day, werewolf_target, MTypeEnum.DEATH_EATEN)

            // 補充狂巫法力
            val live_sorcerors = user_entrys.filter(x=>(x.current_role == RoleEnum.SORCEROR) && (x.live.is))
            live_sorcerors.foreach { live_sorceror =>
              live_sorceror.action_point(Math.min(10, live_sorceror.action_point.is + 2))
              live_sorceror.save
            }

            // 咬到獵人？
            if ((werewolf_target.current_role == RoleEnum.HUNTER) &&
                (room.room_flags.is.indexOf(RoomFlagEnum.HUNTER_OPTION1.toString) != -1)) {
              val werewolf_target_vote = hunter_votes.filter(_.actioner_id.is == werewolf_target.id.is)
              if (werewolf_target_vote.length != 0) {
                 val hunter_target = user_entrys.filter(_.id.is == werewolf_target_vote(0).actionee_id.is)(0)
                 process_death(room_day, hunter_target, MTypeEnum.DEATH_HUNTER_KILL)
              }
            }
            
            // 如果狼人不幸咬到毒            
            if (werewolf_target.current_role == RoleEnum.POISONER) {
              val live_werewolf   = user_entrys.filter(x=>(x.current_role == RoleEnum.WEREWOLF) && (x.live.is))
              val random_werewolf = live_werewolf((new Random()).nextInt(live_werewolf.length))
              process_death(room_day, random_werewolf, MTypeEnum.DEATH_POISON_N)
            }
          }
        
        }
      }
    }

    if ((!bite_successful) && (room.room_flags.is.indexOf(RoomFlagEnum.MADMAN_STUN.toString) != -1)) {
      // 補充狂人怒氣
      val live_madmans = user_entrys.filter(x=>(x.current_role == RoleEnum.MADMAN) && (x.live.is))
      live_madmans.foreach { live_madman =>
        live_madman.action_point(Math.min(10, live_madman.action_point.is + 1))
        live_madman.save
      }
    }

    val wolfcub_votes  = votes.filter(_.mtype.is == MTypeEnum.VOTE_WOLFCUB.toString)
    wolfcub_votes.foreach { wolfcub_vote =>
      val wolfcub_biter  = user_entrys.filter(_.id.is == wolfcub_vote.actioner_id.is)(0)
      val wolfcub_target = user_entrys.filter(_.id.is == wolfcub_vote.actionee_id.is)(0)

      //if ((wolfcub_target.current_role != RoleEnum.RUNNER) || (wolfcub_target.test_memoryloss(room, room_day, user_entrys)) ||
      //    (wolfcub_target.test_fake(room_day)) || (room_day.day_no.is == 1)){
      if (!runners.contains(wolfcub_target.id.is)) {
        if ((hunter_votes.filter(_.actionee_id.is == wolfcub_target.id.is).length != 0)) {} // 1. 獵人守
        else if (archmage_dispell_votes.filter(_.actionee_id.is == wolfcub_biter.id.is).length != 0) {} // 2. 大魔導解除法術
        else if ((wolfcub_target.current_role == RoleEnum.WOLFCUB) || (wolfcub_target.current_role == RoleEnum.WEREWOLF)) {}                                           // 3. 咬到幼狼
        else if ((wolfcub_target.current_role == RoleEnum.FOX) ) {}  // 4. 咬到狐
        else if ((wolfcub_target.current_role == RoleEnum.DEMON)) {} // 5. 咬到惡魔
        else  {
          val herbalist_elixirs = herbalist_elixir_votes.filter(_.actionee_id.is == wolfcub_target.id.is)
          if (herbalist_elixirs.length != 0) { // 7.治療藥，只算用掉一個
            val herbalist_elixir = herbalist_elixirs(0)
            val actioner = user_entrys.filter(_.id.is == herbalist_elixir.actioner_id.is)(0)
            if (actioner.user_flags.is.indexOf(UserEntryFlagEnum.ELIXIR_USED.toString) == -1) {
              actioner.user_flags(actioner.user_flags.is + UserEntryFlagEnum.ELIXIR_USED.toString)
              actioner.save
            }
          }
          else {
            //val cleric_sanctures = cleric_sancture_votes.filter(_.actionee_id.is == werewolf_target.id.is)
            if (cleric_sancture_votes.length != 0) { // 8.聖域術，牧師同時放兩個都會掛
              cleric_sancture_votes.foreach { cleric_sancture =>
                val actioner = user_entrys.filter(_.id.is == cleric_sancture.actioner_id.is)(0)
                process_death(room_day, actioner, MTypeEnum.DEATH_CLERIC)
              }
            } else { // 9.咬到了
              bite_successful = true
              process_death(room_day, wolfcub_target, MTypeEnum.DEATH_WOLFCUB_EATEN)

              // 如果狼人不幸咬到毒
              if (wolfcub_target.current_role == RoleEnum.POISONER) {
                process_death(room_day, wolfcub_biter, MTypeEnum.DEATH_POISON_N)
              }
            }
          }
        }
      }
    }

    
    val augurer_votes = votes.filter(_.mtype.is == MTypeEnum.VOTE_AUGURER.toString)
    // 占卜師占到狐？
    augurer_votes.foreach { augurer_vote =>
      val actioner = user_entrys.filter(_.id.is == augurer_vote.actioner_id.is)(0)
      val target   = user_entrys.filter(_.id.is == augurer_vote.actionee_id.is)(0)
      
      if ((actioner.live.is) && (!actioner.test_fake(room_day))) {
        if (target.current_role == RoleEnum.FOX) {
          process_death(room_day, target, MTypeEnum.DEATH_FOX)
        } else if (target.current_role == RoleEnum.GODFAT) {
          process_death(room_day, actioner, MTypeEnum.DEATH_GODFAT)
        } else if (target.current_role == RoleEnum.DEMON) {
          target.action_point(target.action_point.is + 3)
          target.save
        } else if (target.subrole.is == SubroleEnum.ALPHAWOLF.toString) {
          if (target.user_flags.is.indexOf(UserEntryFlagEnum.AUGURED.toString) == -1) {
            augurer_vote.vote_flags(augurer_vote.vote_flags.is + VoteFlagEnum.FAKE.toString)
            augurer_vote.save

            target.user_flags(target.user_flags.is + UserEntryFlagEnum.AUGURED.toString)
            target.save
          }
        }

        if (actioner.current_role == RoleEnum.CARDMASTER) {
          actioner.user_flags(actioner.user_flags.is.replace(UserEntryFlagEnum.CARD_MAGICIAN.toString, ""))
          actioner.save
        }
      }
    }

    // 狂巫法術
    val sorceror_augures = votes.filter(_.mtype.is == MTypeEnum.VOTE_SORCEROR_AUGURE.toString)
    sorceror_augures.foreach { sorceror_augure =>
      val actioner = user_entrys.filter(_.id.is == sorceror_augure.actioner_id.is)(0)
      if (actioner.live.is) {
        val target   = user_entrys.filter(_.id.is == sorceror_augure.actionee_id.is)(0)
        if (target.current_role == RoleEnum.GODFAT) {
          actioner.live(false)

          val sys_mes = SystemMessage.create.roomday_id(room_day.id.is)
                        .actioner_id(actioner.id.is).mtype(MTypeEnum.DEATH_GODFAT.toString)
          sys_mes.save
        } else if (target.current_role == RoleEnum.DEMON) {
          target.action_point(target.action_point.is + 2)
          target.save
        }
        actioner.action_point(Math.max(0, actioner.action_point.is-2))
        actioner.save
      }
    }
    val sorceror_whispers = votes.filter(_.mtype.is == MTypeEnum.VOTE_SORCEROR_WHISPER.toString)
    sorceror_whispers.foreach { sorceror_whisper =>
      val actioner = user_entrys.filter(_.id.is == sorceror_whisper.actioner_id.is)(0)
      if (actioner.live.is) {
        actioner.action_point(Math.max(0, actioner.action_point.is-3))
        actioner.save
      }
    }
    val sorceror_conjures = votes.filter(_.mtype.is == MTypeEnum.VOTE_SORCEROR_CONJURE.toString)
    sorceror_conjures.foreach { sorceror_conjure =>
      val actioner = user_entrys.filter(_.id.is == sorceror_conjure.actioner_id.is)(0)
      val target   = user_entrys.filter(_.id.is == sorceror_conjure.actionee_id.is)(0)

      if (actioner.live.is) {
        if (target.current_role == RoleEnum.GODFAT) {
          actioner.live(false)

          val sys_mes = SystemMessage.create.roomday_id(room_day.id.is)
                        .actioner_id(actioner.id.is).mtype(MTypeEnum.DEATH_GODFAT.toString)
          sys_mes.save
        } else if (target.current_role == RoleEnum.DEMON) {
          target.action_point(target.action_point.is + 5)
          target.save
        }

        actioner.action_point(Math.max(0, actioner.action_point.is-4))
        actioner.save

        if (target.current_role == RoleEnum.FOX) {
          target.live(false)
          target.save

          val sys_mes = SystemMessage.create.roomday_id(room_day.id.is)
                        .actioner_id(target.id.is).mtype(MTypeEnum.DEATH_SORCEROR.toString)
          sys_mes.save
        }
      }
    }
    val sorceror_shouts = votes.filter(_.mtype.is == MTypeEnum.VOTE_SORCEROR_SHOUT.toString)
    sorceror_shouts.foreach { sorceror_shout =>
      val actioner = user_entrys.filter(_.id.is == sorceror_shout.actioner_id.is)(0)
      if (actioner.live.is) {
        actioner.action_point(Math.max(0, actioner.action_point.is-5))
        actioner.save
      }
    }
    val sorceror_believes = votes.filter(_.mtype.is == MTypeEnum.VOTE_SORCEROR_BELIEVE.toString)
    sorceror_believes.foreach { sorceror_believe =>
      val actioner = user_entrys.filter(_.id.is == sorceror_believe.actioner_id.is)(0)
      val target   = user_entrys.filter(_.id.is == sorceror_believe.actionee_id.is)(0)

      if (actioner.live.is)
      {
        actioner.action_point(Math.max(0, actioner.action_point.is-5))
        actioner.save
        if ((RoleEnum.get_role(target.role.is.substring(0,1)).role_side == RoomVictoryEnum.VILLAGER_WIN) &&
          (target.subrole.is == "")) {
          target.subrole(SubroleEnum.WOLFBELIEVER.toString)
          target.save
        }
      }
      
    }

    // 狂人 STUN 回復
    val stunned1s  =  user_entrys.filter(_.user_flags.is.indexOf(UserEntryFlagEnum.STUNNED_1.toString) != -1)
    stunned1s.foreach { stunned1 =>
       stunned1.user_flags(stunned1.user_flags.is.replace(UserEntryFlagEnum.STUNNED_1.toString,""))
       stunned1.save
    }
    val stunned2s  =  user_entrys.filter(_.user_flags.is.indexOf(UserEntryFlagEnum.STUNNED_2.toString) != -1)
    stunned2s.foreach { stunned2 =>
       stunned2.user_flags(stunned2.user_flags.is.replace(UserEntryFlagEnum.STUNNED_2.toString,UserEntryFlagEnum.STUNNED_1.toString))
       stunned2.save
    }
    val stunned3s  =  user_entrys.filter(_.user_flags.is.indexOf(UserEntryFlagEnum.STUNNED_3.toString) != -1)
    stunned3s.foreach { stunned3 =>
       stunned3.user_flags(stunned3.user_flags.is.replace(UserEntryFlagEnum.STUNNED_3.toString,UserEntryFlagEnum.STUNNED_2.toString))
       stunned3.save
    }

    // 教主 STUN 回復
    val pontiffs  =  user_entrys.filter(_.current_role == RoleEnum.PONTIFF)
    pontiffs.foreach { pontiff =>
      if (pontiff.user_flags.is.indexOf( UserEntryFlagEnum.PONTIFF_STUNNED.toString ) != -1 ) {
         pontiff.user_flags(pontiff.user_flags.is.replace(UserEntryFlagEnum.PONTIFF_STUNNED.toString,""))
         pontiff.save
      }
    }

    val pontiff_votes = votes.filter(_.mtype.is == MTypeEnum.VOTE_PONTIFF.toString)
    // 教主拉人入教
    pontiff_votes.foreach { pontiff_vote =>
      val actioner = user_entrys.filter(_.id.is == pontiff_vote.actioner_id.is)(0)
      val target   = user_entrys.filter(_.id.is == pontiff_vote.actionee_id.is)(0)

      if ((archmage_dispell_votes.filter(_.actionee_id.is == actioner.id.is).length == 0) &&
          (actioner.live.is)) {
        if (target.subrole.is == SubroleEnum.NORELIGION.toString) {
          actioner.user_flags( actioner.user_flags.is + UserEntryFlagEnum.PONTIFF_STUNNED.toString )
          actioner.save
        } else if ((target.user_flags.is.indexOf(UserEntryFlagEnum.RELIGION.toString) == -1) &&
                   (target.user_flags.is.indexOf(UserEntryFlagEnum.NORELIGION.toString) == -1)) {
          target.user_flags( target.user_flags.is + UserEntryFlagEnum.RELIGION.toString )
          target.save
        }
      }
    }

    val pontiff_command_votes = votes.filter(_.mtype.is == MTypeEnum.VOTE_PONTIFF_COMMAND.toString)
    // 教主指定投票
    pontiff_command_votes.foreach { pontiff_command_vote =>
      val actioner = user_entrys.filter(_.id.is == pontiff_command_vote.actioner_id.is)(0)

      if ((archmage_dispell_votes.filter(_.actionee_id.is == actioner.id.is).length == 0) &&
          (actioner.live.is)) {
        actioner.user_flags( actioner.user_flags.is + UserEntryFlagEnum.PONTIFF_COMMAND_USED.toString )
        actioner.save
      }
    }

    val pontiff_aura_votes = votes.filter(_.mtype.is == MTypeEnum.VOTE_PONTIFF_AURA.toString)
    // 教主光環
    pontiff_aura_votes.foreach { pontiff_aura_vote =>
      val actioner = user_entrys.filter(_.id.is == pontiff_aura_vote.actioner_id.is)(0)
      if (actioner.live.is) {
        actioner.user_flags( actioner.user_flags.is + UserEntryFlagEnum.PONTIFF_AURA.toString )
        actioner.save
      }
    }

    // 大魔導 解除法術
    archmage_dispell_votes.foreach { archmage_dispell_vote =>
      val actioner = user_entrys.filter(_.id.is == archmage_dispell_vote.actioner_id.is)(0)
      val target   = user_entrys.filter(_.id.is == archmage_dispell_vote.actionee_id.is)(0)

      if (actioner.live.is) {
        if (target.current_role == RoleEnum.FOX) {
          process_death(room_day, target, MTypeEnum.DEATH_FOX)
        } else {
          if (target.current_role != RoleEnum.DEMON)
            target.action_point(0)

          val lover_str =
            if (target.user_flags.is.indexOf(UserEntryFlagEnum.LOVER.toString) != -1)
              UserEntryFlagEnum.LOVER.toString
            else
              ""

          val autovoted_str =
            if (target.user_flags.is.indexOf(UserEntryFlagEnum.AUTOVOTED.toString) != -1)
              UserEntryFlagEnum.AUTOVOTED.toString
            else
              ""
          val religion_str =
            if (target.subrole.is == SubroleEnum.SUBPONTIFF.toString)
              UserEntryFlagEnum.RELIGION.toString
            else if (target.subrole.is == SubroleEnum.NORELIGION.toString)
              UserEntryFlagEnum.NORELIGION.toString
            else
              ""
          target.user_flags(lover_str + autovoted_str + religion_str)
          target.save()
        }

        actioner.action_point(Math.max(actioner.action_point.is -3, 0))
        actioner.save()
      }
    }
    // 大魔導 召喚水元素
    val archmage_summon_votes   = votes.filter(_.mtype.is == MTypeEnum.VOTE_ARCHMAGE_SUMMON.toString)
    archmage_summon_votes.foreach { archmage_summon_vote =>
      val actioner = user_entrys.filter(_.id.is == archmage_summon_vote.actioner_id.is)(0)
      if ((actioner.live.is) && (actioner.user_flags.is.indexOf(UserEntryFlagEnum.WATER_ELEM_USED.toString) != -1)) {
         actioner.user_flags(actioner.user_flags.is.replace(UserEntryFlagEnum.WATER_ELEM_USED.toString, ""))
         actioner.action_point(Math.max(actioner.action_point.is -3, 0))
         actioner.save()
      }
    }

    // 學者調查能力
    val scholar_examines = votes.filter(_.mtype.is == MTypeEnum.VOTE_SCHOLAR_EXAMINE.toString)
    scholar_examines.foreach { scholar_examine =>
      val actioner = user_entrys.filter(_.id.is == scholar_examine.actioner_id.is)(0)
      val target   = user_entrys.filter(_.id.is == scholar_examine.actionee_id.is)(0)

      if ((actioner.live.is) && (target.user_flags.is.indexOf(UserEntryFlagEnum.RELIGION.toString) == -1) &&
          (target.current_role != RoleEnum.PONTIFF)) {
        if (target.user_flags.is.indexOf(UserEntryFlagEnum.NORELIGION.toString) == -1) {
          target.user_flags( target.user_flags.is + UserEntryFlagEnum.NORELIGION.toString )
          target.save
        }
      }

      actioner.user_flags(actioner.user_flags.is.replace(UserEntryFlagEnum.CARD_JUSTICE.toString, ""))
      actioner.save
    }

    // 若狐全掛，背德連帶死亡
    // 若狼全掛，幼狼連帶死亡
    process_followers(room, room_day, user_entrys)

    val madman_suicide_votes     = votes.filter(_.mtype.is == MTypeEnum.VOTE_MADMAN_SUICIDE.toString)
    // 狂人自爆
    madman_suicide_votes.foreach { madman_suicide_vote =>
      val actioner = user_entrys.filter(_.id.is == madman_suicide_vote.actioner_id.is)(0)

      if (actioner.live.is) {
        process_death(room_day, actioner, MTypeEnum.DEATH_MADMAN)
      }
    }

    val herbalist_poison_votes   = votes.filter(_.mtype.is == MTypeEnum.VOTE_HERBALIST_POISON.toString)
    // 藥師使用毒藥
    herbalist_poison_votes.foreach { herbalist_poison_vote =>
      val actioner = user_entrys.filter(_.id.is == herbalist_poison_vote.actioner_id.is)(0)
      val target   = user_entrys.filter(_.id.is == herbalist_poison_vote.actionee_id.is)(0)
      
      if ((actioner.live.is) && (target.live.is)) {
        actioner.user_flags(actioner.user_flags.is + UserEntryFlagEnum.POISON_USED.toString)
        actioner.save

        process_death(room_day, target, MTypeEnum.DEATH_POISON_H)
      }
    }

    val herbalist_mix_votes   = votes.filter(_.mtype.is == MTypeEnum.VOTE_HERBALIST_MIX.toString)
    // 藥師使用製藥
    herbalist_mix_votes.foreach { herbalist_mix_vote =>
      val actioner = user_entrys.filter(_.id.is == herbalist_mix_vote.actioner_id.is)(0)

      if (actioner.live.is)  {
        val mix_result_int = new Random().nextInt(5)
        mix_result_int match
        {
          case 0 => actioner.user_flags(actioner.user_flags.is.replace(UserEntryFlagEnum.ELIXIR_USED.toString, "")).save
          case 1 => actioner.user_flags(actioner.user_flags.is.replace(UserEntryFlagEnum.POISON_USED.toString, "")).save
          case 2 => votes_for_save = votes_for_save ::: List(Vote.create.roomday_id(room_day.id.is).mtype(MTypeEnum.VOTE_BETRAYER_DISGUISE.toString).actioner_id(0).actionee_id(actioner.id.is))
          case 3 => votes_for_save = votes_for_save ::: List(Vote.create.roomday_id(room_day.id.is).mtype(MTypeEnum.VOTE_BETRAYER_FOG.toString).actioner_id(0))
          case 4 => actioner.user_flags(actioner.user_flags.is + UserEntryFlagEnum.DEATH_2.toString).save
        }
      }
    }

    val madman_stun1_votes     = votes.filter(_.mtype.is == MTypeEnum.VOTE_MADMAN_STUN1.toString)
    // 狂人擊昏1
    madman_stun1_votes.foreach { madman_stun_vote1 =>
      val actioner = user_entrys.filter(_.id.is == madman_stun_vote1.actioner_id.is)(0)
      val target   = user_entrys.filter(_.id.is == madman_stun_vote1.actionee_id.is)(0)

      if ((actioner.live.is) && (target.live.is)) {
        actioner.action_point(Math.max(actioner.action_point.is -1, 0))
        actioner.save

        target.user_flags(target.user_flags.is + UserEntryFlagEnum.STUNNED_1.toString)
        target.save
      }
    }


    val madman_stun3_votes     = votes.filter(_.mtype.is == MTypeEnum.VOTE_MADMAN_STUN3.toString)
    // 狂人擊昏3
    madman_stun3_votes.foreach { madman_stun_vote3 =>
      val actioner = user_entrys.filter(_.id.is == madman_stun_vote3.actioner_id.is)(0)
      val target   = user_entrys.filter(_.id.is == madman_stun_vote3.actionee_id.is)(0)

      if ((actioner.live.is) && (target.live.is)) {
        actioner.action_point(Math.max(actioner.action_point.is -2, 0))
        actioner.user_flags(actioner.user_flags.is.replace(UserEntryFlagEnum.CARD_STRENGTH.toString, ""))
        actioner.save

        target.user_flags(target.user_flags.is + UserEntryFlagEnum.STUNNED_3.toString)
        target.save
      }
    }

    val madman_stun_votes     = votes.filter(_.mtype.is == MTypeEnum.VOTE_MADMAN_STUN.toString)
    // 狂人擊忘
    madman_stun_votes.foreach { madman_stun_vote =>
      val actioner = user_entrys.filter(_.id.is == madman_stun_vote.actioner_id.is)(0)
      val target   = user_entrys.filter(_.id.is == madman_stun_vote.actionee_id.is)(0)

      if ((actioner.live.is) && (target.live.is) &&
          (target.subrole.is == "")) {
        actioner.action_point(Math.max(actioner.action_point.is -2, 0))
        actioner.save

        target.subrole(SubroleEnum.MEMORYLOSS8.toString)
        target.save
      }
    }

    // 模仿師醒來時變成卡片師
    if (room.room_flags.is.indexOf(RoomFlagEnum.ROLE_CARDMASTER.toString) != -1) {
      user_entrys.filter(x=>((x.current_role == RoleEnum.SHIFTER) && (x.live.is))).foreach{shifter =>
      if (((shifter.subrole.is.indexOf(SubroleEnum.MEMORYLOSS4.toString) != -1 ) &&
           (room_day.day_no.is == 7)) ||
          ((shifter.subrole.is.indexOf(SubroleEnum.MEMORYLOSS6.toString) != -1 ) &&
           (room_day.day_no.is == 11)) ||
          ((shifter.subrole.is.indexOf(SubroleEnum.MEMORYLOSS8.toString) != -1 ) &&
           (room_day.day_no.is == 15)) ||
          ((shifter.subrole.is.indexOf(SubroleEnum.FAKEAUGURER.toString) != -1 ) &&
           (room_day.day_no.is == 7)))
        shifter.role(RoleEnum.CARDMASTER.toString)
      }

      user_entrys.filter(x=>((x.current_role == RoleEnum.CARDMASTER) && (x.live.is))).foreach{cardmaster =>
        var card_pool : java.util.LinkedList[String] = new java.util.LinkedList()
        if (cardmaster.user_flags.is.indexOf(UserEntryFlagEnum.CARD_FOOL.toString) == -1) card_pool.add(UserEntryFlagEnum.CARD_FOOL.toString)
        if (cardmaster.user_flags.is.indexOf(UserEntryFlagEnum.CARD_MAGICIAN.toString) == -1) card_pool.add(UserEntryFlagEnum.CARD_MAGICIAN.toString)
        if (cardmaster.user_flags.is.indexOf(UserEntryFlagEnum.CARD_CHARIOT.toString) == -1) card_pool.add(UserEntryFlagEnum.CARD_CHARIOT.toString)
        if (cardmaster.user_flags.is.indexOf(UserEntryFlagEnum.CARD_HERMIT.toString) == -1) card_pool.add(UserEntryFlagEnum.CARD_HERMIT.toString)
        if (cardmaster.user_flags.is.indexOf(UserEntryFlagEnum.CARD_STRENGTH.toString) == -1) card_pool.add(UserEntryFlagEnum.CARD_STRENGTH.toString)
        if (cardmaster.user_flags.is.indexOf(UserEntryFlagEnum.CARD_JUSTICE.toString) == -1) card_pool.add(UserEntryFlagEnum.CARD_JUSTICE.toString)
        if (cardmaster.user_flags.is.indexOf(UserEntryFlagEnum.CARD_TOWER.toString) == -1) card_pool.add(UserEntryFlagEnum.CARD_TOWER.toString)
        if (cardmaster.user_flags.is.indexOf(UserEntryFlagEnum.CARD_SUN.toString) == -1) card_pool.add(UserEntryFlagEnum.CARD_SUN.toString)
        if (card_pool.size() != 0) {
          java.util.Collections.shuffle(card_pool)
          cardmaster.user_flags(cardmaster.user_flags.is + card_pool.removeFirst())
          cardmaster.save
        }
      }
    }

    // 背德粉紅迷霧
    val betrayer_fogs = votes.filter(_.mtype.is == MTypeEnum.VOTE_BETRAYER_FOG.toString)
    betrayer_fogs.foreach { betrayer_fog =>
      val actioner = user_entrys.filter(_.id.is == betrayer_fog.actioner_id.is)(0)
      if (actioner.live.is) {
        actioner.action_point(Math.max(actioner.action_point.is - 4, 0))
        actioner.save
      }
    }

    // 大魔導回復法力
    user_entrys.filter(x=>((x.current_role == RoleEnum.ARCHMAGE) && (x.live.is))).foreach{archmage =>
      archmage.action_point(Math.min(archmage.action_point.is + 1, 10))
      archmage.save
    }

    // 若狐全掛，背德連帶死亡
    // 若狼全掛，幼狼連帶死亡
    process_followers(room, room_day, user_entrys)

    // 如果場上只剩一位村民，則轉職成大魔導
    if ((room.room_flags.is.indexOf(RoomFlagEnum.ROLE_ARCHMAGE.toString) != -1) &&
        (user_entrys.length >= 25) &&
        (room_day.day_no.is == 15)) {
       val live_villagers = user_entrys.filter( x => (x.live.is) && (x.current_role == RoleEnum.VILLAGER))

       if (live_villagers.length == 1) {
         val live_villager = live_villagers(0)
         live_villager.role(RoleEnum.ARCHMAGE.toString + live_villager.role.is.substring(1))
         live_villager.action_point(2)
         live_villager.save
       }
    }

    val card_sun_votes  = votes.filter(_.mtype.is == MTypeEnum.VOTE_CARD_SUN.toString)
    card_sun_votes.foreach { vote =>
      val actioner = user_entrys.filter(_.id.is == vote.actioner_id.is)(0)
      actioner.user_flags(actioner.user_flags.is.replace(UserEntryFlagEnum.CARD_SUN.toString, ""))
      actioner.save
      room_day.weather(WeatherEnum.SUNNY.toString)
    }

    //println("users_for_save length : " + users_for_save.length)
    //users_for_save.removeDuplicates.foreach( _.save )


    // 新的一日
    val new_room_day  = RoomDay.create.room_id(room.id.is).day_no(room_day.day_no.is + 1)
                        .vote_time(1).weather(room_day.weather.is)
    new_room_day.save                    
    
    (votes ::: votes_for_save).foreach { vote =>
      val sys_mes = SystemMessage.create.roomday_id(new_room_day.id.is)
                    .actioner_id(vote.actioner_id.is).actionee_id(vote.actionee_id.is)
                    .mtype(vote.mtype.is).message(vote.vote_flags.is)
      sys_mes.save
    }
    
    // 加入模仿者訊息
    talks_for_save.foreach { talk_for_save =>
      talk_for_save.roomday_id(new_room_day.id.is)
      talk_for_save.save
    }

    // 進入下一天
    val weather_string =
      if (room.room_flags.is.indexOf(RoomFlagEnum.WEATHER.toString) != -1)
        " (" + WeatherEnum.get_weather(room_day.weather.is) + ")"
      else
        ""
    val talk = Talk.create.roomday_id(new_room_day.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                   .message("< < 早晨來臨 第 " + ((new_room_day.day_no.is+2)/2).toString +" 日的早上開始 > >" + weather_string)
    talk.save

    if ((new_room_day.day_no.is == 6) && (room.room_flags.is.indexOf(RoomFlagEnum.DUMMY_REVEAL.toString) != -1)) {
      val dummy_boy = user_entrys.filter(_.uname.is == "dummy_boy")(0)
      val talk = Talk.create.roomday_id(new_room_day.id.is).mtype(MTypeEnum.MESSAGE_EVIL.toString)
                   .message("< < 非人側的你察覺 " + dummy_boy.handle_name.is + " 的職業是 " +
                            RoleEnum.get_role(dummy_boy.current_role).toString.length.toString + " 個字的 > >")
      talk.save
    }

    if ((new_room_day.day_no.is == 12) && (room.room_flags.is.indexOf(RoomFlagEnum.DUMMY_REVEAL.toString) != -1)) {
      val dummy_boy = user_entrys.filter(_.uname.is == "dummy_boy")(0)
      val talk = Talk.create.roomday_id(new_room_day.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                   .message("< < 你發現 " + dummy_boy.handle_name.is + " 的職業是 " +
                            RoleEnum.get_role(dummy_boy.current_role).toString + " > >")
      talk.save
    }
    //room.addToRoom_days(new_day)
    //room.save(flush:true)
  }
  
  // 判斷勝利條件
  def check_victory(user_entrys: List[UserEntry]) : RoomVictoryEnum.Value = {
    var result : RoomVictoryEnum.Value = RoomVictoryEnum.NONE
    
    val live_user_entrys = user_entrys.filter(_.live.is)
    
    val live_human = live_user_entrys.filter( x =>
        (x.current_role != RoleEnum.WEREWOLF) &&
        (x.current_role != RoleEnum.WOLFCUB) &&
        (x.current_role != RoleEnum.FOX) &&
        (x.current_role != RoleEnum.DEMON))
    val live_wolf     = live_user_entrys.filter(x=>(x.current_role == RoleEnum.WEREWOLF) || (x.current_role == RoleEnum.WOLFCUB))
    val live_fox      = live_user_entrys.filter(_.current_role == RoleEnum.FOX)
    val live_pontiff  = live_user_entrys.filter(_.current_role == RoleEnum.PONTIFF)
    val live_religion = live_user_entrys.filter( x => (x.user_flags.is.indexOf(UserEntryFlagEnum.RELIGION.toString) != -1) ||
                                                      (x.current_role == RoleEnum.PONTIFF))

    if ((live_pontiff.length != 0) && (live_religion.length == live_user_entrys.length))
      return RoomVictoryEnum.PONTIFF_WIN

    if (live_wolf.length == 0) {
      if (live_fox.length != 0) 
        result = RoomVictoryEnum.FOX_WIN
      else if (live_human.length != 0)
        result = RoomVictoryEnum.VILLAGER_WIN
      else
        result = RoomVictoryEnum.ABANDONED
    } else if (live_wolf.length >= live_human.length) {
      if (live_fox.length != 0) 
        result = RoomVictoryEnum.FOX_WIN2
      else
        result = RoomVictoryEnum.WEREWOLF_WIN
    } else
      return RoomVictoryEnum.NONE
    
    return result
  }

  def process_phase(room:Room, room_day:RoomDay, user_entrys:List[UserEntry], vote_list:List[Vote]) = {
    if (room_day.day_no.is % 2 == 0) {
      // 白天的話，要判斷是否要重新投票
      val voted_player = VoteHelper.check_vote_hang(room, room_day, user_entrys, vote_list)

      if (voted_player == null) {
        // 平手重投
        val talk = Talk.create.roomday_id(room_day.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                              .message("請重新投票(第 "+ room_day.vote_time.is.toString +" 回)")
        talk.save()

        var time_now = new java.util.Date()
        if (room_day.deadline.is != null) {
          val e_datetime = new java.util.GregorianCalendar()
          e_datetime.setTime(room_day.deadline.is)
          e_datetime.add(java.util.Calendar.MINUTE, 2)
          val e_time = e_datetime.getTime()
          if (time_now.after(e_time))
            time_now = e_time
        }

        room_day.deadline(time_now)
        room_day.vote_time(room_day.vote_time.is + 1)
        room_day.save()

        // 處理自投者
        val votes_auto = vote_list.filter(_.vote_flags.is.indexOf(VoteFlagEnum.AUTO.toString) != -1)
        votes_auto.foreach { vote_auto =>
          val auto_player = user_entrys.filter(_.id.is == vote_auto.actioner_id.is)(0)
          if (auto_player.user_flags.is.indexOf(UserEntryFlagEnum.AUTOVOTED.toString) == -1)
            auto_player.user_flags(auto_player.user_flags.is + UserEntryFlagEnum.AUTOVOTED.toString)
          else if (auto_player.live.is) {
            auto_player.live(false)

            val talk = Talk.create.roomday_id(room_day.id.is)
                         .actioner_id(auto_player.id.is).mtype(MTypeEnum.MESSAGE_DEATHSUDDEN.toString)
            talk.save

            val sys_mes = SystemMessage.create.roomday_id(room_day.id.is)
                         .actioner_id(auto_player.id.is).mtype(MTypeEnum.DEATH_SUDDEN.toString)
            sys_mes.save

            // 若狐全掛，背德連帶死亡
            // 若狼全掛，幼狼連帶死亡
            process_followers(room, room_day, user_entrys)

          }
          auto_player.save
        }

        var victory_check = GameProcesser.check_victory(user_entrys)
        if (victory_check != RoomVictoryEnum.NONE) {
          room.status(RoomStatusEnum.ENDED.toString)
          room.victory(victory_check.toString)
          room.save
        } else if (room_day.vote_time.is > 10) {
          // 直接結束，和局
          val new_day  = RoomDay.create.room_id(room.id.is).day_no(room_day.day_no.is + 1)
                                .vote_time(1)
          new_day.save()
          room.status(RoomStatusEnum.ENDED.toString)
          room.victory(RoomVictoryEnum.DRAW.toString)
          room.save()
        } else {
          // Update 使用者狀態
          DB.use(DefaultConnectionIdentifier) { conn =>
            DB.prepareStatement("update UserEntry set last_day_no = '0' where room_id = ?", conn) { stmt =>
              stmt.setLong(1, room.id.is)
              stmt.executeUpdate()
            }
          }
        }
      } else {
        // 有決定誰被吊了
        var victory_check = GameProcesser.process_day(room, room_day, user_entrys, vote_list, voted_player)
        if (victory_check == RoomVictoryEnum.NONE)
          victory_check = GameProcesser.check_victory(user_entrys)
        if (victory_check != RoomVictoryEnum.NONE) {
          room.status(RoomStatusEnum.ENDED.toString)
          room.victory(victory_check.toString)
          room.save
        }
      }
    } else {
      // 晚上的話就直接進行了
      GameProcesser.process_night(room, room_day, user_entrys, vote_list)
      var victory_check = GameProcesser.check_victory(user_entrys)
      if (victory_check != RoomVictoryEnum.NONE) {
        room.status(RoomStatusEnum.ENDED.toString)
        room.victory(victory_check.toString)
        room.save
      }
    }
  }

  // 傳回值表示是否切入下一天
  def check_deadline(room:Room, room_day:RoomDay, user_entrys:List[UserEntry]) : Boolean= {
    var result = false
    val e_datetime = new java.util.GregorianCalendar()

    // 第 0 日判斷是否廢村
    if (room_day.day_no.is == 0) {
      e_datetime.setTime(room.updated.is)
      e_datetime.add(java.util.Calendar.MINUTE, 10)
      val time_now = new java.util.Date()

      if (time_now.after(e_datetime.getTime())) {
         // 新的一日
         val new_day  = RoomDay.create.room_id(room.id.is).day_no(room_day.day_no.is + 1).vote_time(1)
         new_day.save()

         // 進入下一天
         room.status(RoomStatusEnum.ENDED.toString)
         room.victory(RoomVictoryEnum.ABANDONED.toString)
         room.save()

         return true
      }
    }

    if ((room_day.day_no.is != 0) && (room.status.is != RoomStatusEnum.ENDED.toString)) {
      e_datetime.setTime(room_day.created.is)

      if (room_day.day_no.is % 2 == 0) {
        e_datetime.add(java.util.Calendar.MINUTE, room.day_minutes.is)
      } else {
        e_datetime.add(java.util.Calendar.MINUTE, room.night_minutes.is)
      }

      val time_now = new java.util.Date()
      if ((room_day.deadline.is == null) && (time_now.after(e_datetime.getTime()))) {

        GameProcessLock.get_lock(room.id.is).synchronized {
        // 寫入最後兩分鐘還不投票將會暴斃
          val room_day2 = RoomDay.findAll(By(RoomDay.room_id, room.id.is), OrderBy(RoomDay.day_no, Descending))(0)

          if ((room_day2.deadline.is == null) && (room_day2.day_no.is == room_day.day_no.is) &&
              (room_day2.vote_time.is == room_day.vote_time.is)) {
              val talk = Talk.create.roomday_id(room_day.id.is).mtype(MTypeEnum.MESSAGE_LAST2MIN.toString)
            talk.save()

            room_day.deadline(e_datetime.getTime())
            room_day.save()

          }
        }
      }

      if (room_day.deadline.is != null) {
        val old_deadline  = room_day.deadline.is
        val real_deadline = new java.util.GregorianCalendar()
        real_deadline.setTime(room_day.deadline.is)
        real_deadline.add(java.util.Calendar.MINUTE, 2)

        val deadline = real_deadline.getTime()
        if (time_now.after(deadline)) {
          // 有人暴斃了，投票重投 @@
          GameProcessLock.get_lock(room.id.is).synchronized {
            val room_day2 = RoomDay.findAll(By(RoomDay.room_id, room.id.is), OrderBy(RoomDay.day_no, Descending))(0)

            if ((room_day2.deadline != null) && (room_day2.day_no.is == room_day.day_no.is) &&
                (room_day2.vote_time.is == room_day.vote_time.is) &&
                (room_day2.deadline.is == room_day.deadline.is)) {

              if (room_day2.deadline.is != old_deadline)
                S.error("Roomday : " + room_day.id.is.toString + " " + room_day2.deadline.is.toString + " " +
                         old_deadline.toString)

              var vote_list = Vote.findAll(By(Vote.roomday_id, room_day.id.is), By(Vote.vote_time, room_day.vote_time.is))
              if ((room_day.day_no.is %2 == 0) && (room.room_flags.is.indexOf(RoomFlagEnum.AUTO_VOTE.toString) != -1)) {
                // 白天的話如果有開自投的話
                user_entrys.foreach { user =>
                  if (user.get_action_list(room, room_day, user_entrys, vote_list).length != 0) {
                     val vote = Vote.create.roomday_id(room_day.id.is).actioner_id(user.id.is).vote_time(room_day.vote_time.is)
                                .actionee_id(user.id.is).mtype(MTypeEnum.VOTE_HANG.toString)
                     vote.save()
                  }
                }
                vote_list = Vote.findAll(By(Vote.roomday_id, room_day.id.is), By(Vote.vote_time, room_day.vote_time.is))
                GameProcesser.process_phase(room, room_day, user_entrys, vote_list)
                result = true
              } else {
                // 晚上或是沒開自投
                user_entrys.filter{_.get_action_list(room, room_day, user_entrys, vote_list).length != 0}.foreach { user =>
                  if (user.live.is) {
                    // 有人要暴斃了
                    GameProcesser.process_death(room_day, user, MTypeEnum.DEATH_SUDDEN)
                    //user.live(false)
                    //user.save()

                    //val sys_mes = SystemMessage.create.roomday_id(room_day.id.is).actioner_id(user.id.is)
                    //               .mtype(MTypeEnum.DEATH_SUDDEN.toString)
                    //sys_mes.save

                    val talk = Talk.create.roomday_id(room_day.id.is).actioner_id(user.id.is)
                                    .mtype(MTypeEnum.MESSAGE_DEATHSUDDEN.toString)
                    talk.save

                    // 若狐全掛，背德連帶死亡
                    // 若狼全掛，幼狼連帶死亡
                    process_followers(room, room_day, user_entrys)
                  }
                }

                // 判斷是否遊戲結束
                val victory = check_victory(user_entrys)
                if (victory != RoomVictoryEnum.NONE) {
                  // 新的一日
                  val new_day  = RoomDay.create.room_id(room.id.is).day_no(room_day.day_no.is + 1).vote_time(1)
                  new_day.save()

                  // 進入下一天
                  room.status(RoomStatusEnum.ENDED.toString)
                  room.victory(victory.toString)
                  room.save()

                  result = true
                } else {
                  // 投票重新開始
                  val talk2 = Talk.create.roomday_id(room_day.id.is)
                                   .mtype(MTypeEnum.MESSAGE_REVOTE.toString)
                  talk2.save

                  Vote.bulkDelete_!!(By(Vote.roomday_id, room_day.id.is), By(Vote.vote_time, room_day.vote_time.is))
                  room_day.deadline(deadline)
                  room_day.save

                  // 最後二分還不投會暴斃
                  val talk3 = Talk.create.roomday_id(room_day.id.is)
                                   .mtype(MTypeEnum.MESSAGE_LAST2MIN.toString)
                  talk3.save

                  // Update 使用者狀態
                  DB.use(DefaultConnectionIdentifier) { conn =>
                    DB.prepareStatement("update UserEntry set last_day_no = '0' where room_id = ?", conn) { stmt =>
                      stmt.setLong(1, room.id.is)
                      stmt.executeUpdate()
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    return result
  }
}