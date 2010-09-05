package org.plummtw.jinrou.util

import java.util.Date
import collection.mutable.{HashMap, SynchronizedMap}

class TimedCache[U, V](val timeout : Int) {
  class TimedCacheItem[V](var data : V, var access_time : Date) {
    val lock = new Object()
  }
  val map = new HashMap[U, TimedCacheItem[V]] with SynchronizedMap[U, TimedCacheItem[V]]

  def get(key : U) : Option[V] = {
    val item = map.get(key)
    item match {
      case Some(x) => x.lock.synchronized {
                         x.access_time = new Date()
                         Some(x.data) }
      case None    => None
    }
  }

  def put(key : U, data : V) : Unit = {
    val item = map.get(key)
    item match {
      case Some(x) => x.lock.synchronized {
                         x.access_time = new Date()
                         x.data = data
                       }
      case None    => map += key -> new TimedCacheItem(data, new Date)
    }
  }

  var removeNotifier : () => Unit = null

  def getOr(key : U)(func : () => V) : V = {
    get(key) match {
      case Some(x) => x
      case None    => val data = func()
                      put(key, data)
                      data
    }
  }



  var is_thread_stop = false

  val thread = new Thread {
    override def run() {
      var i = 0
      while (!is_thread_stop) {
        Thread.sleep(1000)
        i += 1000
        if (i >= timeout) {
          i = 0
          // 進行清除動作
          val time_now = (new Date()).getTime()
          map.foreach { t =>
            val data = t._2
            if (time_now - data.access_time.getTime() > timeout) {
              if (removeNotifier != null)
                removeNotifier()
              //map.remove(t._1)
              map.removeKey(t._1)
            }
          }
        }
      }
    }
  }

  thread.start()
}