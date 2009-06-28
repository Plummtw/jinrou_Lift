package org.plummtw.jinrou.snippet

import net.liftweb._ 
import mapper._ 
import http._ 
import SHtml._ 
import util._

class HelloController {
  def game_info = <span>
                現在時間：{new _root_.java.util.Date} <br/>
                Lift 版本： {LiftRules.liftVersion }   <br/><br/>
                OS 資訊：{System.getProperty("os.arch")} 
                             {System.getProperty("os.name")}
                             {System.getProperty("os.version")} <br/> 
                Java 版本： {System.getProperty("java.runtime.version")} <br/>
                檔案編碼：  {System.getProperty("file.encoding")} <br/>
                Web Server：{S.servletSession.map(u => u.getServletContext().getServerInfo()) openOr ""}
              </span>

}

