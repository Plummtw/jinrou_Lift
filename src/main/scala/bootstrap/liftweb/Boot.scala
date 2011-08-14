package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, ConnectionIdentifier}
import _root_.java.sql.{Connection, DriverManager}
import _root_.org.plummtw.jinrou.model._
import _root_.org.plummtw.jinrou.view.IdenticonView
import _root_.javax.servlet.http.{HttpServletRequest}

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    Slf4jLogBoot.enable()
  
    if (!DB.jndiJdbcConnAvailable_?)
      DB.defineConnectionManager(DefaultConnectionIdentifier, DBVendor)

    // where to search snippet
    LiftRules.addToPackages("org.plummtw.jinrou")
    LiftRules.autoIncludeAjax = session => false

    //LiftRules.redirectAjaxOnSessionLoss = false

    //Schemifier.schemify(true, Log.infoF _, User)
    Schemifier.schemify(true, Log.infoF _, AdminManage, Room, RoomDay, SystemMessage, Talk, UserEntry, UserIcon, Vote, ItemVote, SpecialVote)

    // Build SiteMap
    //val entries = Menu(Loc("Home", List("index"), "Home")) :: User.sitemap
    ////LiftRules.setSiteMap(SiteMap(entries:_*))

    /*
     * Show the spinny image when an Ajax call starts
     */
    //LiftRules.ajaxStart =
    //  Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    /*
     * Make the spinny image go away when it ends
     */
    //LiftRules.ajaxEnd =
    //  Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    LiftRules.early.append(makeUtf8)

    S.addAround(DB.buildLoanWrapper)
    
    //DB.addLogFunc((query, len) => Log.info("The query: "+query+" took "+len+" milliseconds"))
    LiftRules.dispatch.append {
      case Req("identicon" :: identicon_str :: Nil, _, _) =>
        () => IdenticonView.render(identicon_str)
    }


    LiftRules.exceptionHandler.prepend {
      case (_, _, exception) => {
        Log.error(exception.getStackTrace.toString)
        RedirectResponse("/")
      }
    }


  }

  /**
   * Force the request to be UTF-8
   */
  private def makeUtf8(req: HttpServletRequest) {
    req.setCharacterEncoding("UTF-8")
  }  
}

/**
* Database connection calculation
*/
object DBVendor extends ConnectionManager {
  private var pool: List[Connection] = Nil
  private var poolSize = 0
  private val maxPoolSize = 75

  private def createOne: Box[Connection] = try {
    val driverName: String = Props.get("db.driver") openOr
      "org.apache.derby.jdbc.EmbeddedDriver"
    println(driverName)

    val dbUrl: String = Props.get("db.url") openOr
      "jdbc:derby:lift_example;create=true"
    println(dbUrl)  

    Class.forName(driverName)

    val dm = (Props.get("db.user"), Props.get("db.password")) match {
      case (Full(user), Full(pwd)) =>
        //println(user)
        //println(pwd)
        DriverManager.getConnection(dbUrl, user, pwd)

      case _ =>
        Log.info("No User Password")
        DriverManager.getConnection(dbUrl)
    }

    Full(dm)
  } catch {
    case e: Exception => e.printStackTrace; Empty
  }

  def newConnection(name: ConnectionIdentifier): Box[Connection] = {
    var counter : Int = 1
    var result : Box[Connection] = Empty
    while (result == Empty) {
      result = synchronized {
        pool match {
          case Nil if poolSize < maxPoolSize =>
            Log.warn("Create Pool" + poolSize)
            val ret = createOne
            poolSize = poolSize + 1
            ret.foreach(c => pool = c :: pool)
            ret

          case Nil => Empty
          case x :: xs => try {
            pool = xs
            x.setAutoCommit(false)
            Full(x)
          } catch {
            case e =>
              println(e.toString)
              e.printStackTrace
              try {
                x.close
                Empty
              } catch {
                case e =>
                println(e.toString)
                e.printStackTrace
                Empty
              }
          }
        }
      }
      
      if (result == Empty) {
        Log.warn("Database Pool Overflow : " + counter)
        wait(100L + (new java.util.Random()).nextInt(500))
        counter += 1
      }
    }

    result
  }

  def releaseConnection(conn: Connection): Unit = synchronized {
    pool = conn :: pool
    notify
  }
}