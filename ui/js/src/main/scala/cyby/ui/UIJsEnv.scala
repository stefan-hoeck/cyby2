/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui

import cats.Monoid

import io.circe.Json
import org.scalajs.dom

import msf.js.{UIEvent}

/**
  * Provides additional type aliases especially for often
  * used signal functions
  */
trait UIJsEnv extends UIEnv {
  /** Logging and IO */
  type LoggerIO[A] = cats.data.WriterT[IO,Logs,A]

  /** Signal function over LoggerIO */
  type LogSF[A,B] = msf.SF[LoggerIO,A,B]

  /** Event stream over LoggerIO */
  type LogEF[A,B] = msf.EF[LoggerIO,A,B]

  /*
   * Input type of CyBy's main signal function:
   * User credentials (therefore, a user must be logged in),
   * an optional result returned by the server and the actual
   * event from the user interface
   */
  type MainIn = (Creds,Option[Result],Option[UIEvent])

  /**
    * Input type of the data accumulator
    */
  type AccumIn  = Option[Result]

  type AccumOut = (St,ExpandOut)

  /**
    * Input type for the expander
    */
  type ExpandIn           = (Creds,Option[UIEvent])

  type ExpandOut          = ((Set[UIdP],EditSt),MainIn)

  type EditorEnv          = (St,Creds)

  /**
    * Environment required by the editor module
    */
  type EditorIn           = (Option[UIEvent],EditorEnv)

  /**
    * Environment required by the display module
    */
  type DisplayIn          = (Option[UIEvent],DispEnv)

  /**
    * Environment required by the explorer module
    */
  type ExplorerEnv        = (St,Creds,Option[Result])

  type ExplorerIn         = (Option[UIEvent], ExplorerEnv)

  type ExplorerOut        = (ExpSt,AccumOut)

  /**
    * Environment required by the main controller of the explorer
    */
  type ExpIn              = (Option[UIEvent], ExpEnv)


  //----------------------------------------------------------------------
  //                      Authentication
  //----------------------------------------------------------------------


  type AuthOut = ((Auth,Option[Result]),Option[UIEvent])


  /**
    * Input required of the Login functionality
    */
  type LoginIn = (Option[UIEvent],Auth)


  //----------------------------------------------------------------------
  //                      Http Commands
  //----------------------------------------------------------------------
  
  /**
    * Description of "load" events to be sent to the HTTP
    * server.
    */
  sealed trait Load { def l: Load = this }

  object Load {
    /**
      * loads all data from the server. this probably excludes
      * incrementally loaded data like compounds. however, the
      * actual implementation depends on function loadAll in
      * trait cyby.ui.CoreEnv.Loader
      */
    case class All(c: Creds) extends Load

    /**
      * causes a login request with the given user name
      * and password to be sent to the server
      */
    case class Login(name: String, pw: String) extends Load

    /**
      * causes a logout request with the given credentials
      * to be sent to the server
      */
    case class Logout(c: Creds) extends Load

    /**
      * causes a mutation request to be sent to the server
      */
    case class Mutate(h: DataType, json: Json, c: Creds) extends Load

    /**
      * causes a request for linking a file to be sent to the server
      */
    case class AddFile(h: DataType, json: Json, fil: dom.File, c: Creds) extends Load

    /**
      * causes a request for loading data of the given type
      * to be sent to the server
      */
    case class Path(dt: DataType, c: Creds) extends Load

    /**
      * causes an already encoded query
      * to be sent to the server
      */
    case class Query(q: String, c: Creds) extends Load

    /**
      * causes a request for exporting data
      * to be sent to the server
      */
    case class Export(s: ExpSettings, c: Creds) extends Load

    /**
      * causes a request for storing updated user settings
      * to be sent to the server
      */
    case class Settings(s: USettings, c: Creds) extends Load

    /**
      * causes an arbitrary json-formatted request
      * to be sent to the server
      */
    case class AnyJson(c: Creds, pth: String, json: Json) extends Load

    /**
      * dummy if nothing is to be done
      */
    case object NoLoad extends Load
  }


  //----------------------------------------------------------------------
  //                      Logging
  //----------------------------------------------------------------------

  /**
    * Almost all signal functions in the UI collect two lists
    * in the background: log messages and load requests. These
    * are grouped together in this datatype. Only at the
    * very end of the user interface's signal function are these
    * two lists extracted and processed.
    */
  case class Logs(
    logs:  Vector[Log],
    loads: Vector[Load]
  )

  object Logs {
    implicit val monoidI: Monoid[Logs] = new Monoid[Logs] {
      val empty = Logs(Vector(), Vector())
      def combine(l1: Logs, l2: Logs) =
        Logs(l1.logs ++ l2.logs, l1.loads ++ l2.loads)
    }
  }



  //----------------------------------------------------------------------
  //                      Utility Functions
  //----------------------------------------------------------------------

  /**
    * The utility functions provided in this trait
    * are used to convert between different input and output types
    * of often used signal functions. Their behavior should be obvios
    * from the type declaration.
    */
  trait CyByHelper extends msf.SFHelper[LoggerIO] {
    val toLoginIn: SF[AuthOut,LoginIn] = arr{ case ((a,_),u) ⇒ u -> a }

    val toExpandIn: SF[MainIn, ExpandIn] = arr{ case (c,_,u) ⇒ c -> u }

    val toAccumIn: SF[ExpandOut, AccumIn] = arr{ case ((_,_),(_,o,_)) ⇒ o }

    val toDispIn: SF[AccumOut, DisplayIn] = arr{
      case (st,((uids,cmd),(cs,or,ui))) ⇒ (ui,DispEnv(or, uids, st, cs,cmd))
    }

    val toExplorerIn: SF[AccumOut, (ExplorerIn,ExpSt)] = arr{
      case (st,((_,cmd),(cs,r,ui))) ⇒ (ui,(st,cs,r)) -> ExpSt.ini(cs.settings)
    }

    val toEditIn: SF[AccumOut, EditorIn] = arr{
      case (st,((_,cmd),(cs,_,ui))) ⇒ (ui,(st,cs))
    }

    val toExpIn: SF[ExplorerOut, ExpIn] = arr{
      case (expSt,(st,((uids,cmd),(creds,ores,ui)))) ⇒
        (ui,ExpEnv(expSt,DispEnv(ores,uids,st,creds,cmd)))
    }

    val toMainIn: PartialFunction[AuthOut,MainIn] = {
      case ((Auth(Authenticated(u,h,ss),cmd),or),ui) ⇒ (Creds(u,h,ss,cmd),or,ui)
    }
  }

}
