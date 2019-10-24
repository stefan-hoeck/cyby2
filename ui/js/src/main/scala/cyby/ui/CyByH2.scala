/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui

import cats.data.RWST
import cats.{Eq, Monad, Monoid}, cats.implicits.{none ⇒ _, _}

import cyby.dat.{Mol, MolFile, EditRes, Found, Added, Updated, Deleted}
import chemdoodle.{SketcherCanvas, ViewerCanvas, Molecule, ChemDoodle}
import select.{Event, Model, Toggle, Range, Single}

import scala.scalajs.js.Dynamic
import org.scalajs.dom.{raw, FormData}, raw.EventTarget

import msf.js.{JSHelper, UIEvent, HttpEvent, HttpError, HttpTimedOut,
               HttpResponseText, HttpMethod, StringContent,
               FormContent, Handler, RequestContent, HttpRequest}

import HttpMethod.{POST, GET}

import UIEvent.{Click, HashChange}

/**
  * Environment for DOM-based interactive UI modules.
  */
trait CoreEnv extends UIJsEnv with TextEnv {
  import tags._

  /**
    * Helper class for writing UI functions over a given
    * monad F[_]. Gives access to DOM-functions as well as
    * signals and sources provided by JSHelper.
    */
  trait CoreH[F[_],I] extends JSHelper[F,UIdP,CyByClass,I] {
    self ⇒ 
  
    private implicit def monadI2: Monad[Eff] = effAsMonad
  
    //@TODO move to msf
    def askId: Html[Option[UIdP]] = Html.ask map (_ flatMap (e ⇒ UIdP unapply e.id) )
   
    override def idToString(i: UIdP): String = i.id
   
    override def clsToString(c: CyByClass): String = c.toString
   
    override def uniqueId(i: Long): UIdP = UId Unique i
   
    override def unapplyId(s: String): Option[UIdP] = UIdP unapply s
   
    /**
      * Fires whenever an expand button (element with Expand ID) is clicked.
      */
    def expanding: Src[UIdP] = Src.ui.collect { case Click(ID(UId.Expand(i))) ⇒ i }
  
    /**
      * Hides element with given UI whenever the incoming signal is evaluated
      */
    def hide[A](id: UIdP): Sink[A] = const(dhide) >>> sink.attribute(id)
   
    /**
      * Shows element with given UI whenever the incoming signal is evaluated
      */
    def show[A](id: UIdP): Sink[A] = const(dshow) >>> sink.attribute(id)
  
    /**
      * Create a sketcher canvas for drawing molecules
      *
      * @param id : ID of the canvas
      * @param os : optional initial molecule (in mol format)
      * @param width : canvas width
      * @param height : canvas height
      * @param floatDrawTool : true if draw tool should be floating
      * @param resizable : true if canvas should be resizable (doesn't seem to work)
      * @param includeQuery : include the ability to draw query molecules
      * @param requireStartingAtom : true if starting atom is required
      */
    def sketcher(
      id                  : UIdP,
      os                  : Option[MolFile],
      width               : Int,
      height              : Int,
      floatDrawTools      : Boolean = false,
      resizable           : Boolean = true,
      includeQuery        : Boolean = false,
      requireStartingAtom : Boolean = false,
    ): Eff[Option[SketcherCanvas]] =
      Eff delayedTryO {
        val mol = os flatMap { m ⇒ tryO(ChemDoodle.readMOL(m.v,0D)) }
        val options = Dynamic.literal(
          "resizable"           -> resizable,
          "includeQuery"        -> includeQuery,
          "floatDrawTools"      -> floatDrawTools,
          "requireStartingAtom" -> requireStartingAtom,
        )
        val c = new SketcherCanvas(id.id, width, height, options)
        c.specs.bonds_width_2D = 0.8D
        c.specs.bonds_saturationWidthAbs_2D = 2.6D
        c.specs.bonds_hashSpacing_2D = 2.5D
        c.specs.atoms_font_size_2D = 10
        c.specs.atoms_displayTerminalCarbonLabels_2D = false
        c.specs.atoms_useJMOLColors= false
        mol foreach c.loadMolecule
        c.repaint()
        c
      }

    /**
      * Converts a ChemDoodle Molecule to a MolFile
      */
    def writeMol(m: Molecule): Option[MolFile] =
      tryO(MolFile fromString ChemDoodle.writeMOL(m))
   
    /**
      * Displays a cyby.dat.Mol in the canvas with the given ID
      */
    def dispMolM(id: UIdP, mol: Mol): Eff[Unit] = dispMolS(id)(mol.structure)
   
    /**
      * Displays a ChemDoodle Molecule in the canvas with the given ID
      */
    def dispMol(id: UIdP)(mol: Molecule): Eff[Unit] = dispMolO(id)(Some(mol))

    /**
      * Displays a molecule in .mol format in the canvas with the given ID
      */
    def dispMolS(id: UIdP)(mol: MolFile): Eff[Unit] =
      Eff.delayedTryO(ChemDoodle.readMOL(mol.v, 0D)) >>= (_ traverse_ dispMol(id))
   
    /**
      * Displays an optional molecule the canvas with the given ID
      */
    def dispMolO(id: UIdP)(mol: Option[Molecule]): Eff[Unit] = Eff delayedTry {
      val c = new ViewerCanvas(id.id)
      c.specs.bonds_width_2D = 0.8D
      c.specs.bonds_saturationWidthAbs_2D = 2.6D
      c.specs.bonds_hashSpacing_2D = 2.5D
      c.specs.atoms_font_size_2D = 10
      c.specs.atoms_displayTerminalCarbonLabels_2D = false
      c.specs.atoms_useJMOLColors= false
   
      mol foreach c.loadMolecule
    }
  
    /**
      * Checks whether the actual HTML element is the first
      * child of its parent
      */
    def isFirstChild: Html[Boolean] = (Html.ask,get.parent).mapN{
      case (Some(e),Some(p)) ⇒ e == p.firstChild
      case _                 ⇒ false
    }
  
    /**
      * Action, firing whenever the given element is
      * the first child of its parent
      */
    def onFirstChild[E:ToElem](e: E): Action =
      Src.ui.collectF{ case Click(_) ⇒ at(e)(isFirstChild)} filter identity void
  
    /**
      * Signal representing the actual editing state of the UI
      */
    lazy val editCmd: Signal[EditSt] =
      Src.click(UId.ToggleEditing).sf.scan(NoChange(NotEditing).c)(EditSt.accum)
  }
  
  /**
    * Core environment specialized to the IO Monad
    */
  trait CoreIO[I] extends CoreH[IO,I] {
    override def effAsLiftIO: msf.LiftIO[IO] = msf.LiftIO[IO]
   
    override def effAsMonad: Monad[IO] = Monad[IO]
  }

  /**
    * Most UI elements can be described as a Monad over environment
    * E (Reader Monad, dependency injection), state S (state monad),
    * and a collection of logs (writer monad).
    *
    * This trait provides a convenient environment for working
    * with signal functions in this situation.
    *
    * Above everything else we added side-effects via IO.
    */
  trait CyByH[E,S,I] extends CoreH[RWST[IO,E,Logs,S,?],I] {
    self ⇒ 

    type Env = E
    
    type ST = S
  
    val logSF = msf.SF.helper[LoggerIO]
   
    override def effAsLiftIO: msf.LiftIO[Eff] = msf.LiftIO[Eff]
   
    override def effAsMonad: Monad[Eff] = Monad[Eff]
  
    /**
      * Extracting environment and state into the signal function
      */
    def unrs[A,B](sf: SF[A,B]): LogSF[((A,E),ST), (B,ST)] = msf.rwst unRS sf
  
    /**
      * Extracting environment and state into the signal function,
      * looping back the state
      */
    def unrsBack[A,B](sf: SF[A,B]): LogSF[((A,E),ST), B] = msf.SF.loopBack(msf.rwst unRS sf)
  
    /**
      * Extracting environment and state into the signal function,
      * looping back the state, starting with the initial state given
      */
    def unrss[A,B](sf: SF[A,B])(ini: S): LogSF[(A,Env),B] = unrs(sf) loop ini
  
    /**
      * Extracting environment and state into the signal function,
      * using the empty value of a Monoid as the environment
      * (useful, if no environment is needed and its type is set to Unit)
      */
    def unrsM[A,B](sf: SF[A,B])(implicit M: Monoid[Env]): LogSF[(A,ST),(B,ST)] =
      logSF.arr[(A,S),((A,E),S)]{ case (a,s) ⇒ (a -> M.empty) -> s} >>>
      unrs(sf)
  
    /**
      * Extracting environment and state into the signal function,
      * using the empty value of a Monoid as the environment
      * (useful, if no environment is needed and its type is set to Unit)
      * and looping back the state starting with the initial state given.
      */
    def unrssM[A,B](sf: SF[A,B])(ini: S)(implicit M: Monoid[E]): LogSF[A,B] =
      unrsM(sf) loop ini
  
    lazy val getS: Eff[ST] = RWST.get
  
    lazy val askE: Eff[Env] = RWST.ask
  
    def setS(s: ST): Eff[Unit] = RWST set s
  
    def modS(f: ST ⇒ ST): Eff[Unit] = RWST modify f
  
    def tell(logs: Logs): Eff[Unit] = RWST tell logs
  
    def runLog(l: Log): Eff[Unit] = runLogs(Nel of l)
  
    def runLogs(ls: Nel[Log]): Eff[Unit] =
      tell(Logs(Vector(ls.toList: _*), Vector()))
  
    def debug(msg: ⇒ String): Eff[Unit] = runLog(cyby.Log(Debug, msg))
  
    def info(msg: ⇒ String): Eff[Unit] = runLog(cyby.Log(Info, msg))
  
    def warn(msg: ⇒ String): Eff[Unit] = runLog(cyby.Log(Warning, msg))
  
    def error(msg: ⇒ String): Eff[Unit] = runLog(cyby.Log(Error, msg))
  
    def debugH(msg: ⇒ String): Html[Unit] = Html liftE debug(msg)
  
    def infoH(msg: ⇒ String): Html[Unit] = Html liftE info(msg)
  
    def warnH(msg: ⇒ String): Html[Unit] = Html liftE warn(msg)
  
    def errorH(msg: ⇒ String): Html[Unit] = Html liftE error(msg)
  
    def loads(ls: List[Load]): Eff[Unit] =
      tell(Logs(Vector(), ls.toVector))
  
    def load(l: Load): Eff[Unit] = loads(List(l))
  
    lazy val env: Signal[Env] = constS(askE)
  
    lazy val st: Signal[ST] = constS(getS)
  
    lazy val doSet: Sink[ST] = liftS(setS)
  
    lazy val doMod: Sink[ST ⇒ ST] = liftS(modS)
  }

  /**
    * The Decoder is at the beginning of CyBy's signal function.
    * It decodes HTTP responses and passes on input events from
    * the UI.
    *
    * It is crutial that the Decoder
    * is only evaluated once, since decoding HTTP responses
    * is typically slow.
    */
  trait Decoder extends CyByH[Unit,Unit,In] {
    import implicits.resDecI
  
    def evToUIEvent(e: Ev) = e.toOption
  
    /**
      * Signal function decoding and logging incoming HttpEvents
      * converting them to optional results.
      */
    def run: LogSF[Ev,Option[Result]] = {
      val logErrs = (e: Nel[Err]) ⇒ runLogs(e map loc.logErr) as none[Result]

      val decode = liftS[HttpEvent,Option[Result]]{
        case HttpError(url)      ⇒ logErrs(Nel of LoadErr(url))
        case HttpTimedOut(url)   ⇒ logErrs(Nel of Timeout(url))
        case HttpResponseText(s) ⇒ parseAndDecode[Result](s) match {
          case Invalid(nel)      ⇒ logErrs(nel)
          case Valid(r)          ⇒ tell(Logs(loc logResult r, Vector())) as some(r)
        }
      }

      val sig = idS[Ev].collect{case Left(he) ⇒ he}.andThenS(decode).onEmpty(none)

      unrssM(sig)(unit)
    }
  }


  /**
    * Checks decoded results from the server for changes
    * in the authentication state of the actual user.
    *
    * If this is the case, the authentication state is
    * adjusted and a corresponding AuthCmd issued, so that
    * other parts in the UI can adjust themselves accordingly.
    */
  trait DoAuth extends CyByH[Unit,Unit,Option[Result]] {
    import implicits.useEqI

    def run: LogSF[Ev,Auth] = unrssM(behavior)(unit)
  
    /**
      * Scans decoded results updating the Auth state whenever
      * something interesting happens. This uses functions
      * resultIsLogin, resultIsUserSettingsChanged,
      * resultIsLogout, and resultToUsers in the background.
      */
    lazy val behavior: Signal[Auth] =
      idS[Ev].scan(Auth(NotAuthenticated, AuthInit))(doAuth)
  
    def evToUIEvent(e: Ev) = None
  
    /**
      * Tries to convert a decoded Result to user credentials
      */
    def resultIsLogin(r: Result): Option[(Use,String,USettings)]
  
    /**
      * Tries to convert a decoded Result to a new set of
      * user settings
      */
    def resultIsUserSettingsChanged(r: Result): Option[USettings]
  
    /**
      * Returns true if the decoded result means that the user
      * has logged out
      */
    def resultIsLogout(r: Result): Boolean
  
    /**
      * Extracts a list of new users from a result (returns
      * the empty list of no users were loaded)
      */
    def resultToUsers(r: Result): List[Use]
  
    private def doAuth(a: Auth, o: Option[Result]): Auth = {
      def onLogout(r: Result): Option[Auth] =
        if (resultIsLogout(r)) Some(Auth(NotAuthenticated, HasLoggedOut))
        else None
  
      def onLogin(r: Result): Option[Auth] = resultIsLogin(r).map{
        case (u,h,sets) ⇒ Auth(Authenticated(u, h, sets), HasLoggedIn)
      }
  
      def onSettingsChanged(r: Result): Option[Auth] = a.st match {
        case NotAuthenticated   ⇒ none[Auth]
        case Authenticated(u,h,_) ⇒ resultIsUserSettingsChanged(r).map(s ⇒ 
          Auth(Authenticated(u,h,s), UserSettingsChanged)
        )
      }
  
      def onUserChanged(r: Result): Option[Auth] = a.st match {
        case NotAuthenticated   ⇒ none[Auth]
        case Authenticated(u,h,sets) ⇒ resultToUsers(r).collect{
          case u2 if (userId(u2) === userId(u)) && (u2 =!= u) ⇒
            Auth(Authenticated(u2,h,sets),UserUpdated)
        }.headOption
      }
  
      a.cmd match {
        case AuthInit ⇒ a.st match {
          case s@Authenticated(_,_,_) ⇒ Auth(s, HasLoggedIn)
          case s@NotAuthenticated     ⇒ Auth(s, HasLoggedOut)
        }
        case _ ⇒ 
          o.flatMap(r ⇒
            onLogout(r)      orElse
            onLogin(r)       orElse
            onUserChanged(r) orElse
            onSettingsChanged(r)
          ).getOrElse(Auth(a.st, NoAuth))
      }
    }
  }


  /**
    * Signal function of the login screen.
    */
  object Login extends CyByH[Unit,Unit,LoginIn] {
  
    import Src.{click, enter}, UId.{Load ⇒ _, _}, sink.innerHtml
  
    override def evToUIEvent(e: Ev) = e._1
  
    /**
      * Checks if the user has logged out and switches to the login
      * screen if this is the case.
      *
      * If a user is trying to login by clicking the login button
      * or pressing enter in the password field, sends a login
      * request to the server.
      */
    def run: LogSF[LoginIn,Unit] = {
      val getAt = (u: UIdP) ⇒ at(u)(get.value <* set.value(""))

      val login: Sink[Unit] =
        constS((getAt(LoginName), getAt(LoginPw)).mapN(Load.Login) >>= load)

      val sig = idS[Ev].collect{ case (_,Auth(_,HasLoggedOut)) ⇒ Txt.login } >>-
                innerHtml(Content)                                           |+|
                (click(LoginBtnId) |+| enter(LoginPw)) >>- login

      unrssM(sig)(unit)
    }
  }


  /**
    * Signal function for expanding and collapsing
    * elements in the UI
    */
  object Expander extends CyByH[Unit,Set[UIdP],ExpandIn] {
    override def evToUIEvent(e: Ev) = e._2
  
    /**
      * Clears list of expanded elements on login
      * or logout and expands / collapses elements
      * whenever a corresponding button is clicked. The
      * set of expanded IDs is treated as mutable state
      * and affects how newly loaded elements are displayed
      * if passed on to later signal functions.
      */
    def run: LogSF[ExpandIn,(Set[UIdP],EditSt)] = {
      val clear = idS[Ev].map(_._1.cmd).collect{
                    case HasLoggedIn  ⇒ Set[UIdP]()
                    case HasLoggedOut ⇒ Set[UIdP]()
                  } >>- doSet

      val adjust = (id: UIdP) ⇒ for {
        _ <- modS(s ⇒ if (s(id)) s - id else s + id)
        b <- getS map (s ⇒ s(id))
        _ <- at(id)(set.attribute(tags.dhidden := !b))
        _ <- at(UId Expand id)(set.attribute(tags dexp b))
      } yield unit

      val sig: Signal[Set[UIdP]] =
        (clear |+| (expanding >>- liftS(adjust))) >>> constS(getS)

      unrssM(sig zip editCmd)(Set.empty)
    }
  }


  /**
    * The Accumulator accumulates the UI state from results
    * returned by the server. How this is actually done (how
    * a result from the server affects the actual UI state)
    * has to be specified by implementors.
    */
  trait Accumulator extends CyByH[Unit,Unit,Option[Result]] {
  
    override def evToUIEvent(e: Ev) = None
  
    def run: LogSF[Ev,St] = unrssM(behavior)(unit)
  
    /** Signal of UI state */
    def behavior: Signal[St] = idS[Ev].scan(ini)(accumS)
  
    /** Initial state */
    def ini: St
  
    /**
      * Accumulation function: Takes the actual state and an optional
      * result from the server and returns a new state.
      */
    def accumS(st: St, r: Option[Result]): St
   
    /**
      * Utility function adjust a list of items in accordance with
      * the latest EditRes from the server.
      *
      * Behavior (depending on value of EditRes):
      * Found : this value signals that either a new hitset has
      * been returned (index of first item ist at 0) or that additional
      * hits have been requested and are now available. In the first case,
      * the original list is replaced with the new one, in the second case
      * the additional results are appended to the original list.
      *
      * Added : A new item has been created. It is prepended to the list
      * of loaded items.
      *
      * Updated : An item has been updated. It is replaced in the list of
      * loaded items.
      *
      * Deleted : An item has been deleted. It is removed from the list
      * of loaded items.
      */
    def modRoot[A,B](r: EditRes[A])(id: A ⇒ Id[B]): List[A] ⇒ List[A] = xs ⇒
      r match {
        case Found(as,_,0) ⇒ as
        case Found(as,_,_) ⇒ xs ::: as
        case Added(a)      ⇒ a :: xs
        case Updated(a)    ⇒ xs map (x ⇒ if (id(x) === id(a)) a else x)
        case Deleted(a)    ⇒ xs filterNot (id(_) === id(a))
      }
  }


  /**
    * accumulates selected items and mutates a state
    * accordingly
    *
    * @tpara E : environment providing access to the total list
    *            of selectable values
    * @tpara S : state grating access to a select.Model
    * @tpara A : type of selectable items
    * @tpara B : type of (unique) identifiers (with an instance of cats.Eq)
    *            for values of type A.
    */
  trait Selector[E,S,A,B] extends CyByH[E,S,Option[UIEvent]] {
    /** cats.Eq instance for identifier type B */
    protected def eqInst: Eq[B]
 
    /** lens to modify the selection model in a state object */
    protected def modelLens: Lens[S,Model[B]]
  
    private[this] implicit lazy val eqI: Eq[B] = eqInst
  
    
    /** returns the identifier from a selectable value */
    def getId(a: A): B
  
    /**
      * Reads a selectable ID from a string.
      *
      * Invariant : read(toIdString(b)) == Some(b)
      */
    def read(str: String): Option[B]
    
    /**
      * Converts a selectable ID to a string. This will
      * be wrapped in a UId.Select type to changed
      * the formatting of the elements in the UI.
      */
    def toIdString(b: B): String
  
    /** list of selectable values */
    def values(e: Env): List[A]
  
    /**
      * Returns true if users are allowed to select entries merely by
      * clicking them without holding down SHIFT or CTRL
      */
    def clickSelect(e: Env): Boolean
  
    /**
      * Processes incoming events from the UI updating the
      * underlying state if necessary
      */
    lazy val selectSignal: Signal[Unit] = {
      // necessary since b might be null (coming from the DOM)
      def check(b: ⇒ Boolean): Boolean = Option(b).exists(identity)

      val getModel = getS map modelLens.get

      def bAt(t: EventTarget): Eff[Option[B]] = at(t)(get.ids).map{
        _.collect{ case UId.Select(s) ⇒ read(s) }
         .headOption
         .flatten
      }
  
      def onEv(e: Event[B]): Eff[Unit] = for {
        vals <- askE map values
        mold <- getModel
        mnew =  Model.select(vals, mold)(getId)(e)
        _    <- mnew.del traverse_ select(false)
        _    <- mnew.add traverse_ select(true)
        _    <- debug(s"""Selected : ${mnew.selected mkString ", "}.""")
        _    <- modS(modelLens.set(_)(mnew))
      } yield ()

      def select(selected: Boolean)(b: B): Eff[Unit] =
        at(UId.Select(toIdString(b)))(set.attribute(tags.dselected := selected))
  
      def handle(env: Env, ev: Option[UIEvent]): Eff[Option[Event[B]]] = ev match {
        case Some(Click(e)) if !e.isInstanceOf[org.scalajs.dom.raw.MouseEvent]  ⇒ Eff pure none[Event[B]]
        case Some(Click(e)) if check(e.ctrlKey)  ⇒ bAt(e.target) map2 (Toggle(_).e)
        case Some(Click(e)) if check(e.shiftKey) ⇒ bAt(e.target) map2 (Range(_).e)
        case Some(Click(e)) if clickSelect(env)  ⇒ bAt(e.target) map2 (Single(_).e)
        case _                                   ⇒ Eff pure none[Event[B]]
      }

      (env,Src.ui.sf).mapN(handle).mapF{ _ flatMap (_ traverse_ onEv) }
    }
  }

  /**
    * The logger is at the end of CyBy's event stream.
    *
    * After sinking events through all other consumers like
    * the loader and the main display, the logger extracts
    * all log messages from a single event and displays
    * them at proper places of the UI.
    */
  object Logger extends CoreIO[(Logs,Option[UIEvent])] {
    override def evToUIEvent(e: Ev) = e._2
  
    def log: Sink[Ev] =
      (logs,logLevel)
        .mapN((ls,lvl) ⇒ ls filter hasLevel(lvl))
        .filter(_.nonEmpty)
        .scan(Vector[Log]())((as,bs) ⇒ (bs.reverse ++ as) take 10)
        .map(Txt.logs) >>- sink.innerHtml(UId.LogId) |+|
      logs.collectO(_ find hasLevel(Info) map Txt.loginLog) >>-
      sink.innerHtml(UId.LoginLog)
  
    private lazy val logs: Signal[Vector[Log]] = arr(_._1.logs)
  
    private def hasLevel(l: LogLevel) = (log: Log) ⇒ log.lvl >= l
  
    lazy val logLevel =
      idS[Ev].collect{ case (_,Some(HashChange(e))) ⇒ readLvl(e.newURL) }
             .collectO{ identity }
             .hold(Info : LogLevel)
  
    def readLvl(s: String): Option[LogLevel] = s.split("#").toList match {
      case _::"debug"::Nil ⇒ some(Debug)
      case _::"info" ::Nil ⇒ some(Info)
      case _::"warn" ::Nil ⇒ some(Warning)
      case _::"error"::Nil ⇒ some(Error)
      case _               ⇒ None
    }
  }


  /**
    * The Loader is the final consumer of all requests
    * that are to be sent to the HTTP server. It
    * collects requests from all major parts of the UI.
    */
  trait Loader extends CoreIO[UIEvent] {
    import io.circe.syntax._
    import implicits._

    type EventH = Handler[HttpRequest]
   
    /**
      * Implementer should use this function to call the given
      * load function with DataType objects representing all data
      * that should automatically be loaded on application
      * startup.
      */
    def loadAll(load: DataType ⇒ Eff[Unit]): Eff[Unit]
  
    override def evToUIEvent(e: Ev) = some(e)
  
    /**
      * Just an adapter to sink with an input type that
      * can more easily be combined with other signal functions
      * in the UI.
      */
    def run(h: EventH, prefix: String, timeout: Long): Sink[(Logs,Option[UIEvent])] =
      sink(h, prefix, timeout).contramap(_._1.loads)

    /**
      * Given a couple of configuration settings, this returns a
      * Sink (consumer) of Load objects. These will be converted
      * to HTTP requests and sent to the server.
      *
      * @param h : Handler of HTTP events
      * @param prefix : Prefix of Server URL
      * @param timeout : number of milliseconds until a HTTP request times out
      */
    def sink(h: EventH, prefix: String, timeout: Long): Sink[Vector[Load]] = {
      def url(p: String): String = if (prefix.isEmpty) p else s"{prefix}/p"

      def get(p: String, heads: (String,String)*) =
        Eff.liftIO(h(HttpRequest(url(p), StringContent(""), timeout, GET, heads.toList)))
  
      def getCreds(p: String, c: Creds) = get(s"${p}${c.credsQ}", jsonH)
  
      def post(p: String, msg: String, c: Creds) = postR(p, StringContent(msg), c)
  
      def postR(p: String, msg: RequestContent, c: Creds) = {
        val hd = msg match {
          case StringContent(_) ⇒ List(jsonH)
          case FormContent(_)   ⇒ Nil
        }
  
        Eff.liftIO(h(HttpRequest(s"${url(p)}${c.credsQ}", msg, timeout, POST, hd)))
      }
  
      def load(d: DataType, c: Creds) = getCreds(s"query/${d}", c)
  
      def query(c: Creds)(q: String) = post(s"query/$subType", q, c)
  
      def single(l: Load) = l match {
        case Load.Login(n,pw)   ⇒ get(s"login?user=${n}", "password" -> pw)
        case Load.All(c)        ⇒ loadAll(load(_,c))
        case Load.Logout(c)     ⇒ getCreds("logout", c)
        case Load.Path(p,c)     ⇒ load(p, c)
        case Load.Query(q,c)    ⇒ query(c)(q)
        case Load.Mutate(p,t,c) ⇒ post(s"mutate/${p}", t.noSpaces, c)
        case Load.Settings(s,c) ⇒ post(s"mutate/settings", (userId(c.user) -> s).asJson.noSpaces, c)
        case Load.Export(s, c)  ⇒ post(s"export", s.asJson.noSpaces, c)
        case Load.AddFile(p,j,f,c)  ⇒ {
          val fd = new FormData()
          fd.append("json", j.noSpaces)
          fd.append("file", f)
  
          postR(s"mutate/${p}", FormContent(fd), c)
        }
        case Load.AnyJson(c,p,j) ⇒ post(p, j.noSpaces, c)
        case Load.NoLoad         ⇒ Eff.unit
      }
  
      liftS[Vector[Load],Unit]{ _ traverse_ single }
    }
  
    private lazy val jsonH = "Content-type" -> "application/json"
  }
}

// vim: set ts=2 sw=2 et:
