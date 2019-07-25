/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui

import cats.implicits.{none ⇒ _, _}

import cyby.dat.{UserLevel, format ⇒ F, Mol, Name, CyByEnv}
import cyby.query.{SubstanceQuery, StatisticsQuery, Q, Chain, QuickSearch}

import cyby.msf.js.{Node, nodes}

import export.Format

/**
  * Environment used in the UI. Declares important types and
  * utility functions used throughout the user interface in CyBy.
  */
trait UIEnv extends CyByEnv {

  /**
    * Type representing the accumulated state of the UI, consisting
    * of all data objects sent by the server, which have to be
    * kept in memory.
    */
  type St

  /**
    * Type representing element IDs in the HTML document.
    */
  type UIdP = cyby.ui.UId[DataType,Path]

  /**
    * Companion object with unapply method allowing us to
    * pattern match over element IDs.
    */
  object UIdP {
    import implicits.{pReadI, dtReadI}

    def unapply(s: String): Option[UIdP] =
      UId.fromSplit[DataType,Path](s split "-" toList)
  }


  //----------------------------------------------------------------------
  //                      Authentication
  //----------------------------------------------------------------------


  
  import implicits.{colDecI, fldDecI, colEncI, fldEncI, colKeyDecI,
                    colKeyEncI, useDecI, useEncI, setDecI, setEncI}


  /**
    * Sum type representing the authentication state of the running
    * CyBy UI
    */
  @io.circe.generic.JsonCodec sealed trait AuthSt

  /**
    * Properly authenticated users get a descrptive Use object,
    * a session hash and the actual user settings from the
    * server.
    */
  case class Authenticated(u: Use, hash: String, sets: USettings) extends AuthSt

  /**
    * User has not logged in yet (or is logged out again).
    */
  case object NotAuthenticated extends AuthSt

  object AuthSt



  /**
    * Command specifying whether the authentication state of
    * the actual user has changed.
    */
  sealed trait AuthCmd

  /**
    * User has just logged in.
    */
  case object HasLoggedIn extends AuthCmd

  /**
    * User has just logged out.
    */
  case object HasLoggedOut extends AuthCmd

  /**
    * Parts of the user's data (Alias etc.) have changed.
    */
  case object UserUpdated extends AuthCmd

  /**
    * User settings have changed.
    */
  case object UserSettingsChanged extends AuthCmd

  /**
    * Authentication state has not changed.
    */
  case object NoAuth extends AuthCmd

  /**
    * UI was freshly initialized (for instance after pressing the
    * reload button in the browser, or when visiting the page
    * for the first time).
    */
  case object AuthInit extends AuthCmd


  /**
    * Data type grouping all authentication information.
    */
  case class Auth(st: AuthSt, cmd: AuthCmd) {
    def creds: Option[Creds] = st match {
      case Authenticated(u,h,sets) ⇒ some(Creds(u, h, sets, cmd))
      case NotAuthenticated        ⇒ none
    }
  }

  /**
    * User credentials. These are part of the (read-only) environment of
    * all parts of the UI available only when a user has properly logged
    * in.
    */
  case class Creds(
    user:     Use,
    hash:     String,
    settings: USettings,
    cmd:      AuthCmd,
  ){
    lazy val credsQ: String = s"?user=${alias(user).v}&hash=${hash}"
  }

  //----------------------------------------------------------------------
  //                      Display
  //----------------------------------------------------------------------

  /**
    * Collected environment used to display major elements in the UI.
    *
    * @param res:   actual Result from decoding the latest server response
    *               (if any)
    * @param exp:   set IDs of expanded nodes in the UI. all other expandable
    *               nodes will be collapsed.
    * @param st:    actual accumulated state of data objects sent by the server
    * @param creds: credentials of the logged in user
    * @param cmd:   actual editing state
    */
  case class DispEnv(
    res:   Option[Result],
    exp:   Set[UIdP],
    st:    St,
    creds: Creds,
    cmd:   EditSt,
  ) extends WithDispEnv { def dispEnv = this }

  /**
    * Utility trait providing additional functions for composite objects
    * having a field of type DispEnv
    */
  trait WithDispEnv {
    def dispEnv: DispEnv

    def mode:    EditMode    = dispEnv.cmd.mode

    def isEditingAs(lvl: UserLevel): Boolean =
      mode.isEditing && level(dispEnv.creds.user) >= lvl

    def ifEditing(s: ⇒ String): String = if (mode.isEditing) s else ""

    def ifEditingAs(lvl: UserLevel)(s: ⇒ Node): Node =
      if (isEditingAs(lvl)) s else nodes()
  }

  /**
    * Utility trait providing additional functions for composite objects
    * having a field of type DispEnv
    */
  trait DispEnvFields extends WithDispEnv {
    def res:     Option[Result] = dispEnv.res
    def exp:     Set[UIdP]      = dispEnv.exp
    def st:      St             = dispEnv.st
    def creds:   Creds          = dispEnv.creds
    def cmd:     EditSt         = dispEnv.cmd
  }



  //----------------------------------------------------------------------
  //                      Explorer
  //----------------------------------------------------------------------
  
  /**
    * Entry in a color gradient definition
    */
  type GradientEntry = (Double,F.Color)

  /**
    * Mapping of color gradients used for conditional formatting
    */
  type Gradients     = Map[Column,F.Format[Double]]
  
  /**
    * Column for displaying molecular structures
    */
  def structCol: Column
  
  /**
    * Default column for sorting
    */
  def defaultSortCol: Column

  def canSortBy(c: Column): Boolean = columnDesc(c).sortable
  
  def columnDesc(c: Column): F.ColumnDesc
  
  def columnPath(c: Column): String

  def columnLoc(st: St)(c: Column): String

  def molFieldToColumn(f: Mol.Field): Column



  /**
    * Commands affecting the state and behavior of the explorer
    */
  sealed trait ExpCmd
  
  case object DisplayChanged extends ExpCmd
  
  case object ExportChanged extends ExpCmd
  
  case object ModeChanged extends ExpCmd
  
  case object NumberOfColumnsChanged extends ExpCmd
  
  case object QueryChanged extends ExpCmd
  
  case object Scrolled extends ExpCmd
  
  case object SettingsChanged extends ExpCmd
  
  case object QueriesChanged extends ExpCmd
  
  case object SizeChanged extends ExpCmd
  
  case object ZoomedIn extends ExpCmd
  
  case object ZoomedOut extends ExpCmd


  /**
    * Different modes of the explorer
    */
  sealed abstract class ExpMode(val dispCount: Int)
  
  case object SubstanceTable extends ExpMode(explorer.DispCount)
  
  case object SubstanceGrid extends ExpMode(explorer.LoadCount)
  
  case object MethodTable extends ExpMode(explorer.DispCount)
  
  case object PlotsView extends ExpMode(0)


  /**
    * Commands used to run and adjust combined queries
    */
  sealed trait QueryCmd { def c: QueryCmd = this }

  case object RunQueryCmd extends QueryCmd

  case class DeleteQueryCmd(s: String) extends QueryCmd

  case class SaveQueryCmd(n: Name) extends QueryCmd


  /**
    * Environment describing the actual state and behavior of
    * the explorer
    */
  case class ExpEnv(
    expSt:   ExpSt,
    dispEnv: DispEnv,
  ) extends DispEnvFields {
    def expMode: ExpMode = expSt.mode
    def settings: USettings = expSt.settings
  }
  
  /**
    * Actual state of the explorer
    *
    * This state is adjusted and accumulated in several explorer units
    * (see trait cyby.ui.explorer.Explorer.ExpUnit). It affects the behavior
    * of all explorer controllers
    * (see trait cyby.ui.explorer.Explorer.Controller).
    * 
    */
  case class ExpSt(
    /** actual explorer mode */
    mode             : ExpMode,

    /** actual combined query*/
    query            : Query,

    /** actual row height*/
    rowH             : Int,

    /** actual number of grid columns*/
    gridColumns      : Int,

    /** actual commands affecting the explorer*/
    cmds             : List[ExpCmd],

    /** vertical size of the explorer (the rest is filled up by the query view) */
    verticalSize     : Int,

    /** horizontal size of the explorer (the rest is filled up by the navigator) */
    horizontalSize   : Int,

    /** actual user settings */
    settings         : USettings,
  ) {
  
    def exportSettings: ExpSettings = export.Settings(
      query, settings.exportFields, settings.exportFormat getOrElse export.Txt
    )
  
    lazy val columns: List[Column] = mode match {
      case SubstanceTable ⇒ settings.substanceColumns
      case MethodTable    ⇒ settings.methodColumns
      case SubstanceGrid  ⇒ Nil
      case PlotsView      ⇒ Nil
    }
  
    def changedToPlotsView: Boolean = cmds.exists(DisplayChanged == _) &&
                                      mode == PlotsView
  
    def changedToMethodView: Boolean = cmds.exists(DisplayChanged == _) &&
                                       mode == MethodTable
  
    def changedToExplorerView: Boolean = cmds.exists(DisplayChanged == _) && (
                                         mode == SubstanceTable ||
                                         mode == SubstanceGrid
                                       )
  }
  
  object ExpSt {
    import implicits.colEqI

    /**
      * Initial explorer state (right after loading the UI)
      */
    def ini(s: USettings): ExpSt = {
      val qini = s.lastQueryO getOrElse cyby.query.Query(
        Chain[Field](Nil),
        SubstanceQuery,
        defaultSortCol,
        false,
        0,
        explorer.LoadCount
      )

      ExpSt(
        SubstanceTable,
        qini,
        explorer.EnlargeIni,
        5,
        Nil,
        explorer.StartSize,
        explorer.StartSize,
        s,
      )
    }
  
    lazy val l  = lens[ExpSt]
    lazy val ql = lens[Query]
  
    /**
      * prepends a command to the actual explorer state
      */
    def cmd(c: ExpCmd): ExpSt ⇒ ExpSt = modL(l.cmds)(c :: _)
  
    /**
      * changes the actual query, prepending a corresponding
      * command to the list of commands, as well as setting the index
      * of the next compound to load to 0.
      */
    def modQuery(f: Query ⇒ Query): ExpSt ⇒ ExpSt =
      cmd(QueryChanged)              andThen
      modL(l.query)(f)               andThen
      setL(l.query.start)(0)         andThen
      setL(l.query.count)(explorer.LoadCount)
  
    /**
      * changes the actual query and user settings, prepending corresponding
      * commands
      */
    def quick(q: Q[Field], quick: QuickSearch): ExpSt ⇒ ExpSt =
      modQuery(setL(ql.query)(q)) andThen
      setL(l.settings.quickO)(some(quick)) andThen
      cmd(SettingsChanged)
  
    /**
      * changes the actual query and user settings, prepending corresponding
      * commands
      */
    def query(q: Q[Field]): ExpSt ⇒ ExpSt = 
      modQuery(setL(ql.query)(q)) andThen
      setL(l.settings.queryO)(some(q)) andThen
      cmd(SettingsChanged)

    /**
      * changes the actual query without affecting user settings,
      * prepending corresponding commands
      */
    def clickquery(q: Q[Field]): ExpSt ⇒ ExpSt = modQuery(setL(ql.query)(q))

    /**
      * adds a new combined query to the list of persisted queries
      */
    def storeQuery(q: Q[Field], n: Name): ExpSt ⇒ ExpSt = {
      val p = n -> q
      def mod(o: Option[List[(Name,Q[Field])]]) =
        some(o.fold(List(p))(p::_).sortBy(_._1.v))

      modL(l.settings.queriesO)(mod) andThen
      cmd(QueriesChanged)
    }

    /**
      * deletes a query from the list of persisted queries
      */
    def deleteQuery(s: String): ExpSt ⇒ ExpSt = {
      def mod(o: Option[List[(Name,Q[Field])]]) = for {
        ps <- o
        i  <- Read[Int] read s
      } yield ps.zipWithIndex.collect{ case (p,j) if i =!= j ⇒ p }

      modL(l.settings.queriesO)(mod) andThen
      cmd(QueriesChanged)
    }
  
    /**
      * changes the sorting order of hit sets
      */
    def sort(c: Column): ExpSt ⇒ ExpSt = modQuery{
      case q if q.sort === c ⇒ ql.reverse.modify(q)(!_)
      case q                 ⇒ q.copy(sort = c, reverse = false)
    }
  
    /**
      * increases the vertical size of the explorer
      */
    def incVertical: ExpSt ⇒ ExpSt = st ⇒
      if (st.verticalSize >= explorer.MaxSize) st
      else cmd(SizeChanged) andThen modL(l.verticalSize)(_ + 1) apply st
  
    /**
      * decreases the vertical size of the explorer
      */
    def decVertical: ExpSt ⇒ ExpSt = st ⇒
      if (st.verticalSize <= explorer.MinSize) st
      else cmd(SizeChanged) andThen modL(l.verticalSize)(_ - 1) apply st
  
    /**
      * increases the horizontal size of the explorer
      */
    def incHorizontal: ExpSt ⇒ ExpSt = st ⇒
      if (st.horizontalSize >= explorer.MaxSize) st
      else cmd(SizeChanged) andThen modL(l.horizontalSize)(_ + 1) apply st
  
    /**
      * decreases the horizontal size of the explorer
      */
    def decHorizontal: ExpSt ⇒ ExpSt = st ⇒
      if (st.horizontalSize <= explorer.MinSize) st
      else cmd(SizeChanged) andThen modL(l.horizontalSize)(_ - 1) apply st
  
    /**
      * adjusts number of grid view columns after parsing the provided
      * string as an Int
      */
    def readCols(s: String): Option[ExpSt ⇒ ExpSt] =
      Read[Int] read s map numCols
  
    def numCols(n: Int): ExpSt ⇒ ExpSt =
      if (n < 2 || n > 20) identity
      else cmd(NumberOfColumnsChanged) andThen setL(l.gridColumns)(n)
  
    /**
      * changes the actual explorer mode, possibly adjusting
      * the actual query
      */
    def mode(m: ExpMode): ExpSt ⇒ ExpSt = st ⇒
      if (st.mode == m) st
      else m match {
        case MethodTable ⇒ 
          cmd(DisplayChanged) andThen
          setL(l.mode)(m)     andThen
          setL(l.query.qtype)(StatisticsQuery) apply st
        case PlotsView ⇒ 
          cmd(DisplayChanged) andThen setL(l.mode)(m) apply st
        case _ ⇒ st.mode match {
          case SubstanceGrid  ⇒ 
            cmd(ModeChanged) andThen setL(l.mode)(m) apply st
          case SubstanceTable ⇒ 
            cmd(ModeChanged) andThen setL(l.mode)(m) apply st
          case MethodTable    ⇒ 
            cmd(DisplayChanged) andThen
            setL(l.mode)(m)     andThen
            setL(l.query.qtype)(SubstanceQuery) apply st
          case PlotsView      ⇒ 
            cmd(DisplayChanged) andThen setL(l.mode)(m) apply st
        }
      }
  
    /**
      * adjusts the list of defined gradients
      */
    def formats(gs: Gradients): ExpSt ⇒ ExpSt =
      cmd(SettingsChanged) andThen modL(l.settings.doubleFormats)(_ ++ gs)
  
    /**
      * adjusts export settings and issues a corresponding command
      */
    def export(p: (Format,Boolean,List[Field])): ExpSt ⇒ ExpSt =
      cmd(ExportChanged) andThen
      setL(l.settings.exportFormat)(some(p._1)) andThen
      setL(l.settings.exportSelectionO)(some(p._2)) andThen
      setL(l.settings.exportFieldsO)(some(p._3))
  
    /**
      * adjusts list of displayed columns
      */
    def columns(f: List[Column] ⇒ List[Column]): ExpSt ⇒ ExpSt = st ⇒
      cmd(SettingsChanged) andThen (st.mode match {
        case SubstanceTable ⇒ modL(l.settings.substanceColumns)(f)
        case MethodTable    ⇒ modL(l.settings.methodColumns)(f)
        case SubstanceGrid  ⇒ modL(l.settings.substanceColumns)(f)
        case PlotsView      ⇒ modL(l.settings.substanceColumns)(f)
      }) apply st
  
    /**
      * deletes a visible column
      *
      * @param i: the column's index
      */
    def delColumn(i: Int): ExpSt ⇒ ExpSt =
      columns(_ splitAt(i) match {
        case (fst,snd) ⇒ tailOption(snd).fold(fst)(fst ::: _)
      })
  
    /**
      * inserts a new visible column
      *
      * @param i: the column's index
      * @param c: the new column
      */
    def addColumn(i: Int)(c: Column): ExpSt ⇒ ExpSt =
      columns(_ splitAt(i + 1) match {
        case (fst,snd) ⇒ fst ::: (c::snd)
      })
  
    /**
      * replaces a visible column with another one
      *
      * @param i: the column's index
      * @param c: the new column
      */
    def replaceColumn(i: Int)(c: Column): ExpSt ⇒ ExpSt =
      columns(_ splitAt(i) match {
        case (fst,h::t) ⇒ fst ::: (c :: t)
        case (fst,Nil)  ⇒ fst
      })
  
    /**
      * sets all commands to the empty list. this function is
      * called at the beginning of every evaluation stept to
      * clear all commands from the last evaluation step.
      */
    lazy val reset: ExpSt ⇒ ExpSt = setL(l.cmds)(Nil)
  
    /**
      * increases height of compound rows
      */
    lazy val zoomIn: ExpSt ⇒ ExpSt = zoom(true)
  
    /**
      * decreases height of compound rows
      */
    lazy val zoomOut: ExpSt ⇒ ExpSt = zoom(false)
  
    /**
      * signals that the user has scrolled the compounds table
      */
    lazy val scroll: ExpSt ⇒ ExpSt = cmd(Scrolled)
  
    private def zoom(zoomIn: Boolean): ExpSt ⇒ ExpSt = st ⇒
      if (zoomIn)
        if (st.rowH >= explorer.EnlargeMax) st else
        cmd(ZoomedIn) andThen modL(l.rowH)(_ + explorer.EnlargeStep) apply st
      else
        if (st.rowH <= explorer.EnlargeMin) st else
        cmd(ZoomedOut) andThen modL(l.rowH)(_ - explorer.EnlargeStep) apply st
  
  }
}

