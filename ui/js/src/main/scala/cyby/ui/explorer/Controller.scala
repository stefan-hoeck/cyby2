/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui
package explorer

import cats.Eq, cats.implicits.{none ⇒ _, _}

import cyby.dat.format.Gradient
import cyby.dat.{Added, Updated, Deleted, Found, EditRes, ToMol, UserSettings, Name}

import cyby.query.{Q, Prim, Chain, QuickSearch}
import cyby.ui.{IconType ⇒ IT, CompType ⇒ CT, WidgetType ⇒ WT, TitleType ⇒ TT}

import msf.js.{Handler, UIEvent, InputType ⇒ IPT, SelectEntry, Node, nodes}
import UIEvent.{Scroll, KeyPress, Click, DblClick}
import select.Model

import io.circe.syntax._

import org.scalajs.dom.document.getElementById

trait Explorer extends DomEnv {
  def querySels(s: USettings, ix: Option[Int]): List[SelectEntry] = 
    s.queryPairs.zipWithIndex.map{
      case ((n,_),i) ⇒ SelectEntry(some(i.toString), ix.fold(i === 0)(_ === i), n.v)
    }

  /**
    * Main controller of the explorer view. This class is responsible
    * for requesting more compounds when a user scrolls close to
    * the bottom of the view, for displaying freshly loaded compounds,
    * and for sending requests to the server based on query changes
    * made by the user.
    */
  abstract class Controller[Sub:ToMol,I:Eq](
    subs: St ⇒ List[Sub],
    h:    Handler[UIEvent],
    url:  String,
  ) extends DomH[ExpEnv,ControllerSt[I],Option[UIEvent]]
    with    Selector[ExpEnv,ControllerSt[I],Sub,I] {
    import UId.{Load ⇒ _, _}, set.innerHtml

    private lazy val mkId: Sub ⇒ UIdP = s ⇒ Select(toIdString(getId(s)))

    override protected def eqInst = Eq[I]

    override protected def modelLens = lens[ControllerSt[I]].select

    override def evToUIEvent(e: Ev) = e

    def values(e: Env): List[Sub] = subs(e.st)

    def clickSelect(e: Env) = e.expSt.mode == SubstanceGrid

    def run: LogSF[ExpIn,Unit] =
      unrss(behavior)(ControllerSt[I](true, 0, 0, 0, Model.empty))

    private def behavior: Signal[Unit] =
      env >>> liftS{ e ⇒
        e.expSt.cmds.traverse_(handleCmd(e)) *> 
        e.res.flatMap(subRes).traverse_(onLoad(_,e) *> loadNext(true))
      }                                                       |+|
      env.collect { case e if e.cmd.hasChanged ⇒ () }         >>-
      constS(registerScroll *> displayMols)                   |+|
      env.toEF.head.void                                      >>-
      constS(registerScroll *> loadFrom(0) *> adjSize)        |+|
      selectSignal                                            |+|
      env.map(e ⇒ e.res >>= exportRes).collectO(identity)     >>- download

    /**
      * Adjusts the UI according to the collected explorer commands:
      *
      *   SettingsChanged: sends a request to the server to persist user settings
      *                    and redisplays compounds
      *   QueryChanged: sends a new query for compounds to the server
      *   QueriesChanged: sends a request to the server to persist user settings
      *                   and adjusts the list of store queries
      *   ExportChanged: sends a request to the server to persist user settings
      *                  and another request to export the selected pieces of information
      *   Scrolled: displays additional compound if arrived close to the bottom
      *             of the compound table, possibly also sending a request to
      *             the server to return additional compounds
      *   ZoomedIn, ZoomedOut, and SizeChanged: adjusts the size of the explorer and the
      *                                         neighboring views, as well as the height
      *                                         of rows in the explorer
      *   ModeChanged: Adjusts the size of the explorer and redisplays compounds
      *   NumberOfColumnsChanged: Adjusts the size of the explorer and redisplays compounds
      *   Anything else: do nothing
      */
    private def handleCmd(e: Env)(c: ExpCmd): Eff[Unit] = c match {
      case SettingsChanged        ⇒ load(Load.Settings(e.settings,e.creds)) *> displayMols
      case QueryChanged           ⇒ loadFrom(0)
      case QueriesChanged         ⇒ load(Load.Settings(e.settings,e.creds)) *> adjQuerySel(e)
      case ExportChanged          ⇒ doExport(e)
      case Scrolled               ⇒ dispMore
      case ZoomedIn               ⇒ adjSize
      case ZoomedOut              ⇒ adjSize
      case ModeChanged            ⇒ adjSize *> displayMols
      case SizeChanged            ⇒ adjSize
      case NumberOfColumnsChanged ⇒ adjSize *> displayMols
      case _                      ⇒ Eff.unit
    }

    private def adjQuerySel(e: Env): Eff[Unit] = {
      val sel = T.select(id := QuerySelect, cls := WT.LoadQuerySel.c)(
                  T.options(querySels(e.settings, None))
                )

      at(QuerySelectCont)(set innerHtml sel) *>
      at(QuerySelect)(set.value(""))
    }

    /**
      * Displays a single substance in HTML format.
      */
    def dispSub(e: Env)(s: Sub): Node

    /**
      * Checks, whether a Result loaded from the server actually
      * contains a URL to a downloadable resource, in which case
      * this URL should be returned wrapped in a Some.
      */
    def exportRes(r: Result): Option[String]

    /**
      * Adjusts a combined query based on a selection of
      * substance IDs. This is used for exporting only the selected
      * set of compounds
      */
    def adjQuery(ids: List[I]): Q[Field] ⇒ Q[Field]

    private def doExport(e: Env): Eff[Unit] = for {
      st       <- getS
      settings =  if (e.expSt.settings.exportSelection)
                    lens[ExpEnv].expSt.query.query
                      .modify(e)(adjQuery(st.select.selected.toList))
                      .expSt
                      .exportSettings
                  else e.expSt.exportSettings
      _        <- load(Load.Export(settings, e.creds))
      _        <- load(Load.Settings(e.settings, e.creds))
    } yield ()

    private lazy val download = liftS((pth: String) ⇒ at(MainId)(
      for {
        c    <- Html liftE askE.map(_.creds)
        lnk  =  s"${url}/export/${pth}"
        _    <- debugH(s"about to download ${lnk}")
        link <- a(
                  href         := s"${lnk}${c.credsQ}",
                  tags.dhidden := true,
                  target       := "_blank"
                )
        _    <- Html delayedTry link.click()
      } yield unit
    ))

    private def displayMols: Eff[Unit] =
      at(SubTableId)(clear)                                             *>
      at(SubTableHeader)(clear)                                         *>
      askE.flatMap(e ⇒ at(SubTableHeader)(innerHtml(Txt.mkHeadRow(e)))) *>
      modS(stLens.displayed.set(_)(0))                                  *>
      appendMols

    private def appendMols: Eff[Unit] = for {
      qst <- getS
      e   <- askE

      ss  = subs(e.st) drop qst.displayed take e.expMode.dispCount
      _   <- modS(stLens.displayed.modify(_)(_ + ss.size))
      _   <- at(SubTableId)(withinH(ul(cls := Comp(CompType.SubTable)))(
               innerHtml(nodes(Txt.subItems[Sub](e, dispSub, mkId)(ss): _*))
             ))
      _   <- dispMore
    } yield ()

    private def dispMore: Eff[Unit] = for {
      qst      <- getS
      close    <- at(SubTableId)(closeToBottom)
      _        <- if (close && qst.displayed < qst.loaded) appendMols
                  else if (close && qst.loaded < qst.total && qst.readyToLoad)
                    loadFrom(qst.loaded)
                  else Eff.unit
    } yield ()

    private def adjSize: Eff[Unit] = askE >>= { e ⇒ Eff.delayedTry{
      val w = getElementById(SubTableId.id).clientWidth

      getElementById(ExplorerStyle.id).innerHTML = Txt.expStyle(e, w).toString
    }}

    /**
      * Checks, whether a Result loaded from the server actually
      * affects the set of substances loaded in the client.
      */
    def subRes(r: Result): Option[EditRes[Sub]]
    
    private def onLoad(r: EditRes[Sub], e: Env): Eff[Unit] =
      r match {
        case Added(s)      ⇒ modS(stLens.loaded.modify(_)(_ + 1))    *>
                             modS(stLens.total.modify(_)(_ + 1))     *>
                             displayMols
        case Updated(s)    ⇒ at(mkId(s))(innerHtml(dispSub(e)(s)))
        case Deleted(s)    ⇒ modS(stLens.loaded.modify(_)(_ - 1))    *>
                             modS(stLens.total.modify(_)(_ - 1))     *>
                             modS(stLens.displayed.modify(_)(_ - 1)) *>
                             at(mkId(s))(remove)
        case Found(ss,t,0) ⇒ modS(s ⇒ ControllerSt(true, ss.size, 0, t, s.select)) *>
                             displayMols
        case Found(ss,_,_) ⇒ modS(stLens.loaded.modify(_)(_ + ss.size)) *>
                             appendMols
      }
    
    private def loadNext(b: Boolean): Eff[Unit] = modS(stLens.readyToLoad.set(_)(b))

    import implicits.{colEncI, fldEncI}

    private def loadFrom(pos: Int): Eff[Unit] = for {
      _ <- loadNext(false)
      e <- askE
      _ <- load(Load.Query(e.expSt.query.copy(start = pos).asJson.noSpaces, e.creds))
    } yield ()

    private def closeToBottom: Html[Boolean] =
      withElem{ e: Elem ⇒ Eff delayed (
        (e.scrollTop + e.clientHeight) >= (e.scrollHeight - DistanceToBottom)
      ) }.map(_ getOrElse false)

    private lazy val stLens = lens[ControllerSt[I]]
    
    private def registerScroll: Eff[Unit] =
      at(SubTableId)(withElem_{ e: Elem ⇒ e.onscroll = e ⇒ { h(Scroll(e)).value } })
  }


  /**
    * Helper trait used to modify the actual explorer state (ExpSt)
    * based on the explorer's environment (ExplorerEnv) and
    * the actual UIEvent.
    */
  trait ExpUnit extends DomH[ExplorerEnv,ExpSt,Option[UIEvent]] {
  
    override def evToUIEvent(e: Ev) = e
  
    def run(s: Signal[Unit]): LogSF[(ExplorerIn,ExpSt),ExpSt] =
      msf.SF.loopBack(unrs(s).map{ case (_,s) ⇒ s -> s})
  
    /**
      * Core behavior of this explorer unit
      */
    def signal: Signal[Unit]
  
    lazy val stL  = lens[ExpSt]

    lazy val qL = lens[Query]
  
    lazy val getMode: Eff[ExpMode] = getS map (_.mode)
  
    lazy val getUserSettings: Eff[USettings] = getS map (_.settings)
  
    lazy val getFormats: Eff[Gradients] = getUserSettings map (_.doubleFormats)
  
    lazy val askCreds: Eff[Creds] = askE map (_._2)
  
    lazy val askSt: Eff[St] = askE map (_._1)
  
    lazy val creds: Signal[Creds] = constS(askCreds)

    lazy val queryName: Signal[Option[Name]] =
      Src.valueValidated(Read[Name].read, loc.msg("name"))(UId.QueryName)
  }

  /**
    * Controls basic behavior of the explorer view such
    * as zooming (for enlarging molecules), resizing
    * the explorer view, and changing the explorer mode (= how
    * information is being displayed).
    */
  trait BasicController extends ExpUnit {
  
    import Src._, UId.{Load ⇒ _, _}
  
    /**
      * Returns a combined query based on quick search information
      */
    def quickToQ(q: QuickSearch): Q[Field]

    /**
      * creates a combined query from a Path and a type of
      * navigator search
      */
    def navSearch(p: Path, m: NavSearchMode): Q[Field]

    /**
      * Signal function controlling the basic behavior of the
      * explorer
      */
    def signal: Signal[Unit] = (
      ef.const(ExpSt.reset)                                       |+|
      click(ExplorerZoomIn).as(ExpSt.zoomIn)                      |+|
      click(ExplorerZoomOut).as(ExpSt.zoomOut)                    |+|
      click(SubGridBtn).as(ExpSt mode SubstanceGrid)              |+|
      click(SubTableBtn).as(ExpSt mode SubstanceTable)            |+|
      click(SubStatsBtn).as(ExpSt mode MethodTable)               |+|
      click(EditPlotBtn).as(ExpSt mode PlotsView)                 |+|
      click(NavLeft).as(ExpSt.incHorizontal)                      |+|
      click(NavRight).as(ExpSt.decHorizontal)                     |+|
      click(QueryUp).as(ExpSt.decVertical)                        |+|
      click(QueryDown).as(ExpSt.incVertical)                      |+|
      navSearch                                                   |+|
      gridCols                                                    |+|
      quickSrc                                                    |+|
      scroll(SubTableId).as(ExpSt.scroll)                         |+|
      ui.collect{ case Click(ID(SortSub(Col(c)))) ⇒ ExpSt.sort(c) }
    ) >>- doMod
  
    private lazy val gridCols =
      change(GridColumns).asF(at(GridColumns)(get.select)).collectO(ExpSt.readCols)
  
    private lazy val onQuick: QuickSearch ⇒ (ExpSt ⇒ ExpSt) =
      q ⇒ ExpSt.quick(quickToQ(q), q)

    private lazy val navSearch: Src[ExpSt ⇒ ExpSt] = Src.ui.collect{
      case Click(ID(NavSearch(p,m))) ⇒ ExpSt clickquery navSearch(p,m)
    }
  
    private def toggleCls(id: UIdP, ifTrue: CyByClass, ifFalse: CyByClass):
      Boolean ⇒ Eff[Boolean] = b ⇒
        if (b) at(id)(set.attribute(cls := ifTrue)) as b
        else at(id)(set.attribute(cls := ifFalse)) as b
  
    private lazy val toggleEmpty =
      toggleCls(QuickSearchEmpty, IT.QuickIncludeEmpty.c, IT.QuickExcludeEmpty.c)
  
    private lazy val toggleCase =
      toggleCls(QuickSearchCase, IT.QuickCaseSensitive.c, IT.QuickCaseInsensitive.c)
  
    private def includeEmpty(b: Option[Boolean]) =
      click(QuickSearchEmpty)
        .flip(false,true)
        .hold(b getOrElse false)
        .mapF(toggleEmpty)
  
    private def caseSensitive(b: Option[Boolean]) =
      click(QuickSearchCase)
        .flip(false,true)
        .hold(b getOrElse false)
        .mapF(toggleCase)
  
    private def quickEnter(b: Option[String]) =
      enter(QuickSearchTxt).asF(at(QuickSearchTxt)(get.value))
  
    private def quickSrc: Src[ExpSt ⇒ ExpSt] =
      msf.EF.joinF(getS.map(_.settings.quickO).flatMap { q ⇒
        at(QuickSearchTxt)(set value q.map(_.query).getOrElse("")) as
        quickSrcE(q)
      })

    private def quickSrcE(q: Option[QuickSearch]): Src[ExpSt ⇒ ExpSt] = (
      quickEnter(q map (_.query)).sf,
      includeEmpty(q map (_.includeEmpty)),
      caseSensitive(q map (_.caseSensitive)),
    ).mapN((_,_,_))
     .collect{ case (Some(txt),i,c) ⇒ onQuick(QuickSearch(i,c,txt)) }
  }

  
  /**
    * Helper trait for interactively editing trees of
    * combined queries.
    */
  trait QueryEd extends ExpUnit {
  
    import implicits.fldEqI

    /**
      * UI element(s) for selecting available fields
      * for querying.
      */
    def fieldDesc(st: St): WidgetDesc[Unit,Field,Field]

    /**
      * Returns for every queryable field appropriate
      * UI elements for editing queries of the field.
      */
    def query(f: Field, st: St): WidgetDesc[Unit,String,String]
  
    import UId.{Load ⇒ _, _}

    /**
      * As queries typically can only be properly displayed when
      * all necessary data has been loaded from the server, we
      * need a way for signalling, when this is the case.
      */
    def loaded: Signal[Boolean]
  
    /**
      * signal for editing and running combined queries.
      *
      * The return type is Unit because queries and load commands
      * are part of the signal functions State and Writer monad.
      */
    lazy val signal: Signal[Unit] = {
      type Pair = Option[(Int,Q[Field])]

      def sig(p: Pair) = msf.SF.joinF(at(QueryView)(clear *> subs(p))).toEF

      val settings = getS map (_.settings)

      val lastQuery = settings map (_.queryO map (0 -> _))

      def getPair(s: String): Eff[Pair] = settings map {
        ss ⇒ s match {
          case "" ⇒ ss.queries.headOption map (0 -> _)
          case s  ⇒ Read[Int] read s flatMap (i ⇒ ss.queries.lift(i) map (i -> _))
        }
      }

      val loaded_ : Src[Pair] = 
        loaded.scan(false -> false){ case ((_,n),v) ⇒ n -> v }
              .collectF{ case (false,true) ⇒ lastQuery}

      val querySig = Src.enter(QuerySelect)
                        .asF(at(QuerySelect)(get.select))
                        .mapF(getPair)
                        .collectO(identity)
                        .map(some) <|> loaded_
  
      ef.switch(querySig map sig).holdEmpty
    }
  
    private def subs(q: Option[(Int,Q[Field])]): Html[Signal[Unit]] = for {
      s   <- Html liftE getS
      sel = selectEntries(querySels(s.settings, q map (_._1)))
      l   <- li(
               cls := CT.QuerySubRow.c,
               button(id := RunQuery, cls := WT.RunQueryBtn.c, text(loc.runQuery)),
             )
      u   <- ul(cls := CT.QueryList.c)
      res <- within(u)(block(l, q collect { case (_,Chain(ps)) ⇒ ps}))
      _   <- within(l){
               div(cls := CT.QueryFill.c) *>
               div(
                 id := QuerySelectCont,
                 cls := CT.QuerySelectContainer.c,
                 select(id := QuerySelect, cls := WT.LoadQuerySel.c, sel)
               ) *>
               input(IPT.Text, id := QueryName, cls := WT.QueryName.c) *>
               button(id := SaveQuery, cls := WT.SaveQueryBtn.c, text(loc.saveQuery)) *>
               button(id := DeleteQuery, cls := WT.DeleteQueryBtn.c, text(loc.deleteQuery))
             }

    } yield (res.map2(Chain(_)) >>* liftS(onEdit))
              .onWith(runQuery)(queryEvent) >>- doMod

    private def onEdit(o: Option[Q[Field]]): Eff[Unit] =
      at(RunQuery)(set attribute (title := o.toString))

    private def queryEvent(o: Option[Q[Field]], c: QueryCmd): ExpSt ⇒ ExpSt = (o,c) match {
        case (Some(q),RunQueryCmd)       ⇒ ExpSt query q
        case (Some(q),SaveQueryCmd(n))   ⇒ ExpSt.storeQuery(q, n)
        case (_,      DeleteQueryCmd(s)) ⇒ ExpSt deleteQuery s
        case _                           ⇒ identity
      }

    private lazy val runQuery: Src[QueryCmd] =
      (Src.click(RunQuery) |+| queryEnter).as(RunQueryCmd.c)      <|>
      Src.click(UId.DeleteQuery)
         .asF(at(QuerySelect)(get.select))
         .mapF(s ⇒ debug(s"About to delete query $s").as(DeleteQueryCmd(s).c)) <|>
      queryName.on(Src.click(SaveQuery) |+| Src.enter(QueryName))
               .collect{ case Some(n) ⇒ SaveQueryCmd(n)}
  
    private lazy val queryEnter =
      Src.ui.collect{  case KeyPress(x) if x.key === "Enter" ⇒ x }
            .collectF{ case ID(x)     ⇒ at(x)(get.ids) }
            .filter(_ contains QueryView)
            .void
  

    private type ChainPair = (cyby.query.Comp,Chain[Field])

    private type PrimPair = (cyby.query.Comp,Prim[Field])

    private type QPair = (cyby.query.Comp,Q[Field])

    private def block(l: Elem, qs: Option[List[QPair]]): Html[Signal[Option[List[QPair]]]] =
      dynlistA(
        (within(l)(iconAdd), within(l)(iconParens)).mapN(
          (add,par) ⇒ add.as(primQ.swapped) <|> par.as(chainQ.swapped)
        ),
        qs getOrElse Nil traverse editQ
      )

    private def editQ(q: QPair): Html[(Signal[Option[QPair]],Action)] = q match {
      case p@(_,Prim(_,_,_)) ⇒ primQ  pairO some(p) map (_.swap)
      case p@(_,Chain(_))    ⇒ chainQ pairO some(p) map (_.swap)
    }

    private lazy val primQ: WidgetDesc[Action,QPair,QPair] =
      prim map (p ⇒ p : QPair) cmapP { case (c,p@Prim(_,_,_)) ⇒ c -> p }

    private lazy val chainQ: WidgetDesc[Action,QPair,QPair] = 
      chain map (p ⇒ p : QPair) cmapP { case (c,p@Chain(_)) ⇒ c -> p }

    private lazy val chain: WidgetDesc[Action,ChainPair,ChainPair] = WidgetDesc(i ⇒ for {
      l     <- li(cls := CT.ParensRow.c)
      u     <- ul(cls := CT.QueryParen.c)
      m     <- li(cls := CT.ParensRow.c, span(cls := CT.QueryParenTxt.c, text(")")))
  
      cs    <- within(l)(
                 comparatorQ(i map (_._1)) <*
                 span(cls := CT.QueryParenTxt.c, text("("))
               )
      items <- within(u)(block(l, i map (_._2.qs)))
      del   <- within(l)(iconDelete)
  
      s     = (cs,items).mapN2((c,qs) ⇒ c -> Chain(qs))
    } yield del.asF(at(l)(remove) *> at(u)(remove) *> at(m)(remove)) -> s)

    private lazy val prim: WidgetDesc[Action,PrimPair,PrimPair] = WidgetDesc(i ⇒ for {
      l      <- li(cls := CT.QueryRow.c)
      st     <- Html liftE askSt
      cmp    <- within(l)(comparatorQ(i map (_._1)))
      neg    <- within(l)(negator.query.desc.signalO(i map (_._2.negate)))
      fldP   <- within(l)(
                  fieldDesc(st).switch[String,(Field,String)](
                    f ⇒ o ⇒ if (i.exists(_._2.lbl === f))
                              query(f, st).signalO(o).map3(f -> _)
                            else query(f, st).signalO(none).map3(f -> _),
                    _ ⇒ i.map(_._2.query),
                    CompType.QueryDetails.c,
                  ).signalO(i map (_._2.lbl))
                )
      del    <- within(l)(icon(IT.DeleteQuery))
    } yield del.asF(at(l)(remove)) ->
            (cmp,neg,fldP).mapN2((c,n,p) ⇒ c -> Prim(p._1, p._2, n)))
  }


  /**
    * Helper trait for controlling the list of exported
    * fields and the file type to use
    */
  trait Export extends ExpUnit {
    import UId._
  
    /**
      * UI element(s) for selecting available fields
      * for exporting.
      */
    def fieldDesc(st: St): WidgetDesc[Select,Field,Field]
  
    def signal: Signal[Unit] = const(unit).switch(
      Src click EditExportBtn asF at(ExportId)(clear *> exportSettings)
    )
  
    private def entry(st: St): WidgetDesc[Elem,List[Field],List[Field]] =
      fieldDesc(st).list(CT.ListEditContainer, CT.ExportFieldRow)

    /**
      * Creates the title of the export view
      */
    private def exportTitle: Html[Elem] =
      li(cls := CT.NavRow.c,
        h1(cls := TT.NavSection.c, text(loc.exportSettings)),
        div(cls := CT.ExplorerTitelFill.c),
        iconInfo(UId.ExportDoc),
      )
  
    private def exportSettings: Html[Signal[Unit]] = for {
      st     <- Html liftE askSt
      expSt  <- Html liftE getS
      _      <- exportTitle
      format <- Li(
                  cls := CT.NavEditRow(UserSettings.exportFormat).c,
                  lbl(UserSettings.exportFormat)
                ){ exportFormat.desc.signalO(expSt.settings.exportFormat) }
      select <- Li(
                  cls := CT.NavEditRow(UserSettings.exportSelection).c,
                  lbl(UserSettings.exportSelection)
                ){ checkBox(CT.CheckBoxContainer)
                     .desc.signal(expSt.settings.exportSelection) }
      fields <- Li(
                  cls := CT.NavEditRow(UserSettings.exportFields).c,
                  lbl(UserSettings.exportFields)
                ){ entry(st).signal(expSt.settings.exportFields) }
      sig    = (format,select,fields).mapN2((_,_,_))
      src    <- withConfirmButtons(sig)
    } yield src.effect(at(ExportId)(clear))
               .collect{ case Some(p) ⇒ ExpSt export p} >>- doMod
  }

  /**
    * Helper trait for controlling and arranging the set
    * of visible, properly formatted columns in the explorer.
    */
  trait ColumnControl extends ExpUnit {
  
    /**
      * Description of a select element (drow down menu)
      * with column entries to choose from.
      */
    def column(st: St, m: ExpMode): WidgetDesc[Select,Column,Column]
  
    import Src.ui

    /**
      * Signal function used to react on clicks for
      * displaying, editing, and rearranging columns.
      */
    def signal: Signal[Unit] = const(unit).switch(ui.collectF{
      case Click(ID(UId.DelColumnId(i)))       ⇒ Eff pure delCol(i)
      case DblClick(ID(UId.EditableColumn(i))) ⇒ editCol(i)
      case Click(ID(UId.AddColumnId(i)))       ⇒ addCol(i)
      case Click(ID(UId.EditFormatId(Col(c)))) ⇒ editFormat(c)
    })

    private def delCol(i: Int): Signal[Unit] =
      ef.const(ExpSt delColumn i).head >>- doMod

    private def addCol(i: Int): Eff[Signal[Unit]] =
      colSrc(None).map(_.collectO(_ map ExpSt.addColumn(i)) >>- doMod)

    private def editCol(i: Int): Eff[Signal[Unit]] =
      colSrc(some(i)).map(_.collectO(_ map ExpSt.replaceColumn(i)) >>- doMod)

    /**
      * Creates the title of the column editing view
      */
    private def columnsTitle: Html[Elem] =
      li(cls := CT.NavRow.c,
        h1(cls := TT.NavSection.c, text(loc.addColumn)),
        div(cls := CT.ExplorerTitelFill.c),
        iconInfo(UId.ColumnsDoc)
      )

    private def colSrc(index: Option[Int]): Eff[Src[Option[Column]]] = for {
      st       <- askSt
      s        <- getS
      ini      =  index flatMap (s.columns.splitAt(_)._2.headOption)
      src      <- at(UId.Columns)(
                    clear *>
                    columnsTitle *>
                    Li(cls := CT.NavRow.c)(column(st, s.mode).signalO(ini))
                      .flatMap(withConfirmButtons)
                  )
    } yield src.head.effect(at(UId.Columns)(clear))

    private def formatTitle(st: St, c: Column): Html[Elem] =
      li(
        cls := CT.NavRow.c,
        h1(cls := TT.NavSection.c, text(loc.formatHeader(c,st))),
      )
     
    private def editFormat(c: Column): Eff[Signal[Unit]] = for {
      st     <- askSt
      grads  <- getFormats
      src    <- at(UId.Columns)(
                  clear             *>
                  formatTitle(st,c) *>
                  gradient(st, c, getGrad(grads, c)).map3(g ⇒ Map(c -> g)) >>=
                  withConfirmButtons
                )
    } yield src.effect(at(UId.Columns)(clear))
               .collect{ case Some(fs) ⇒ ExpSt formats fs } >>- doMod

    private def getGrad(gs: Gradients, c: Column): Gradient[Double] =
      gs.get(c).collect{ case g@Gradient(_,_) ⇒ g}
        .getOrElse(Gradient[Double](Nil,0))
  }
}

// vim: set ts=2 sw=2 et:
