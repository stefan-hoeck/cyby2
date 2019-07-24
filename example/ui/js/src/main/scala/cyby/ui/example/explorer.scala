/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui
package example

import cats.implicits.{none ⇒ _, _}

import cyby.dat.example._
import cyby.query.{Q, Chain, Prim, ReadPred ⇒ RP, QuickSearch}
import cyby.query.Comp.And

import msf.writer.unWriter
import msf.js.{Handler, UIEvent, HttpRequest}

case class ExplorerZ(
  hht:   Handler[HttpRequest],
  hui:   Handler[UIEvent],
  url: String
) extends CoreZ
  with EditZ
  with DispZ
  with cyby.ui.explorer.Explorer {

  outer ⇒

  def navSearchImpl(p: Path, m: NavSearchMode): Q[Field] = (p,m) match {
    case (ProP(p),NSSub)         ⇒ prim(SubProject.ef, p.head.toString)
    case (ProP(p),NSConAll)      ⇒ prim(ConProject.ef, p.head.toString)
    case (ProP(p),NSConNonEmpty) ⇒ nonEmpty(ConProject.ef, p.head.toString)
    case (ProP(p),NSBio)         ⇒ prim(BioProject.ef, p.head.toString)
    case (MetP(p),NSBio)         ⇒ prim(BioMethod.ef, p.head.toString)
    case (SupP(p),NSConAll)      ⇒ prim(ConSupplier.ef, p.head.toString)
    case (SupP(p),NSConNonEmpty) ⇒ nonEmpty(ConSupplier.ef, p.head.toString)
    case (SupP(p),NSBio)         ⇒ prim(BioSupplier.ef, p.head.toString)
    case (StoP(p),NSConAll)      ⇒ prim(ConLocation.ef, p.head.toString)
    case (StoP(p),NSConNonEmpty) ⇒ nonEmpty(ConLocation.ef, p.head.toString)
    case _                       ⇒ Chain(Nil)
  }

  def prim(f: Field, s: String): Q[Field] = Prim(f, s, false)

  def nonEmpty(f: Field, s: String): Q[Field] = Chain(List(
    And -> Prim(f,s,false),
    And -> Prim(ConEmpty.ef,"false",false),
  ))



  //----------------------------------------------------------------------
  //                      Main Signal Function
  //----------------------------------------------------------------------

  object sfh extends CyByHelper {
    lazy val bc: BasicController = new BasicController {
      def quickToQ(q: QuickSearch) = quick(q)
      def navSearch(p: Path, m: NavSearchMode) = navSearchImpl(p,m)
    }

    lazy val authSF: SF[In,AuthOut] = idS[In] >>> (
      (DecoderZ.run --< AuthZ.run) &&& arr(_.toOption)
    )

    lazy val login: SF[AuthOut,Unit] = toLoginIn >>> Login.run

    lazy val expUnits = Que.run(
      bc.signal       |+|
      ExportZ.signal  |+|
      Que.signal      |+|
      Format.signal
    )

    lazy val expSF: SF[ExpIn,Unit] =
      ExpZ.run switch idS[ExpIn].collect {
        case (_,e) if e.expSt.changedToMethodView   ⇒ BioZ.run
        case (_,e) if e.expSt.changedToExplorerView ⇒ ExpZ.run
      }

    val mainSF: SF[MainIn,Unit] = (
      idS[MainIn]                         --< 
      (toExpandIn  >>> Expander.run)      --< 
      (toAccumIn   >>> Accum.run)         >>*
      (toDispIn    >>> Disp.run)          >>*
      (toEditIn    >>> Edit.run)
    ) --< (toExplorerIn >>> expUnits)     >>> (toExpIn >>> expSF)

    val mainSF2: SF[AuthOut,Unit] = idS[AuthOut].collect(toMainIn) >>- mainSF

    val outerSF = (
      authSF >>> login.switch(
        idS[AuthOut].collect{
          case ((_,Some(cyby.dat.example.LoggedIn(_,_,_))), _) ⇒ mainSF2
          case ((_,Some(cyby.dat.example.LoggedOut)), _)       ⇒ login
        }
      )
    ).zipWith(idS[In]){ (_,e) ⇒ e.toOption }
  }

  val behavior: msf.SF[IO,In,Unit] =
    unWriter(sfh.outerSF) >>> (LoaderZ.run(hht, url, 20000) |+| Logger.log)


  //----------------------------------------------------------------------
  //                      Main Signal Function
  //----------------------------------------------------------------------

  object ExpZ extends Controller[Sub.Cli,Sub.Id](_.subs, hui, url) {

    private def idsQ(ids: List[Sub.Id]): ZQ =
      Prim(ExportSub(SubId).f, ids mkString " ", false)

    def adjQuery(ids: List[Sub.Id]) = {
      case Chain(Nil) ⇒ idsQ(ids)
      case q          ⇒ Chain(List(And.c -> idsQ(ids), And.c -> q))
    }

    def read(s: String) =
      if (s startsWith SubStr) Read[Sub.Id] read s.drop(SubStrL)
      else None

    def toIdString(s: Sub.Id) = subIdString(s)

    def getId(s: Sub.Cli) = s.id

    override def dispSub(e: Env)(s: Sub.Cli): String = outer.dispSub(e)(s)

    def exportRes(r: Result): Option[String] = r match {
      case ExportRes(p) ⇒ some(p)
      case _            ⇒ None
    }

    //----------------------------------------------------------------------
    //                      Substances
    //----------------------------------------------------------------------

    override def subRes(r: Result) = r match {
      case SubRes(r) ⇒ some(r)
      case _         ⇒ None
    }
  }

  trait ExpDom extends DomZ[ExplorerEnv,ExpSt,Option[UIEvent]]

  object ExportZ extends Export with ExpDom {
    def readField(s: String) = Read[ExportField] read s
  
    def fieldDesc(st: St) = expDesc(st)
  }

  object Que extends QueryEd with ExpDom {
    def fieldDesc(st: St) = queryDesc(st).mapEl(_ ⇒ unit)

    lazy val loaded =
      env.map(_._3).scan(loadInit)(adjLoadState).map(_.loaded)
      
    def query(f: ExportField, st: St) = f match {
      case ExportSub(s)       ⇒ qsub(s, st)
      case ExportCon(s)       ⇒ qcon(s, st)
      case ExportBio(s)       ⇒ qbio(s, st)
      case ExportStats(_,_)   ⇒ qstats
    }

    def qsub(f: SubField, st: St): WidgetDesc[Unit,String,String] = f match {
      case SubId           ⇒ txtQ(RP.id_[Sub.type])
      case SubName         ⇒ stringQ
      case SubCasNr        ⇒ stringQ
      case SubProject      ⇒ qpro(st)
      case SubAbs          ⇒ boolQ
      case SubCreated      ⇒ dateQ
      case SubMol(mf)      ⇒ molQ(mf)
      case SubEditInfo(mf) ⇒ editQ(mf, quse(st))
      case SubContainers   ⇒ noQ
      case SubFil(ff)       ⇒ qfil(ff, st)
    }

    def qcon(f: ConField, st: St): WidgetDesc[Unit,String,String] = f match {
      case ConId            ⇒ txtQ(RP.id_[Con.type])
      case ConAmount        ⇒ txtQ(RP.double_)
      case ConPurity        ⇒ txtQ(RP.double_)
      case ConPurityStr     ⇒ stringQ
      case ConBatch         ⇒ stringQ
      case ConComment       ⇒ stringQ
      case ConConcentration ⇒ txtQ(RP.double_)
      case ConDensity       ⇒ txtQ(RP.double_)
      case ConEmpty         ⇒ boolQ
      case ConLentTo        ⇒ stringQ
      case ConLocation      ⇒ qsto(st)
      case ConOrderNr       ⇒ stringQ
      case ConSupplier      ⇒ qsup(st)
      case ConProject       ⇒ qpro(st)
      case ConCreated       ⇒ dateQ
      case ConEditInfo(mf)  ⇒ editQ(mf, quse(st))
      case ConFil(ff)       ⇒ qfil(ff, st)
    }

    def qbio(f: BioField, st: St): WidgetDesc[Unit,String,String] = f match {
      case BioId           ⇒ txtQ(RP.id_[Bio.type])
      case BioValue        ⇒ txtQ(RP.double_)
      case BioMethod       ⇒ qmet(st)
      case BioSupplier     ⇒ qsup(st)
      case BioDate         ⇒ dateQ
      case BioComment      ⇒ stringQ
      case BioProject      ⇒ qpro(st)
      case BioCreated      ⇒ dateQ
      case BioEditInfo(mf) ⇒ editQ(mf, quse(st))
      case BioFil(ff)      ⇒ qfil(ff, st)
    }

    def qfil(f: FilField, st: St): WidgetDesc[Unit,String,String] = f match {
      case FilId           ⇒ txtQ(RP.id_[Bio.type])
      case FilName         ⇒ stringQ
      case FilPath         ⇒ stringQ
      case FilComment      ⇒ stringQ
      case FilProject      ⇒ qpro(st)
      case FilCreated      ⇒ dateQ
      case FilEditInfo(mf) ⇒ editQ(mf, quse(st))
    }

    lazy val qstats: WidgetDesc[Unit,String,String] = txtQ(RP.double_)
  }

  object Format extends ColumnControl with ExpDom

  object BioZ extends Controller[BioStats,(Con.Id,Sub.Id)](_.bio, hui, url) {

    private val BioStats = "biostats"
    private val BioStatsL = BioStats.length
  
    def adjQuery(ids: List[(Con.Id,Sub.Id)]) = q ⇒ q
  
    def read(s: String) =
      if (s startsWith BioStats) s drop BioStatsL split "_" toList match {
        case a::b::Nil ⇒ (Read[Con.Id] read a, Read[Sub.Id] read b).mapN((_,_))
        case _         ⇒ None
      }
      else None
  
    def toIdString(p: (Con.Id,Sub.Id)) = statsIdString(p)
  
    def getId(s: BioStats) = statsId(s)

    override def subRes(r: Result) = r match {
      case BioStatsRes(r) ⇒ some(r)
      case _              ⇒ None
    }
  
    override def dispSub(e: Env)(s: BioStats): String = dispStats(e)(s)
  
    def exportRes(r: Result): Option[String] = r match {
      case ExportRes(p) ⇒ some(p)
      case _            ⇒ None
    }

    def metCell(c: ExportStats, expSt: ExpSt)(s: BioStats): String =
      Txt.statsCell(c, c.stat, s.stats get c.mid, expSt)
  }
}

// vim: set ts=2 sw=2 et:
