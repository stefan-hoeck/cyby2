/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui
package example

import cats.implicits.{none ⇒ _, _}

import cyby.dat.{Updated, Found}
import cyby.dat.example.{ExportField ⇒ ExF, _}
import cyby.ui.{WidgetType ⇒ WT, CompType ⇒ CT}

trait CoreZ extends CoreEnv with DomEnv with CyByZ {
  // ---------------------------------------------------------------
  //                      Core Functionality
  // ---------------------------------------------------------------

  object Accum extends Accumulator {
    def ini = St.ini
  
    val l = lens[St]
  
    def accumS(s: St, r: Option[Result]): St = r match {
      case Some(SubRes(r)) ⇒ l.subs.modify(s)(modRoot(r)(_.id))
      case Some(ProRes(r)) ⇒ l.pros.modify(s)(modRoot(r)(_.id))
      case Some(StoRes(r)) ⇒ l.stos.modify(s)(modRoot(r)(_.id))
      case Some(SupRes(r)) ⇒ l.sups.modify(s)(modRoot(r)(_.id))
      case Some(MetRes(r)) ⇒ l.mets.modify(s)(modRoot(r)(_.id))
      case Some(UseRes(r)) ⇒ l.uses.modify(s)(modRoot(r)(_.id))
      case Some(BioStatsRes(r)) ⇒ l.bio.modify(s)(modRoot(r)(_.con.id.to))
      case _               ⇒ s
    }
  }

  object AuthZ extends DoAuth {
    def resultIsLogin(r: Result) = r match {
      case LoggedIn(h,u,ss) ⇒ some((u,h,ss))
      case _                ⇒ none
    }
  
    def resultIsLogout(r: Result) = r match {
      case LoggedOut ⇒ true
      case _         ⇒ false
    }
  
    def resultIsUserSettingsChanged(r: Result) = r match {
      case cyby.dat.example.SettingsChanged(ss) ⇒ some(ss)
      case _                                 ⇒ none
    }
  
    def resultToUsers(r: Result) = r match {
      case UseRes(Updated(u))    ⇒ List(u)
      case UseRes(Found(us,_,_)) ⇒ us
      case _                     ⇒ Nil
    }
  }

  object LoaderZ extends Loader {
    def loadAll(load: DataType ⇒ Eff[Unit]): Eff[Unit] =
      List(UseT, SupT, ProT, StoT, MetT).traverse_(load)
  }
  
  object DecoderZ extends Decoder


  //----------------------------------------------------------------------
  //                      Dom Manipulation
  //----------------------------------------------------------------------

  trait DomZ[E,S,I] extends DomH[E,S,I] {
    def proDef(p: Option[Project.Cli])(st: St): SelectDesc[Project.Cli] =
      pro(st).copy(default = p)

    def pro(st: St): SelectDesc[Project.Cli] = link(st.pros)(_.name.v.v)
  
    def sup(st: St): SelectDesc[Sup.Cli] = link(st.sups)(_.name.v.v)
  
    def use(st: St): SelectDesc[Use.Cli] = link(st.uses)(_.alias.v.v)
   
    def met(st: St): SelectDesc[Met.Cli] = link(st.mets)(_.name.v.v)
   
    def sto(st: St): SelectDesc[Sto.Cli] = link(st.stos)(_.name.v.v)

    // ---------------------------------------------------------------
    // Query
    // ---------------------------------------------------------------
    
    def qlink[A,El,P](p: String ⇒ Option[P], desc: St ⇒ WidgetDesc[El,A,A])(
      find: St ⇒ P ⇒ Option[A],
      show: P ⇒ String,
      get:  A ⇒ P,
    ): St ⇒ WidgetDesc[Unit,String,String] = st ⇒
      desc(st).cmapO[String](p(_) flatMap find(st))
              .map(get andThen show)
              .mapEl(_ ⇒ unit)

    lazy val qpro: St ⇒ WidgetDesc[Unit,String,String] =
      qlink(Read[Project.AccId].read, pro(_).query.desc)(fpro, _.toString, _.id)

    lazy val qsup: St ⇒ WidgetDesc[Unit,String,String] =
      qlink(Read[Sup.Id].read, sup(_).query.desc)(fsup, _.toString, _.id)

    lazy val quse: St ⇒ WidgetDesc[Unit,String,String] =
      qlink(Read[Use.Id].read, use(_).query.desc)(fuse, _.toString, _.id.to)

    lazy val qsto: St ⇒ WidgetDesc[Unit,String,String] =
      qlink(Read[Sto.Id].read, sto(_).query.desc)(fsto, _.toString, _.id)

    lazy val qmet: St ⇒ WidgetDesc[Unit,String,String] =
      qlink(Read[Met.Id].read, met(_).query.desc)(fmet, _.toString, _.id)

  
  
    // ---------------------------------------------------------------
    // Export
    // ---------------------------------------------------------------
    
    def subField(p: SubField ⇒ Boolean): SelectDesc[SubField] =
      selDescEnumP(p)(WT.Field2Sel, _ locName loc)
  
    def conField(p: ConField ⇒ Boolean): SelectDesc[ConField] =
      selDescEnumP(p)(WT.Field2Sel, _ locName loc)
  
    def bioField(p: BioField ⇒ Boolean): SelectDesc[BioField] =
      selDescEnumP(p)(WT.Field2Sel, _ locName loc)
  
    def dataType(p: DataType ⇒ Boolean): SelectDesc[DataType] =
      selDescEnumP(p)(WT.FieldSel, _ locName loc)
  
    def expDescP(
      st:    St,
      expP:  DataType ⇒ Boolean,
      subP:  SubField ⇒ Boolean,
      conP:  ConField ⇒ Boolean,
      bioP:  BioField ⇒ Boolean,
      wtMod: WT ⇒ WT,
      comp:  CT,// = CT.ExportDetailRow,
    ): WidgetDesc[Select,ExF,ExF] = {
      val mkSig = (ef: DataType) ⇒ (o: Option[ExF]) ⇒ ef match {
        case SubT   ⇒ subField(subP).widgetMod(wtMod).desc.map(ExportSub(_).f)
                        .signalO(o collect { case ExportSub(f) ⇒ f})
  
        case ConT   ⇒ conField(conP).widgetMod(wtMod).desc.map(ExportCon(_).f)
                        .signalO(o collect { case ExportCon(f) ⇒ f})
  
        case BioT   ⇒ bioField(bioP).widgetMod(wtMod).desc.map(ExportBio(_).f)
                        .signalO(o collect { case ExportBio(f) ⇒ f})
  
        case StatsT ⇒ exportStats(st, wtMod).map(_.f)
                          .signalO(o collect { case s@ExportStats(_,_) ⇒ s})

        case _      ⇒ Html.pure(const(none[ExF]) : Signal[Option[ExF]])
      }
  
      dataType(expP).widgetMod(wtMod).desc.cmap[ExF](_.tag)
        .switch[ExF,ExF](mkSig, some, comp.c)
  
    }

    def expDesc(st: St): WidgetDesc[Select,ExF,ExF] = expDescP(st, _.canExport,
      _.canExport, _.canExport, _.canExport, WT.Export, CT.ExportDetailRow)

    def queryDesc(st: St): WidgetDesc[Select,ExF,ExF] = expDescP(st, _.canQuery,
      _.canQuery, _.canQuery, _.canQuery, WT.Query, CT.QueryDetailRow)

    def column(st: St, mode: ExpMode): WidgetDesc[Select,ExF,ExF] =
      mode match {
        case MethodTable ⇒ expDescP(st, _.inStatsTable, _.inColumn,
          _.inColumn, _.inColumn, WT.Export, CT.ExportDetailRow)
        case _ ⇒ expDescP(st, _.inSubTable, _.inColumn, _.inColumn,
          _.inColumn, WT.Export, CT.ExportDetailRow)
      }
  
    def exportStats(st: St, wtMod: WT ⇒ WT): WidgetDesc[Elem,ExportStats,ExportStats] =
      WidgetDesc[Elem,ExportStats,ExportStats](o ⇒ 
        for {
          m <- met(st).widgetMod(wtMod).desc.cmapO(fmet(st)).signalO(o map (_.mid))
          s <- statsType.widgetMod(wtMod).pairO(o map (_.stat))
        } yield s._1 -> (m,s._2).mapN2((m,s) ⇒ ExportStats(m.id, s))
      )
  }
}
