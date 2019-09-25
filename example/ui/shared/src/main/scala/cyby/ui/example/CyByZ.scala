/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui
package example

import cats.implicits.{none ⇒ _, _}

import cyby.dat.{Mol, Found}
import cyby.dat.example._

trait CyByZ extends TextEnv with ZEnv {
  type St       = cyby.ui.example.St

  def useId(i: Use.AccId): Use.Id = i.to
  def proId(i: Project.AccId): Project.Id = i.to

  override def columnDesc(c: Column)       = c.desc
  override def columnPath(c: Column)       = c.toString
  override def molFieldToColumn(m: Mol.Field) = ExportCpd(CpdMol(m))
  override def structCol                      = molFieldToColumn(Mol.Structure)
  override def defaultSortCol                 = ExportCpd(CpdId)

  def columnLoc(st: St)(c: Column)   = c match {
    case ExportCpd(f)         ⇒ f locName loc
    case ExportCon(f)         ⇒ f locName loc
    case ExportBio(f)         ⇒ f locName loc
    case x@ExportStats(i,s)   ⇒ fmet(st)(i).fold(i.toString){
      m ⇒ s"${m.name} (${s locName loc})"
    }
  }

  val SubStr = "sub"
  val SubStrL = SubStr.length
  def subIdString(s: Compound.Id): String = s"${SubStr}${s}"
  
  def statsIdString(p: (Container.Id,Compound.Id)): String = s"${BioStats}${p._1}_${p._2}"
  def statsId(s: BioStats) = s.path.head -> s.path.tail.head

  //----------------------------------------------------------------------
  //                      Localization
  //----------------------------------------------------------------------

  object Txt extends TextH

  val loc = EnUS

  trait Loc extends LocUtil with cyby.dat.example.Loc {
    def xaxis: String
    def yaxis: String
    def colorBy: String
    def makePlot: String
  }
  
  object EnUS extends Loc with EnUSUtil with cyby.dat.example.LocEnUS{
    def logResult(r: Result): Vector[Log] = logRes(r)
    def xaxis = "x-Axis"
    def yaxis = "y-Axis"
    def colorBy = "Color"
    def makePlot = "Make Plot"
  }

  def modifiedSym = Use.modified
  def createdSym = Use.created

  //----------------------------------------------------------------------
  //                      Data Access
  //----------------------------------------------------------------------

  lazy val fbio: St ⇒ BiodataEntry.Path ⇒ Option[(BiodataEntry.Cli,Container.Cli,Compound.Cli)] = s ⇒ p ⇒ for {
    (c,s) <- fcon(s)(p.tail)
    b     <- c.bio find (_.id === p.head)
  } yield (b,c,s)

  lazy val fbioFil: St ⇒ BiodataEntry.FilPath ⇒ Option[(Fil.Cli,BiodataEntry.Cli,Container.Cli,Compound.Cli)] = s ⇒ p ⇒ for {
    (b,c,s) <- fbio(s)(p.tail)
    f       <- b.files find (_.id === p.head)
  } yield (f,b,c,s)

  lazy val fcon: St ⇒ Container.Path ⇒ Option[(Container.Cli,Compound.Cli)] = s ⇒ p ⇒ for {
    s <- fsub(s)(p.tail.head)
    c <- s.containers find (_.id === p.head)
  } yield c -> s

  lazy val fconFil: St ⇒ Container.FilPath ⇒ Option[(Fil.Cli,Container.Cli,Compound.Cli)] = s ⇒ p ⇒ for {
    (c,s) <- fcon(s)(p.tail)
    f     <- c.files find (_.id === p.head)
  } yield (f,c,s)

  lazy val fsubFil: St ⇒ Compound.FilPath ⇒ Option[(Fil.Cli,Compound.Cli)] = s ⇒ p ⇒ for {
    s <- fsub(s)(p.tail.head)
    f <- s.files find (_.id === p.head)
  } yield (f,s)

  lazy val fpro: St ⇒ Project.AccId ⇒ Option[Project.Cli] = s ⇒ i ⇒ s.pros find (_.id === i)

  lazy val fsub: St ⇒ Compound.Id ⇒ Option[Compound.Cli] = s ⇒ i ⇒ s.subs find (_.id === i)

  lazy val fsup: St ⇒ Sup.Id ⇒ Option[Sup.Cli] = s ⇒ i ⇒ s.sups find (_.id === i)

  lazy val fmet: St ⇒ Method.Id ⇒ Option[Method.Cli] = s ⇒ i ⇒ s.mets find (_.id === i)

  lazy val fuse: St ⇒ Use.Id ⇒ Option[Use.Cli] = s ⇒ i ⇒ s.uses find (_.id.v === i.v)

  lazy val fsto: St ⇒ Location.Id ⇒ Option[Location.Cli] = s ⇒ i ⇒ s.stos find (_.id === i) 

  // ---------------------------------------------------------------
  //                      Load State
  // ---------------------------------------------------------------

  case class LoadState(
    prosLoaded: Boolean,
    stosLoaded: Boolean,
    supsLoaded: Boolean,
    metsLoaded: Boolean,
    usesLoaded: Boolean,
  ){
    def loaded: Boolean =
      prosLoaded && stosLoaded && supsLoaded && metsLoaded && usesLoaded
  }

  lazy val loadInit: LoadState = LoadState(false, false, false, false, false)

  lazy val adjLoadState: (LoadState,Option[Result]) ⇒ LoadState = {
    case (l@LoadState(pr, st, su, me, us),res) ⇒ res match {
      case Some(LoggedIn(_,_,_))      ⇒ loadInit
      case Some(LoggedOut)            ⇒ loadInit
      case Some(ProRes(Found(_,_,_))) ⇒ LoadState(true, st,   su,   me,   us)
      case Some(StoRes(Found(_,_,_))) ⇒ LoadState(pr,   true, su,   me,   us)
      case Some(SupRes(Found(_,_,_))) ⇒ LoadState(pr,   st,   true, me,   us)
      case Some(MetRes(Found(_,_,_))) ⇒ LoadState(pr,   st,   su,   true, us)
      case Some(UseRes(Found(_,_,_))) ⇒ LoadState(pr,   st,   su,   me,   true)
      case _ ⇒ l
    }
  }
}

