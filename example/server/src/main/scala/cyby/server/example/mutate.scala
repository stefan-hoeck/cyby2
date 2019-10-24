/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

import cyby.dat._, example.{DataType ⇒ DT, _}

import tagInstances._

import org.http4s.dsl.io.{ Root ⇒ _, NotFound ⇒ _, Path ⇒ _, _ }

import io.circe.syntax._

/**
  * Component used (together with the different editors)
  * for creating, editing, and deleting data objects.
  */
case class Mutate(coreSettings: CoreSettings) extends CyByZ with MutateEnv[EditEnv] {
  val M = CyByMonadIO.editEnv[St]
  val files = Files(coreSettings)

  def env(e: EditEnv) = e.env
  def readErr(s: String) = ReadErr(s)

  def prog(r: Request): M.Prog[Result] =  r match {
    case _ -> _ / _ / DT(t@MetT)   ⇒ editDec(MethodS fullEd hnil, logMet, t, r)
    case _ -> _ / _ / DT(t@SupT)   ⇒ editDec(SupplierS fullEd hnil, logSup, t, r)
    case _ -> _ / _ / DT(t@UseT)   ⇒ editDec(UserS fullEd hnil, logUse, t, r)
    case _ -> _ / _ / DT(t@ProT)   ⇒ editDec(ProjectS fullEd hnil, logPro, t, r)
    case _ -> _ / _ / DT(t@StoT)   ⇒ editDec(LocationS fullEd hnil, logSto, t, r)
    case _ -> _ / _ / DT(t@CpdT)   ⇒ editDec(edCpd, logCpd, t, r, delFil)
    case _ -> _ / _ / DT(FilT)     ⇒ edFile(r)
    case _ -> _ / _ / "settings"   ⇒ decoding(mutate(adjSettings)(storeSettings))(r)
    case r                             ⇒ M.raise(NotFound(r.uri.path))
  }

  val edCpd = (v: EditEnv, st: St, p: CpdTree) ⇒ p match {
    case CpdEdit(e)      ⇒ m3(CompoundS.fullEd(hnil)(v,st,e))(subEd)
    case ConEdit(p,e)    ⇒ m3(ContainerS.fullEd(p)(v,st,e))(conEd(p))
    case BioEdit(p,e)    ⇒ m3(BiodataEntryS.fullEd(p)(v,st,e))(bioEd(p))
    case CpdFilEdit(p,e) ⇒ m3(CpdFileS.fullEd(p)(v,st,e))(subFilEd(p))
    case ConFilEdit(p,e) ⇒ m3(ConFileS.fullEd(p)(v,st,e))(conFilEd(p))
    case BioFilEdit(p,e) ⇒ m3(BioFileS.fullEd(p)(v,st,e))(bioFilEd(p))
  }

  def delFil(p: CpdTree): IO[Unit] = p match {
    case CpdFilEdit(p,Del(i)) ⇒ files delete CpdFilP(i :: p)
    case ConFilEdit(p,Del(i)) ⇒ files delete ConFilP(i :: p)
    case BioFilEdit(p,Del(i)) ⇒ files delete BioFilP(i :: p)
    case _                    ⇒ ioUnit
  }

  private def m3[A,B,X,Y](e: DataE[(X,A,Y)])(f: A ⇒ B): DataE[(X,B,Y)] =
    e map { case (x,a,y) ⇒ (x,f(a),y) }

  def addFil(
    pth: File.Id ⇒ Path,
    bs:  Array[Byte],
    e:   DataE[(St @@ Adjusted,CpdFileS.LoadEd,Result)]
  )(
    f: CpdFileS.LoadEd ⇒ CpdTreeL
  ): M.Prog[(St @@ Adjusted,CpdTreeL,Result)] = for {
    t             <- M wrapEither e
    (st,sfl,res)  = t
    id            = sfl.id(a ⇒ mapTagged(a)(_.id))
    _             <- M lift files.insert(pth(id))(bs)
  } yield (st,f(sfl),res)

  def bioEd(p: Container.Path)(ed: BiodataEntryS.LoadEd): CpdTreeL = BioEdit(p, ed)
  def bioFilEd(p: BiodataEntry.Path)(ed: BioFileS.LoadEd): CpdTreeL = BioFilEdit(p, ed)
  def conEd(p: Compound.Path)(ed: ContainerS.LoadEd): CpdTreeL = ConEdit(p, ed)
  def conFilEd(p: Container.Path)(ed: ConFileS.LoadEd): CpdTreeL = ConFilEdit(p, ed)
  def subEd(ed: CompoundS.LoadEd): CpdTreeL = CpdEdit(ed)
  def subFilEd(p: Compound.Path)(ed: CpdFileS.LoadEd): CpdTreeL = CpdFilEdit(p, ed)

  def edFile(r: Request): M.Prog[Result] = for {
    v   <- M.ask
    st  <- M.get
    pr  <- decodeAddFile[CpdTree](r)
    (ed,bs) = pr
    t   <- ed match {
             case CpdFilEdit(p, e@Add(_)) ⇒ 
               addFil(i ⇒ CpdFilP(i::p), bs, CpdFileS.fullEd(p)(v,st,e))(subFilEd(p))
             case ConFilEdit(p, e@Add(_)) ⇒
               addFil(i ⇒ ConFilP(i::p), bs, ConFileS.fullEd(p)(v,st,e))(conFilEd(p))
             case BioFilEdit(p, e@Add(_)) ⇒
               addFil(i ⇒ BioFilP(i::p), bs, BioFileS.fullEd(p)(v,st,e))(bioFilEd(p))
             case _ ⇒ M.raise(ReadErr("error when adding file"))
           }
    (newSt,ed,res) = t
    _   <- M lift appendLine(CpdT, ed.asJson.noSpaces)
    _   <- M doLog logCpd(ed)
    _   <- M set newSt
  } yield res

  lazy val adjSettings = (le: EditEnv, st: St, p: (User.Id,USettings)) ⇒
    (dotag[St,Adjusted](St.L.sets.modify(st)(_ + p)), p, SettingsChanged(p._2).r)

  def logCpd(s: CpdTreeL): Log = s match {
    case CpdEdit(ed)    ⇒ logEd("substance", ed)(_.id)
    case ConEdit(p, ed) ⇒ logEd(s"substance ${p.head}: container", ed)(_.id)
    case BioEdit(p, ed) ⇒ logEd(s"substance ${p.tail.head}, container ${p.head}: biodata", ed)(_.id)
    case CpdFilEdit(p, ed) ⇒ logEd(s"substance ${p.head}: file", ed)(_.id)
    case ConFilEdit(p, ed) ⇒ logEd(s"substance ${p.tail.head}, container ${p.head}: file", ed)(_.id)
    case BioFilEdit(p, ed) ⇒ logEd(s"substance ${p.tail.tail.head}, container ${p.tail.head}, biodata ${p.head}: file", ed)(_.id)
  }

  def logPro(s: ProjectS.LoadEd): Log = logEd("pro", s)(_.id)
  def logSto(s: LocationS.LoadEd): Log = logEd("sto", s)(_.id)
  def logMet(s: MethodS.LoadEd): Log = logEd("met", s)(_.id)
  def logSup(s: SupplierS.LoadEd): Log = logEd("sup", s)(_.id)
  def logUse(s: UserS.LoadEd): Log = logEd("use", s)(_.id)
}

// vim: set ts=2 sw=2 et:
