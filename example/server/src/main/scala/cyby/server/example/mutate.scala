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
    case _ -> _ / _ / DT(t@MetT)   ⇒ editDec(MetS fullEd hnil, logMet, t, r)
    case _ -> _ / _ / DT(t@SupT)   ⇒ editDec(SupS fullEd hnil, logSup, t, r)
    case _ -> _ / _ / DT(t@UseT)   ⇒ editDec(UseS fullEd hnil, logUse, t, r)
    case _ -> _ / _ / DT(t@ProT)   ⇒ editDec(ProS fullEd hnil, logPro, t, r)
    case _ -> _ / _ / DT(t@StoT)   ⇒ editDec(StoS fullEd hnil, logSto, t, r)
    case _ -> _ / _ / DT(t@SubT)   ⇒ editDec(edSub, logSub, t, r, delFil)
    case _ -> _ / _ / DT(FilT)     ⇒ edFile(r)
    case _ -> _ / _ / "settings"   ⇒ decoding(mutate(adjSettings)(storeSettings))(r)
    case r                         ⇒ M.raise(NotFound(r.uri.path))
  }

  val edSub = (v: EditEnv, st: St, p: SubTree) ⇒ p match {
    case SubEdit(e)      ⇒ m3(SubS.fullEd(hnil)(v,st,e))(subEd)
    case ConEdit(p,e)    ⇒ m3(ContainerS.fullEd(p)(v,st,e))(conEd(p))
    case BioEdit(p,e)    ⇒ m3(BiodataEntryS.fullEd(p)(v,st,e))(bioEd(p))
    case SubFilEdit(p,e) ⇒ m3(SubFilS.fullEd(p)(v,st,e))(subFilEd(p))
    case ConFilEdit(p,e) ⇒ m3(ConFilS.fullEd(p)(v,st,e))(conFilEd(p))
    case BioFilEdit(p,e) ⇒ m3(BioFilS.fullEd(p)(v,st,e))(bioFilEd(p))
  }

  def delFil(p: SubTree): IO[Unit] = p match {
    case SubFilEdit(p,Del(i)) ⇒ files delete SubFilP(i :: p)
    case ConFilEdit(p,Del(i)) ⇒ files delete ConFilP(i :: p)
    case BioFilEdit(p,Del(i)) ⇒ files delete BioFilP(i :: p)
    case _                    ⇒ ioUnit
  }

  private def m3[A,B,X,Y](e: DataE[(X,A,Y)])(f: A ⇒ B): DataE[(X,B,Y)] =
    e map { case (x,a,y) ⇒ (x,f(a),y) }

  def addFil(
    pth: Fil.Id ⇒ Path,
    bs:  Array[Byte],
    e:   DataE[(St @@ Adjusted,SubFilS.LoadEd,Result)]
  )(
    f: SubFilS.LoadEd ⇒ SubTreeL
  ): M.Prog[(St @@ Adjusted,SubTreeL,Result)] = for {
    t             <- M wrapEither e
    (st,sfl,res)  = t
    id            = sfl.id(a ⇒ mapTagged(a)(_.id))
    _             <- M lift files.insert(pth(id))(bs)
  } yield (st,f(sfl),res)

  def bioEd(p: Container.Path)(ed: BiodataEntryS.LoadEd): SubTreeL = BioEdit(p, ed)
  def bioFilEd(p: BiodataEntry.Path)(ed: BioFilS.LoadEd): SubTreeL = BioFilEdit(p, ed)
  def conEd(p: Sub.Path)(ed: ContainerS.LoadEd): SubTreeL = ConEdit(p, ed)
  def conFilEd(p: Container.Path)(ed: ConFilS.LoadEd): SubTreeL = ConFilEdit(p, ed)
  def subEd(ed: SubS.LoadEd): SubTreeL = SubEdit(ed)
  def subFilEd(p: Sub.Path)(ed: SubFilS.LoadEd): SubTreeL = SubFilEdit(p, ed)

  def edFile(r: Request): M.Prog[Result] = for {
    v   <- M.ask
    st  <- M.get
    pr  <- decodeAddFile[SubTree](r)
    (ed,bs) = pr
    t   <- ed match {
             case SubFilEdit(p, e@Add(_)) ⇒ 
               addFil(i ⇒ SubFilP(i::p), bs, SubFilS.fullEd(p)(v,st,e))(subFilEd(p))
             case ConFilEdit(p, e@Add(_)) ⇒
               addFil(i ⇒ ConFilP(i::p), bs, ConFilS.fullEd(p)(v,st,e))(conFilEd(p))
             case BioFilEdit(p, e@Add(_)) ⇒
               addFil(i ⇒ BioFilP(i::p), bs, BioFilS.fullEd(p)(v,st,e))(bioFilEd(p))
             case _ ⇒ M.raise(ReadErr("error when adding file"))
           }
    (newSt,ed,res) = t
    _   <- M lift appendLine(SubT, ed.asJson.noSpaces)
    _   <- M doLog logSub(ed)
    _   <- M set newSt
  } yield res

  lazy val adjSettings = (le: EditEnv, st: St, p: (Use.Id,USettings)) ⇒
    (dotag[St,Adjusted](St.L.sets.modify(st)(_ + p)), p, SettingsChanged(p._2).r)

  def logSub(s: SubTreeL): Log = s match {
    case SubEdit(ed)    ⇒ logEd("substance", ed)(_.id)
    case ConEdit(p, ed) ⇒ logEd(s"substance ${p.head}: container", ed)(_.id)
    case BioEdit(p, ed) ⇒ logEd(s"substance ${p.tail.head}, container ${p.head}: biodata", ed)(_.id)
    case SubFilEdit(p, ed) ⇒ logEd(s"substance ${p.head}: file", ed)(_.id)
    case ConFilEdit(p, ed) ⇒ logEd(s"substance ${p.tail.head}, container ${p.head}: file", ed)(_.id)
    case BioFilEdit(p, ed) ⇒ logEd(s"substance ${p.tail.tail.head}, container ${p.tail.head}, biodata ${p.head}: file", ed)(_.id)
  }

  def logPro(s: ProS.LoadEd): Log = logEd("pro", s)(_.id)
  def logSto(s: StoS.LoadEd): Log = logEd("sto", s)(_.id)
  def logMet(s: MetS.LoadEd): Log = logEd("met", s)(_.id)
  def logSup(s: SupS.LoadEd): Log = logEd("sup", s)(_.id)
  def logUse(s: UseS.LoadEd): Log = logEd("use", s)(_.id)
}

// vim: set ts=2 sw=2 et:
