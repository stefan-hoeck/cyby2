/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

import cats.implicits._

import cyby.dat.{Mol ⇒ DMol, StatsType}
import cyby.dat.example.{CpdField ⇒ SField, ConField ⇒ CField, BioField ⇒ BField, _}
import cyby.export.{Format, Sdf, Odf, Txt}
import cyby.server.export.{OfficeValueType ⇒ OVT, _}

import java.nio.file.{Paths, Path ⇒ PathNIO}

/**
  * Decodes an object of type ESettings (export settings), runs
  * the corrsponding combined query and exports all
  * hits into a file of the given format.
  *
  * The fields to be exported are defined by a list
  * of "cyby.dat.example.ExportField"s.
  */
case class Export(coreSettings: CoreSettings)
  extends cyby.server.export.Env[LoggedInEnv] with CyByZ {
  import ImplicitContextShift.cs

  val query = Query(coreSettings)
  val field = Field(coreSettings)
  val M = CyByMonadIO.authEnv[Unit]

  protected def path(fn: String) = Paths get s"/data/cyby/export/${fn}"

  def prog(r: Request): M.Prog[Result] = for {
    le  <- M.ask
    exp <- M.decodeReq[ESettings](r)
    ss  <- query.queryCpds(exp.query, le).trans(Pure.to[IO])
    fn  =  s"${le.u.alias.v}_${le.env.timestamp}.${exp.format}"
    nameFs = exp.fields map locName(le.st)

    _   <- M lift act(ss, exp.format, nameFs, path(fn))
  } yield ExportRes(fn).r

  private def pairs(ss: List[Compound.Cli]) =
    ss.map(s ⇒ (s,s.structure.v.o)).collect{ case (s,Some(m)) ⇒ s -> m }

  private def act(ss: List[Compound.Cli], f: Format, fs: List[(ExportField,String)], fn: PathNIO) =
    f match {
      case Sdf ⇒ store(strings(pairs(ss))(sdf(fs)), fn)
      case Txt ⇒ store(strings(ss)(txt(fs), txtHeader(fs)), fn)
      case Odf ⇒ zipOds(strings(ss)(odf(fs), OVT.prefix :: odfHeader(fs), List(OVT.postfix)), fn)
    }

  private def statsName(st: St)(m: Method.Id, s: StatsType): String =
    MethodS.link(st,m)
      .fold(_ ⇒ s locName LocEnUS, m ⇒ s"${m._2} (${s locName LocEnUS})")
    
  private def locName(st: St)(f: ExportField): (ExportField,String) = f match {
    case ExportCpd(sf)       ⇒ (f, sf locName LocEnUS)
    case ExportCon(cf)       ⇒ (f, cf locName LocEnUS)
    case ExportBio(bf)       ⇒ (f, bf locName LocEnUS)
    case ExportStats(m,s)    ⇒ (f, statsName(st)(m, s))
  }

  private def odf(fields: List[(ExportField,String)]): Compound.Cli ⇒ List[String] = s ⇒
    OVT odfRow  apply(field sub _ odf, field con _ odf, field bio _ odf, statsOdf, s, fields)

  private def apply[A](
    sub:    SField ⇒ Compound.Cli ⇒ A,
    con:    CField ⇒ Container.Cli ⇒ A,
    bio:    BField ⇒ BiodataEntry.Cli ⇒ A,
    stats:  (String,Option[Double]) ⇒ A,
    s:      Compound.Cli,
    fields: List[(ExportField,String)],
  ): List[List[A]] = {
    def exp(c: Option[Container.Cli], b: Option[BiodataEntry.Cli], st: Option[BioStats]): List[A] = fields.flatMap{
      case (ExportCpd(f),n) ⇒ List(sub(f)(s))
      case (ExportCon(f),n) ⇒ c map con(f) toList
      case (ExportBio(f),n) ⇒ b map bio(f) toList
      case (ExportStats(m,s),n) ⇒
        st.map(x ⇒ stats(n, x.stats get m map s.get)).toList
    }

    if (fields.collect{ case (ExportBio(f),_) ⇒ unit}.nonEmpty)
      for {
        c <- s.containers
        b <- c.bio
      } yield exp(some(c), some(b), some(BioStats(s,c)))
    else if (
      fields.collect{ case (ExportCon(f),_) ⇒ f}.nonEmpty ||
      fields.collect{ case (ExportStats(_,_),_) ⇒ unit}.nonEmpty
    ) s.containers.map(c ⇒ exp(some(c), None, some(BioStats(s,c))))
    else List(exp(None, None,None))
  }

  private def txt(fields: List[(ExportField,String)]): Compound.Cli ⇒ List[String] = s ⇒
    textLines(apply(field sub _ txt, field con _ txt, field bio _ txt, (_,d) ⇒ d.fold("")(_.toString), s, fields))

  private val structF = ExportCpd(CpdMol(DMol.Structure)).f

  private def sdf(fields: List[(ExportField,String)]): ((Compound.Cli,DMol)) ⇒ List[String] = {
    case (s,m) ⇒ sdfBlock(apply(
      sdfField((f: SField) ⇒ field sub f sdf)(_ locName LocEnUS),
      sdfField((f: CField) ⇒ field con f txt)(_ locName LocEnUS),
      sdfField((f: BField) ⇒ field bio f txt)(_ locName LocEnUS),
      (n,o) ⇒ o.fold(sdfEntry(n,""))(d ⇒ sdfEntry(n, d.toString)),
      s,
      fields.filterNot{ case (f,_) ⇒ structF === f}
    ) map (m.structure.v :: _))
  }
}
