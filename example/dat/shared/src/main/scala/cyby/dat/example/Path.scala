/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat
package example

import cats.Eq, cats.implicits._
import io.circe.{Decoder, Encoder}

/**
  * Sum type representing properly typed paths leading to data objects in
  * the data tree. Paths are used in error messages and in the client
  * to figure out the behavior of interactive elements in the UI.
  */
sealed trait Path {
  def path: Path = this

  final override lazy val toString: String = this match {
    case BioFilP(p) ⇒ Path.csv(BioFilStr :: p)
    case BioP(p)    ⇒ Path.csv(BioStr :: p)
    case ConFilP(p) ⇒ Path.csv(ConFilStr :: p)
    case ConP(p)    ⇒ Path.csv(ConStr :: p)
    case MetP(p)    ⇒ Path.csv(MetStr :: p)
    case ProP(p)    ⇒ Path.csv(ProStr :: p)
    case StoP(p)    ⇒ Path.csv(StoStr :: p)
    case SubFilP(p) ⇒ Path.csv(SubFilStr :: p)
    case SubP(p)    ⇒ Path.csv(SubStr :: p)
    case SupP(p)    ⇒ Path.csv(SupStr :: p)
    case UseP(p)    ⇒ Path.csv(UseStr :: p)
    case RootP      ⇒ "root"
  }
}

case object RootP                           extends Path
case class BioFilP(p: BiodataEntry.FilPath) extends Path
case class BioP(p: BiodataEntry.Path)       extends Path
case class ConFilP(p: Container.FilPath)          extends Path
case class ConP(p: Container.Path)                extends Path
case class MetP(p: Method.Path)                extends Path
case class ProP(p: Project.Path)                extends Path
case class SubFilP(p: Sub.FilPath)          extends Path
case class SubP(p: Sub.Path)       extends Path
case class SupP(p: Sup.Path)       extends Path
case class UseP(p: Use.Path)       extends Path
case class StoP(p: Sto.Path)       extends Path

object Path {
  def unapply(s: String): Option[Path] = read(s)

  implicit lazy val eqI: Eq[Path] = Eq.fromUniversalEquals

  implicit lazy val readI: Read[Path] =
    Read.inst(s ⇒ s"Invalid cyby.dat.example.Path: $s")(read)

  implicit lazy val decI: Decoder[Path] = readI.decoder

  implicit lazy val encI: Encoder[Path] = Encoder[String] contramap (_.toString)

  private def csv[A:CsvEncoder](a: A): String = CsvEncoder.csv(a, "_")

  private def uncsv[A:CsvDecoder](f: A ⇒ Path, s: List[String]): Option[Path] =
    CsvDecoder.decode[A](s).toOption map f

  private def read(s: String): Option[Path] = s.split("_").toList match {
    case "root"::Nil  ⇒ some(RootP.path)
    case BioStr::t    ⇒ uncsv(BioP, t)
    case BioFilStr::t ⇒ uncsv(BioFilP, t)
    case ConStr::t    ⇒ uncsv(ConP, t)
    case ConFilStr::t ⇒ uncsv(ConFilP, t)
    case SubStr::t    ⇒ uncsv(SubP, t)
    case SubFilStr::t ⇒ uncsv(SubFilP, t)
    case MetStr::t    ⇒ uncsv(MetP, t)
    case ProStr::t    ⇒ uncsv(ProP, t)
    case UseStr::t    ⇒ uncsv(UseP, t)
    case SupStr::t    ⇒ uncsv(SupP, t)
    case StoStr::t    ⇒ uncsv(StoP, t)
    case _            ⇒ None
  }
}
