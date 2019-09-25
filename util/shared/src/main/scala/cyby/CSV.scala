/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby

import cats.implicits._

import shapeless.{HNil, HList, :: ⇒ :+:, Generic}

/**
  * Basic CSV encoder: Converts a value to a list of strings.
  */
trait CsvEncoder[A] {
  def apply(a: A): List[String]

  def contramap[B](f: B ⇒ A): CsvEncoder[B] = CsvEncoder.inst(b ⇒ apply(f(b)))
}

object CsvEncoder {
  def apply[A](implicit A: CsvEncoder[A]): CsvEncoder[A] = A

  def derive[A,HL](implicit A: CsvEncoder[HL], G: Generic.Aux[A,HL])
    : CsvEncoder[A] = A contramap G.to

  def inst[A](enc: A ⇒ List[String]): CsvEncoder[A] = new CsvEncoder[A]{
    def apply(a: A) = enc(a)
  }

  def encode[A](a: A)(implicit A: CsvEncoder[A]): List[String] = A apply a

  def csv[A:CsvEncoder](a: A, sep: String): String = encode(a) mkString sep

  implicit lazy val hnilI: CsvEncoder[HNil] = inst(_ ⇒ Nil)

  implicit def hconsI[H,T<:HList:CsvEncoder]: CsvEncoder[H :+: T] =
    inst(hl ⇒ hl.head.toString :: encode(hl.tail))
}

/**
  * Basic CSV decoder: Converts a list of strings to a value (with the
  * possibility of failure)
  */
trait CsvDecoder[A] {
  self ⇒
  final def apply(ts: List[String]): ValNel[CsvError,A] = run(None, 0, ts)

  final def runRow(row: Int, ts: List[String]): ValNel[CsvError,A] = run(Some(row),0,ts)

  protected def run(row: Option[Int], col: Int, ts: List[String]): ValNel[CsvError,A]

  def map[B](f: A ⇒ B): CsvDecoder[B] = new CsvDecoder[B]{
    protected def run(row: Option[Int], col: Int, ts: List[String]) =
      self.run(row, col, ts) map f
  }
}

object CsvDecoder {
  import CsvError._

  def apply[A](implicit A: CsvDecoder[A]): CsvDecoder[A] = A

  def derive[A,HL](implicit A: CsvDecoder[HL], G: Generic.Aux[A,HL])
    : CsvDecoder[A] = A map G.from

  def decode[A](ts: List[String])(implicit A: CsvDecoder[A]): ValNel[CsvError,A] =
    A apply ts

  def decodeRow[A](row: Int, ts: List[String])(implicit A: CsvDecoder[A]): ValNel[CsvError,A] =
    A.runRow(row, ts)

  def uncsv[A:CsvDecoder](s: String, sep: Char): ValNel[CsvError,A] =
    decode[A](split(s,sep))

  def uncsvRow[A:CsvDecoder](row: Int, sep: Char)(s: String): ValNel[CsvError,A] =
    CsvDecoder[A].runRow(row, split(s,sep))

  private def split(s: String, sep: Char): List[String] = s indexOf sep match {
    case -1 ⇒ List(s)
    case i  ⇒ {
      val (h,t) = s splitAt i
      h :: split(t.tail, sep)
    }
  }

  implicit lazy val hnilI: CsvDecoder[HNil] = new CsvDecoder[HNil]{
    protected def run(row: Option[Int], col: Int, ts: List[String]) = ts match {
      case Nil ⇒ valid(HNil)
      case t   ⇒ fail(NoEOIErr(row, col).e)
    }
  }

  implicit def hconsI[H,T<:HList](implicit H: Read[H], T: CsvDecoder[T])
    : CsvDecoder[H :+: T] = new CsvDecoder[H :+: T] {
    protected def run(row: Option[Int], col: Int, ts: List[String]) = ts match {
      case Nil  ⇒ fail(EOIErr(row).e)
      case h::t ⇒ (
        H.readV(h, ColErr(row, col, H msg h)),
        T.run(row, col+1, t)
      ).mapN(_ :: _)
    }
  }
}

sealed trait CsvError {
  def e: CsvError = this
  val row: Option[Int]

  override def toString: String = {
    def rowPrefix = row.fold("")(i ⇒ s"Row ${i}: ")
    def colStr = this match {
      case CsvError.ColErr(_, c, m) ⇒ s"Column ${c}: ${m}"
      case CsvError.EOIErr(_)       ⇒ "Unexpected end of line"
      case CsvError.NoEOIErr(_,c)   ⇒ s"Column ${c}: expected end of line"
    }

    rowPrefix ++ colStr
  }
}

object CsvError {
  case class ColErr(row: Option[Int], col: Int, msg: String) extends CsvError

  case class EOIErr(row: Option[Int]) extends CsvError

  case class NoEOIErr(row: Option[Int], col: Int) extends CsvError
}

