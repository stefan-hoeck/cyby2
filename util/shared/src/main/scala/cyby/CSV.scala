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
  def apply(ts: List[String]): ValNel[String,A]

  def map[B](f: A ⇒ B): CsvDecoder[B] = CsvDecoder.inst(apply(_) map f)
}

object CsvDecoder {
  def apply[A](implicit A: CsvDecoder[A]): CsvDecoder[A] = A

  def derive[A,HL](implicit A: CsvDecoder[HL], G: Generic.Aux[A,HL])
    : CsvDecoder[A] = A map G.from

  def inst[A](dec: List[String] ⇒ ValNel[String,A]): CsvDecoder[A] =
    new CsvDecoder[A]{
      def apply(ts: List[String]) = dec(ts)
    }

  def decode[A](ts: List[String])(implicit A: CsvDecoder[A]): ValNel[String,A] =
    A apply ts

  def uncsv[A:CsvDecoder](s: String, sep: String): ValNel[String,A] =
    decode[A](s split sep toList)

  implicit lazy val hnilI: CsvDecoder[HNil] = inst{
    case Nil ⇒ valid(HNil)
    case t   ⇒ fail(s"Expected EOI: $t")
  }

  implicit def hconsI[H:Read,T<:HList:CsvDecoder]: CsvDecoder[H :+: T] =
    inst{
      case Nil ⇒ fail(s"Unexpected end of input")
      case h::t ⇒ (Read[H] readVS h, decode[T](t)).mapN(_ :: _)
    }
}
