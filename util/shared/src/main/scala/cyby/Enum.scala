/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby

import cats.Order, cats.implicits._
import io.circe.{Encoder, Decoder, KeyEncoder, KeyDecoder}

/**
  * Type class for finit sized lists of values
  */
trait Enum[A] {
  def values: Nel[A]
}

object Enum {
  def apply[A](implicit A: Enum[A]): Enum[A] = A

  def inst[A](as: Nel[A]): Enum[A] = new Enum[A]{
    def values = as
  }
}

/**
  * Utility trait typically used with companion objects to
  * provide typical typeclass implementations for Enum instance.
  */
trait EnumHelper[A] {
  self ⇒ 

  def name: String

  def values: Nel[A]

  def show(a: A): String = encode(a).head

  def encode(a: A): Nel[String]

  lazy val readMap: Map[String,A] =
    values.flatMap(a ⇒ encode(a) map (_ -> a)).toList.toMap

  lazy val idMap: Map[A,Int] = values.toList.zipWithIndex.toMap

  implicit lazy val readI: Read[A] =
    Read.inst(s ⇒ s"Not a ${name}: $s")(readMap.get)

  implicit lazy val ordI: Order[A] = Order by idMap

  implicit lazy val encI: Encoder[A] = Encoder[String] contramap show

  implicit lazy val decI: Decoder[A] = readI.decoder

  implicit lazy val enumI: Enum[A] = Enum inst values

  implicit lazy val keyEncI: KeyEncoder[A] =
    new KeyEncoder[A]{ def apply(a: A) = show(a) }

  implicit lazy val keyDecI: KeyDecoder[A] =
    new KeyDecoder[A]{ def apply(s: String) = readI read s }


  def lowerHead(s: String): String = s"${s.head.toLower}${s.tail}"
  
  def lowerHeadEncode(a: A): Nel[String] =
    Nel.of(lowerHead(a.toString), a.toString)
  
  def lowerHeadDropEncode(a: A, n: Int): Nel[String] =
    Nel.of(lowerHead(a.toString drop n), a.toString)
}
