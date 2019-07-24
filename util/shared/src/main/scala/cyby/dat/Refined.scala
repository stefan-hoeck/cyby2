/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

import cats.{Order, Show}
import cats.instances.all._

import io.circe.{Encoder,Decoder}

/**
  * Utility class for defining type class instances of
  * refined wrapper type (Alias, Name, CasN etc.). Use at the companion object.
  */
abstract class Refined[A:Read:Order:Encoder:Decoder,B](
  name:    String,
  get:     B ⇒ A,
){
  def unsafe(a: A): B = apply(a).get

  def apply(a: A): Option[B]

  def msg(s: String): String = s"Not a ${name}: $s"

  implicit lazy val orderI: Order[B] = Order by get

  implicit lazy val readI: Read[B] =
    Read[A].mapOpt(msg)(a ⇒ apply(adjustForDecoding(a)))

  implicit lazy val showI: Show[B] = Show.fromToString

  implicit lazy val encodeI: Encoder[B] = enc

  implicit lazy val decodeI: Decoder[B] = dec

  protected def enc: Encoder[B] =
    Encoder[A] contramap (b ⇒ adjustForEncoding(get(b)))

  protected def dec: Decoder[B] = Decoder[A] emap { a ⇒
    apply(adjustForDecoding(a)).fold[Either[String,B]](Left(name))(Right(_))
  }

  protected def adjustForEncoding(a: A): A = a

  protected def adjustForDecoding(a: A): A = a
}

abstract class RefinedDef[A:Read:Order:Encoder:Decoder,B](
  name:    String,
  get:     B ⇒ A,
) extends Refined[A,B](name, get) {
  val default: B

  override protected def dec: Decoder[B] = Decoder[Option[A]] emap {
    case Some(a) ⇒ apply(a).fold[Either[String,B]](Left(name))(Right(_))
    case None    ⇒ Right(default)
  }
}

abstract class RefinedStringWithLineBreaks[B](
  name:    String,
  get:     B ⇒ String,
) extends Refined[String,B](name, get) {
  override def adjustForEncoding(s: String) = escape(s)

  override def adjustForDecoding(s: String) = unescape(s)

  def escape(s: String): String = s map {
    case '\n' ⇒ '|'
    case c    ⇒ c
  }

  def unescape(s: String): String = s map {
    case '|'  ⇒ '\n'
    case c    ⇒ c
  }
}

// vim: set ts=2 sw=2 et:
