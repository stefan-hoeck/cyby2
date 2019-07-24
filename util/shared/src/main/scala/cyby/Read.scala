/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby

import cats.implicits._

import io.circe.Decoder

import scala.util.matching.Regex

/** A typeclass similar to the Read typeclass in Haskell but
  * returning optional results after parsing a String.
  *
  * Instances of this typeclass are
  * mostly used for reading user input from UI widgets.
  * Only a minimal set of instances are therefore provided.
  */
trait Read[A] {

  /**
    * Error message to be returned if reading fails. This is
    * mostly used in (JSON)-decoders or when untyped error
    * messages are acceptable. For better control over
    * error handling and error types, use functions readE and readV.
    */
  def msg(s: String): String

  def read(s: String): Option[A]

  def readV[E](s: String, err: ⇒ E): ValNel[E,A] =
    optionToValNel(err)(read(s))

  def readE[E](s: String, err: ⇒ E): ErrNel[E,A] =
    optionToErrNel(err)(read(s))

  def readES(s: String): ErrNel[String,A] = readE(s, msg(s))

  def readVS(s: String): ValNel[String,A] = readV(s, msg(s))

  def map[B](f: A ⇒ B): Read[B] = Read.inst(msg)(read(_) map f)

  def mapOpt[B](m: String ⇒ String)(f: A ⇒ Option[B]): Read[B] =
    Read.inst(m)(read(_) flatMap f)

  /**
    * Automatically creates a Json decoder from this read instance.
    *
    * For numerical types, this is probably not what you want.
    */
  def decoder: Decoder[A] = Decoder[String].emap(readES(_) leftMap (_.head))
}

object Read {
  def apply[A:Read]: Read[A] = implicitly

  implicit def optionI[A](implicit ra: Read[A]): Read[Option[A]] =
    inst(ra.msg) {
      case "" ⇒ Some(none[A])
      case s  ⇒ ra read s map Some.apply
    }

  def inst[A](m: String ⇒ String)(r: String ⇒ Option[A]): Read[A] =
    new Read[A]{
      def msg(s: String) = m(s)
      def read(s: String) = r(s)
    }

  def tryI[A](msg: String ⇒ String)(f: String ⇒ A): Read[A] =
    inst[A](msg)(s ⇒ tryO(f(s)))

  implicit val bigIntI  : Read[BigInt]  = tryI(s ⇒ s"Not a BigInt: $s")(BigInt(_))

  implicit val booleanI : Read[Boolean] = tryI(s ⇒ s"Not a Boolean: $s")(_.toBoolean)

  implicit val byteI    : Read[Byte]    = tryI(s ⇒ s"Not a Byte: $s")(_.toByte)

  implicit val charI    : Read[Char]    = inst(s ⇒ s"Not a Char: $s")(_.headOption)

  implicit val doubleI  : Read[Double]  = tryI(s ⇒ s"Not a Double: $s")(_.toDouble)

  implicit val floatI   : Read[Float]   = tryI(s ⇒ s"Not a Float: $s")(_.toFloat)

  implicit val intI     : Read[Int]     = tryI(s ⇒ s"Not an Int: $s")(_.toInt)

  implicit val longI    : Read[Long]    = tryI(s ⇒ s"Not a Long: $s")(_.toLong)

  implicit val regexI   : Read[Regex]   = tryI(s ⇒ s"Not a Regex: $s")(_.r)

  implicit val shortI   : Read[Short]   = tryI(s ⇒ s"Not a Short: $s")(_.toShort)

  implicit val stringI  : Read[String]  = inst(s ⇒ s"Not a String: $s")(Some(_))

  implicit val unitI    : Read[Unit]    = inst(s ⇒ s"Not Unit: $s"){
                                            case "()" ⇒ Some(())
                                            case _    ⇒ None
                                          }
}

// vim: set ts=2 sw=2 et:
