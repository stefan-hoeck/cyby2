/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby

import cats.implicits._
import cats.{Eq, Monad, Traverse, Foldable,Eval,Applicative}

import io.circe.{Decoder, Encoder, HCursor, DecodingFailure}

/**
  * Option with different JSON encoding behavior
  *
  * Note: This type encodes empty values as raw empty strings. Do not
  * use, if this is a valid encoding for the type you wrap!
  *
  * This data type solves the following problematic usecase:
  * 
  * {{{
  * val x: Option[Option[Int]] = Some(None)
  *
  * Decoder[Option[Option[Int]]].decodeJson(x.asJson) == Right(None)
  * }}}
  *
  * Obviously, in the case above, some information is lost.
  * But when mutating optional data, we require
  * constructs like the one above: The outer Option is
  * used to signal whether we actually want to mutate an
  * item, while the inner is the item's new value. In these
  * cases, we use type {{{Maybe[Option[Int]]}}} instead to
  * circumvent the encoding problem descried here.
  */
case class Maybe[+A](o: Option[A]) extends AnyVal {
  def exists(p: A ⇒ Boolean): Boolean = o exists p
  def filter(p: A ⇒ Boolean): Maybe[A] = Maybe(o filter p)
  def filterNot(p: A ⇒ Boolean): Maybe[A] = Maybe(o filterNot p)
  def flatMap[B](f: A ⇒ Maybe[B]): Maybe[B] = Maybe(o flatMap (a ⇒ f(a).o))
  def fold[B](b: ⇒ B)(f: A ⇒ B): B = o.fold(b)(f)
  def forall(p: A ⇒ Boolean): Boolean = o forall p
  def getOrElse[AA>:A](a: ⇒ AA): AA = o getOrElse a
  def map[B](f: A ⇒ B): Maybe[B] = Maybe(o map f)
  def orElse[AA>:A](that: Maybe[AA]): Maybe[AA] = Maybe(o orElse that.o)
  def toList: List[A] = o.toList

  def get: A = o.get
}

object Maybe {
  def maybe[A](a: A): Maybe[A] = Maybe(some(a))

  def nothing[A]: Maybe[A] = Maybe(None)

  implicit def eqI[A:Eq]: Eq[Maybe[A]] = Eq.by(_.o)

  implicit lazy val monadI = new Monad[Maybe] with Traverse[Maybe] {
    def pure[A](a: A) = maybe(a)
    def flatMap[A,B](fa: Maybe[A])(f: A ⇒ Maybe[B]) = fa flatMap f
    def foldLeft[A,B](fa: Maybe[A], b: B)(f: (B,A) ⇒ B) = Foldable[Option].foldLeft(fa.o,b)(f)
    def foldRight[A,B](fa: Maybe[A], b: Eval[B])(f: (A,Eval[B]) ⇒ Eval[B]) = Foldable[Option].foldRight(fa.o,b)(f)
    def tailRecM[A,B](a: A)(f: A ⇒ Maybe[Either[A,B]]): Maybe[B] = Maybe(Monad[Option].tailRecM(a)(f(_).o))
    def traverse[G[_]:Applicative,A,B](fa: Maybe[A])(f: A ⇒ G[B]): G[Maybe[B]] = fa.o traverse f map Maybe.apply
  }

  implicit def decI[A](implicit A: Decoder[A]): Decoder[Maybe[A]] = new Decoder[Maybe[A]] {
    def empty(s: String): Either[DecodingFailure,Maybe[A]] = s match {
      case "" ⇒ Right(nothing[A])
      case s  ⇒ Left(DecodingFailure.fromThrowable(new Throwable(s"unexpected string: $s"),Nil))
    }

    def apply(h: HCursor) = Decoder[String].apply(h).flatMap(empty) <+> A(h).map(maybe)
      
  }

  implicit def encI[A](implicit A: Encoder[A]): Encoder[Maybe[A]] = new Encoder[Maybe[A]]{
    def apply(m: Maybe[A]) = m.fold(Encoder[String] apply "")(A.apply)
  }
}
