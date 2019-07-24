/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby

import io.circe.{Decoder,Encoder}
import cats.{Eq, Monad}, cats.implicits._
import cats.arrow.FunctionK

/**
  * Monadic wrapper around eagerly evaluated values.
  */
final case class Pure[A](v: A) extends AnyVal {
  override def toString = v.toString
}

object Pure {
  def to[F[_]:Monad]: FunctionK[Pure,F] = new FunctionK[Pure,F]{
    def apply[A](p: Pure[A]) = Monad[F] pure p.v
  }

  implicit def toPure[A](a: A): Pure[A] = Pure(a)

  implicit def fromPure[A](p: Pure[A]): A = p.v

  implicit def eqI[A:Eq]: Eq[Pure[A]] = new Eq[Pure[A]]{
    def eqv(a: Pure[A], b: Pure[A]) = a.v === b.v
  }

  implicit val monadI: Monad[Pure] = new Monad[Pure] {
    def pure[A](a: A) = Pure(a)
    def flatMap[A,B](fa: Pure[A])(f: A ⇒ Pure[B]) = f(fa.v)
    def tailRecM[A,B](a: A)(f: A ⇒ Pure[Either[A,B]]): Pure[B] = f(a) match {
      case Pure(Left(a))  ⇒ tailRecM(a)(f)
      case Pure(Right(b)) ⇒ Pure(b)
    }
  }

  implicit def pureEncI[A](implicit A: Encoder[A]): Encoder[Pure[A]] =
    A.contramap(_.v)

  implicit def pureDecI[A](implicit A: Decoder[A]): Decoder[Pure[A]] =
    A.map(Pure.apply)
}

