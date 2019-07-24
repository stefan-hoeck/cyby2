/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby

import cats.{Monad, Semigroup}, cats.implicits._

import org.scalacheck.Shapeless._

import org.scalacheck._

trait TestImplicits {
  def arbInst[A](implicit A: Arbitrary[A]): Arbitrary[A] = A

  def arb[A:Arbitrary]: Gen[A] = Arbitrary.arbitrary[A]

  def mixedGen[A](g: Gen[A], as: A*): Gen[A] =
    Gen.frequency(1 -> g, 9 -> Gen.oneOf(as))

  // Enums
  def enumGen[A:Enum]: Gen[A] = Gen.oneOf(Enum[A].values.toList)

  implicit def enumArb[A:Enum]: Arbitrary[A] = Arbitrary(enumGen[A])

  // Ids
  def idRandomGen[A]: Gen[Id[A]] = Gen choose (-1000L, 1000000L) map Id.apply

  def idGen[A]: Gen[Id[A]] = mixedGen[Id[A]](idRandomGen, Id(0), Id(1), Id(2))

  implicit def idArb[A]: Arbitrary[Id[A]] = Arbitrary(idGen)

  implicit def idCogen[A]: Cogen[Id[A]] = Cogen[Long].contramap(_.v)

  implicit def maybeArb[A:Arbitrary]: Arbitrary[Maybe[A]] =
    Arbitrary(arbInst[Option[A]].arbitrary map Maybe.apply)


  implicit def pureArb[A](implicit A: Arbitrary[A]): Arbitrary[Pure[A]] =
    Arbitrary(A.arbitrary map Pure.apply)

  implicit def nelArb[A](implicit A: Arbitrary[A]): Arbitrary[Nel[A]] = 
    Arbitrary(
      for {
        h <- A.arbitrary
        t <- Gen.listOf(A.arbitrary)
      } yield Nel(h,t)
    )

  object tagArbInstances {
    implicit def tagArb[V:Arbitrary,K]: Arbitrary[V @@ K] =
      Arbitrary(arb[V] map dotag[V,K])

    implicit def tagCog[V:Cogen,K]: Cogen[V @@ K] = Cogen[V].contramap[V @@ K](v ⇒ v)
  }

  // Monad

  implicit val genMonad: Monad[Gen] = new Monad[Gen] {
    def pure[A](a: A): Gen[A] = Gen const a

    def flatMap[A, B](fa: Gen[A])(f: A ⇒ Gen[B]): Gen[B] = fa flatMap f

    def tailRecM[A, B](a: A)(f: (A) ⇒ Gen[Either[A, B]]): Gen[B] =
      f(a) flatMap {
        case Right(b) ⇒ pure(b)
        case Left(aa) ⇒ tailRecM(aa)(f)
      }
  }

  implicit def genSemigroup[T: Semigroup]: Semigroup[Gen[T]] = new Semigroup[Gen[T]] {
    def combine(g1: Gen[T], g2: Gen[T]): Gen[T] = for { t1 <- g1; t2 <- g2 } yield t1 |+| t2
  }
}

// vim: set ts=2 sw=2 et:
