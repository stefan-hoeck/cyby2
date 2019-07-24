/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package msf

import cats.{Applicative, Monoid}
import cats.data.{Kleisli, StateT, RWST, WriterT}
import cats.implicits._

/**
  * Typeclass for lifting an IO value into another effect.
  */
trait LiftIO[F[_]] {
  def liftIO[A](io: IO[A]): F[A]
}

object LiftIO {
  def apply[F[_]](implicit L: LiftIO[F]): LiftIO[F] = L

  implicit val ioI: LiftIO[IO] = new LiftIO[IO] {
    def liftIO[A](io: IO[A]) = io
  }

  implicit def writerI[F[_]:LiftIO:Applicative, L:Monoid]: LiftIO[WriterT[F,L,?]] =
    new LiftIO[WriterT[F,L,?]] {
      def liftIO[A](io: IO[A]) = WriterT.liftF[F,L,A](LiftIO[F] liftIO io)
    }

  implicit def kleisliI[F[_]:LiftIO, L]: LiftIO[Kleisli[F,L,?]] =
    new LiftIO[Kleisli[F,L,?]] {
      def liftIO[A](io: IO[A]) = Kleisli[F,L,A](_ ⇒ LiftIO[F] liftIO io)
    }

  implicit def stateI[F[_]:LiftIO:Applicative, S]: LiftIO[StateT[F,S,?]] =
    new LiftIO[StateT[F,S,?]] {
      def liftIO[A](io: IO[A]) =
        StateT[F,S,A](s ⇒ LiftIO[F] liftIO io map (s -> _))
    }

  implicit def rwstI[F[_]:LiftIO:Applicative,E,L:Monoid,S]: LiftIO[RWST[F,E,L,S,?]] =
    new LiftIO[RWST[F,E,L,S,?]] {
      def liftIO[A](io: IO[A]) = RWST.liftF[F,E,L,S,A](LiftIO[F] liftIO io)
    }
}

// vim: set ts=2 sw=2 et:

