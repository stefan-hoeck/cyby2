/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package msf
package js

import cats.{Monad, Monoid}, cats.implicits._

abstract class MonadH[F[_]:Monad:LiftIO] {
  def pure[A](a: A): F[A] = Monad[F] pure a

  lazy val unit: F[Unit] = pure(())

  def liftIO[A](a: IO[A]): F[A] = LiftIO[F].liftIO(a)

  def delayed[A](a: ⇒ A): F[A] = liftIO(msf delay a)

  def delayedTry(f: ⇒ Unit): F[Unit] = liftIO(msf delay tryu(f))

  def delayedTryO[A](f: ⇒ A): F[Option[A]] = liftIO(msf delay tryO(f))

  def putStrLn(s: String): F[Unit] = liftIO(delay(println(s)))

  def when[A:Monoid](b: Boolean)(a: F[A]): F[A] =
    if (b) a else pure(Monoid[A].empty)

  def whenM[A:Monoid](b: F[Boolean])(a: F[A]): F[A] = b >>= (when(_)(a))
}

// vim: set ts=2 sw=2 et:
