/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby

import cats.Applicative
import java.util.concurrent.atomic.AtomicLong

/**
  * This module provides an implementation of Monadic Stream Functions,
  * a model for arrowized functional reactive programming.
  * Use type SF as a starting point followed by EF as a model
  * for event streams. Other parts define utility functions
  * providing conversions between different types of effects
  * and corresponding signal function.
  */
package object msf extends msf.types {
  private [msf] var doDebug = false

  def some[A](a: A): Option[A] = Some(a)

  private final val atom: AtomicLong = new AtomicLong()

  private[msf] def debug(s: ⇒ String) = delay(if (doDebug) println(s))

  def uiId[F[_]:LiftIO]: F[Long] = impure(atom.incrementAndGet())

  def impure[F[_]:LiftIO,A](a: ⇒ A): F[A] = liftIO(delay(a))

  def liftIO[F[_]:LiftIO,A](io: IO[A]): F[A] = LiftIO[F] liftIO io

  val unit: Unit = ()

  val ioUnit: IO[Unit] = ioNow(())

  def pure[F[_]:Applicative,A](a: A): F[A] = Applicative[F] pure a
}

// vim: set ts=2 sw=2 et:

