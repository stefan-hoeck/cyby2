/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server

import cats.Show
import cats.effect.{ContextShift, Resource}

import scala.util.control.NonFatal

import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors

trait util {
  val hnil: shapeless.HNil = shapeless.HNil

  def async[A](cb: (Either[Throwable,A] ⇒ Unit) ⇒ Unit): IO[A] =
    cats.effect.IO async cb

  lazy val ioUnit: IO[Unit] = IO pure (())

  val IO = cats.effect.IO

  def delay[A](a: ⇒ A): IO[A] = cats.effect.IO(a)

  def putStrLn(s: ⇒ String): IO[Unit] = delay(println(s))

  def printLn[A](a: ⇒ A)(implicit S: Show[A]): IO[Unit] = putStrLn(S show a)

  def eval[A](io: IO[A]): IStream[A] = fs2.Stream eval io

  val now: IO[Timestamp] = delay(System.currentTimeMillis())

  def tryIO(logger: Logger[IO])(f: ⇒ Unit): IO[Unit] = delay{
    try { f } catch {
      case e@NonFatal(_) ⇒ logger.log(e.toString, Error).unsafeRunSync()
    }
  }

  def utf8Lines(p: String)(implicit c: ContextShift[IO]): fs2.Stream[IO,String] =
    blocking.flatMap { b ⇒
      fs2.io.file.readAll[IO](java.nio.file.Paths get p, b, 4096)
          .through(fs2.text.utf8Decode)
          .through(fs2.text.lines)
    }

  def allLines(p: String)(implicit c: ContextShift[IO]): IO[List[String]] =
    utf8Lines(p).compile.to[List]

  def blocking: fs2.Stream[IO,ExecutionContext] =
    fs2.Stream.resource(blockingExecutionContext)
    
  private val blockingExecutionContext =
    Resource.make(IO(ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())))(ec ⇒ IO(ec.shutdown()))
}

// vim: set ts=2 sw=2 et:
