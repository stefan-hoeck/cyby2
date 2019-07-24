/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby

import cats._, cats.implicits._

/**
  * Entries for logging
  */
class Log(val lvl: LogLevel, msg: ⇒ String) {
  def message: String = msg
}

object Log {
  def apply(lvl: LogLevel, msg: ⇒ String): Log = new Log(lvl, msg)

  def debug(msg: ⇒ String): Log = Log(Debug, msg)
  def info(msg: ⇒ String):  Log = Log(Info, msg)
  def warn(msg: ⇒ String):  Log = Log(Warning, msg)
  def error(msg: ⇒ String): Log = Log(Error, msg)
}


sealed trait LogLevel {
  def value: Int
}

case object Debug extends LogLevel { val value = 1 }

case object Info extends LogLevel { val value = 10 }

case object Warning extends LogLevel { val value = 100 }

case object Error extends LogLevel { val value = 1000 }

object LogLevel extends EnumHelper[LogLevel] {
  val name = "LogLevel"
  val values = Nel.of(Debug,Info,Warning,Error)
  def encode(l: LogLevel) = lowerHeadEncode(l)
}

sealed trait Logger[F[_]] {
  def apply(l: Log): F[Unit] = log(l.message, l.lvl)

  def log(msg: ⇒ String, lvl: LogLevel): F[Unit]

  def filtered(l: LogLevel)(implicit A: Applicative[F]): Logger[F] =
    Logger((msg,lvl) ⇒ if (lvl >= l) log(msg, lvl) else A.pure(()))
}

object Logger {
  def apply[F[_]](f: (=> String, LogLevel) ⇒ F[Unit]): Logger[F] = new Logger[F] {
    def log(msg: ⇒ String, lvl: LogLevel) = f(msg, lvl)
  }

  def empty[F[_]](implicit A: Applicative[F]): Logger[F] = new Logger[F]{
    def log(msg: ⇒ String, lvl: LogLevel) = A.pure(())
  }

  implicit def loggerMonoid[F[_]](implicit M: Monad[F]): Monoid[Logger[F]] =
    new Monoid[Logger[F]] {
      def empty = Logger.empty[F]
      def combine(a: Logger[F], b: Logger[F]) = new Logger[F] {
        def log(msg: ⇒ String, lvl: LogLevel) =
          M.flatMap(a.log(msg, lvl))(_ ⇒ b.log(msg, lvl))
      }
    }
}

