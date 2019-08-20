/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server

import cats.data.OptionT
import doobie._

case class CoreSettings(
  contextShift: cats.effect.ContextShift[IO],
  dataPath: String,
  transactor: Transactor[IO]
)

object CoreSettings{
  def apply(cs: cats.effect.ContextShift[IO], p: String): CoreSettings =
    CoreSettings(cs, p, doobie.Transactor.fromDriverManager[IO]( "org.sqlite.JDBC", s"jdbc:sqlite:${p}/cyby.sqlite"))
}

/**
  * Environment of a client request together with the
  * actual in-memory server state S.
  *
  * @param logger    : used for logging
  * @param timeout   : number of milliseconds before a session times out
  * @param timestamp : time when the actual user session was initialized
  * @param st        : in memory server state
  * @param req       : actual HTTP request
  *
  * @tparam S : server state passed on with this environment
  */
case class Env[S](
  logger:    Logger[IO],
  timeout:   Long,
  timestamp: Timestamp,
  st:        S,
){
  def expiresAt: Timestamp = timeout + timestamp
}

object Env {
  private val Timeout = 8L * 60L * 60L * 1000L // 8 hours

  def apply[S](ref: Ref[S]): IO[Env[S]] =
    ref.get flatMap applyS

  def applyS[S](s: S): IO[Env[S]] =
    now map (Env(consoleLogger, Timeout, _, s))

  def opt[S](ref: Ref[S]): OptionT[IO,Env[S]] =
    OptionT[IO,Env[S]](apply(ref) map Some.apply)

  private def color (c: String, msg: String) = c + msg + Console.RESET
  private val red = (s: String) ⇒ color (Console.RED, s)
  private val green = (s: String) ⇒ color (Console.GREEN, s)
  private val yellow = (s: String) ⇒ color (Console.YELLOW, s)
  private val blue = (s: String) ⇒ color (Console.BLUE, s)


  /**
    * logger colorizing messages according to severity
    */
  lazy val consoleLogger = Logger( (msg,lvl) ⇒ lvl match {
      case Debug   ⇒ putStrLn ("[" + blue   ("debug") + "]   " + msg)
      case Info    ⇒ putStrLn ("[" + green  ("info") + "]    " + msg)
      case Warning ⇒ putStrLn ("[" + yellow ("warning") + "] " + msg)
      case Error   ⇒ putStrLn ("[" + red    ("error") + "]   " + msg)
    }
  )
}

// vim: set ts=2 sw=2 et:
