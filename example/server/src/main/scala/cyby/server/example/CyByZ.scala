/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

import cyby.dat.{Alias,example}, example._
import java.text.SimpleDateFormat

/**
  * Default environment
  */
trait CyByZ extends ServerEnv with cyby.dat.example.ZEnv {
  type St = cyby.server.example.St

  type UseIdType = cyby.dat.example.Use.type

  type UseS = cyby.server.example.UseS.Srv

  type Err = DataErr

  type SProg[C,S,A] = CyByProg[IO,C,S,A]

  type SLProg[C,A] = SProg[C,Unit,A]

  type SLMonad[C] = CyByMonadIO[C,Unit]

  def errsToRes(es: Nel[Err]) = Errors(es)

  def throwableToErr(t: Throwable) = Serious(t.toString)

  def logErr(e: Err) = LocEnUS logDataErr e

  def useIdS(u: UseS) = u.id

  object CyByMonadIO {
    private val format = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss")

    def apply[C,S](
      logger: C ⇒ Logger[IO],
      time  : C ⇒ Timestamp,
      user  : C ⇒ Option[Alias],
    ): CyByMonadIO[C,S] = new CyByMonadIO[C,S]{
      def handleErr(c: C) = _ ⇒ ioUnit

      def handleLog(c: C) = l ⇒ {
        lazy val aliasStr = user(c).fold("")(a ⇒ s" ${a} ")
        lazy val totalMsg = s"${format format time(c)}${aliasStr} ${l.message}"

        logger(c).log(totalMsg, l.lvl)
      }
    }

    def env[S]: CyByMonadIO[Env[St],S] =
      apply[Env[St],S](_.logger, _.timestamp, _ ⇒ None)

    def editEnv[S]: CyByMonadIO[EditEnv,S] =
      apply[EditEnv,S](_.env.logger, _.env.timestamp, e ⇒ some(e.u.alias.v))

    def authEnv[S]: CyByMonadIO[LoggedInEnv,S] =
      apply[LoggedInEnv,S](_.env.logger, _.env.timestamp, e ⇒ some(e.u.alias.v))
  }
}
