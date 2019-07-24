/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server

import cats.effect.{IOApp, ExitCode}
import cats.implicits._

import org.http4s.server.blaze._
import org.http4s.implicits._
import org.http4s.server.blaze._


/**
  * Utility trait for the application's main entry point.
  *
  * ip and port of the server as well as its ExecutorService
  * can be overridden.
  */
trait MainEnv {
  protected val ip   : String          = "localhost"

  /**
    * Stream of HTTP services
    */
  protected def services(cs: CoreSettings): IO[HttpService]

  protected val logger: Logger[IO]

  def main(args: Array[String]) = MainInner main args
  
  object MainInner extends IOApp {
    private def serv(dataPath: String): IO[HttpService] =
      services(CoreSettings(contextShift, dataPath))

    override def run(args: List[String]): IO[ExitCode] = args match {
      case port::dir::Nil ⇒ Read[Int] read port match {
        case Some(p) ⇒ serv(dir) flatMap { s ⇒
          logger(Log(Info, s"services ready on port ${port}")) *>
          BlazeServerBuilder[IO]
            .bindHttp(p, ip)
            .withHttpApp(s.orNotFound)
            .resource
            .use(_ ⇒ cats.effect.IO.never)
            .as(ExitCode.Success)
        }
        case None ⇒ err
      }
      case _ ⇒ err
    }
  }

  def err: IO[ExitCode] =
    delay(println("Usage: java -jar server.jar portnr /path/to/data/dir")).
      as(ExitCode.Error)
}

// vim: set ts=2 sw=2 et:

