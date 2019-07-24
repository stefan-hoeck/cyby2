/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

object Main extends MainEnv {
  val logLevel: LogLevel = Info

  lazy val logger: Logger[IO] = Env.consoleLogger filtered logLevel

  def services(cs: CoreSettings) = Service(cs).make
}

// vim: set ts=2 sw=2 et:
