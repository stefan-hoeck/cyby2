/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/


package cyby
package server
package example

import java.nio.file.Paths

import org.http4s.dsl.io._

import fs2.io

case class Page(coreSettings: CoreSettings) extends ServerEnv with CyByZ {
  import ImplicitContextShift.cs
  val M = CyByMonadIO.env[Unit]

  private def pathStr(p: String) = s"${coreSettings.dataPath}/page/${p}"

  private def path(p: String) = Paths get pathStr(p)

  /**
    * Prepares a response for downloading the file at the given
    * location
    */
  def download(pth: String): M.Prog[Response] = for {
    b <- M pure (new java.io.File(pathStr(pth))).exists
    r <- if (b) M lift Ok(blocking.flatMap{b ⇒ io.file.readAll[IO](path(pth), b, 4096)})
         else   M lift NotFound()
  } yield r
}

