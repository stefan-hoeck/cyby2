/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui
package example

import scala.scalajs.js.annotation.{JSExportTopLevel, JSExport}

import msf.js.runUI

@JSExportTopLevel("CyBy2")
object CyBy {
  type Elem = org.scalajs.dom.html.Element

  @JSExport
  def main(url: String, e: Elem): Unit = mainIO(url, e).value

  def mainIO(url: String, e: Elem): IO[Unit] =
    runUI((hht,hui) ⇒ cats.Monad[IO] pure ExplorerZ(hht, hui, url).behavior)
}

// vim: set ts=2 sw=2 et:
