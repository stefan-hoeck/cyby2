/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package msf
package js

import cats.Monoid
import scala.util.control.NonFatal

trait util {
  type Handler[A] = A ⇒ IO[Unit]

  def tryO[A](a: ⇒ A): Option[A] = try {
    a match {
      case null ⇒ None
      case a2   ⇒ Some(a2)
    }
  } catch { case e@NonFatal(x) ⇒ None }

  def tryu(f: ⇒ Unit): Unit = try {
    f
  } catch { case e@NonFatal(x) ⇒ unit }

  def delayTry[A:Monoid](a: ⇒ A): IO[A] =
    delay(tryO(a) getOrElse Monoid[A].empty)
}
