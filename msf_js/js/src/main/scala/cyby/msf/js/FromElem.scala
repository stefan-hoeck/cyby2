/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package msf
package js

import org.scalajs.dom.html

trait FromElem[A] { def apply(e: html.Element): Option[A] }

object FromElem {
  def apply[A](implicit A: FromElem[A]): FromElem[A] = A

  def inst[A](e: html.Element ⇒ Option[A]): FromElem[A] =
    new FromElem[A]{ def apply(x: html.Element) = e(x) }

  implicit val elemI: FromElem[html.Element] = inst(some)

  implicit val inputI: FromElem[html.Input] = inst{
    case i: html.Input  ⇒ Some(i)
    case _              ⇒ None
  }

  implicit val selectI: FromElem[html.Select] = inst{
    case i: html.Select ⇒ Some(i)
    case _              ⇒ None
  }

  implicit val buttonI: FromElem[html.Button] = inst{
    case i: html.Button ⇒ Some(i)
    case _              ⇒ None
  }

  implicit val canvasI: FromElem[html.Canvas] = inst{
    case i: html.Canvas ⇒ Some(i)
    case _              ⇒ None
  }
}

// vim: set ts=2 sw=2 et:
