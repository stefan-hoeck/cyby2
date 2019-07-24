/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

case class ToMol[A](
  id:  A ⇒ Long,
  mol: A ⇒ Option[Mol],
) {
  def svg(a: A): String = mol(a).fold("")(_.svg.v)
}

object ToMol {
  def apply[A](implicit A: ToMol[A]): ToMol[A] = A
}

// vim: set ts=2 sw=2 et:
