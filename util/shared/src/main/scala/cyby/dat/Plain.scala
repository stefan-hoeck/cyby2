/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

import cats.instances.all._

/**
  * Refined String of up to 2000 characters containing no control
  * characters.
  */
final class Plain private(val v: String) extends AnyVal {
  override def toString = v
}

object Plain extends RefinedDef[String,Plain]("Plain", _.v){
  val MaxLength = 2000

  val default: Plain = unsafe("")

  def apply(s: String): Option[Plain] =
    if (s.exists(_.isControl) || s.size > MaxLength) None
    else Some(new Plain(s))
}

// vim: set ts=2 sw=2 et:
