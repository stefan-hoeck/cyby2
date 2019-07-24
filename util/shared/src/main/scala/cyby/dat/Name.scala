/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

import cats.instances.all._

/**
  * Similar to Plain strings, this represents strings without
  * control characters. In addition, names must not be empty.
  */
final class Name private(val v: String) extends AnyVal {
  override def toString = v
}

object Name extends Refined[String,Name]("Name", _.v){
  def apply(s: String): Option[Name] =
    Plain(s) flatMap (p ⇒ if (p.v.nonEmpty) Some(new Name(p.v)) else None)
}

// vim: set ts=2 sw=2 et:
