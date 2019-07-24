/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

import cats.instances.all._

/**
  * Refined type representing amounts of things like
  * compounds. Only non-negative values are allowed.
  */
final class Amount private(val v: Double) extends AnyVal {
  override def toString = v.toString
}

object Amount extends RefinedDef[Double,Amount]("Amount", _.v){
  val default: Amount = unsafe(0D)

  def apply(v: Double): Option[Amount] =
    if (0D <= v) Some(new Amount(v)) else None
}

// vim: set ts=2 sw=2 et:
