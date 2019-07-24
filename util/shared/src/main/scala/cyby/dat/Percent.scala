/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

import cats.instances.all._

/**
  * Refined Double in the range [0,100] representing percentages.
  */
final class Percent private(val v: Double) extends AnyVal {
  override def toString = v.toString
}

object Percent extends RefinedDef[Double,Percent]("Percent", _.v){
  val default: Percent = unsafe(0D)

  def apply(v: Double): Option[Percent] =
    if (0D <= v && v <= 100D) Some(new Percent(v)) else None
}

// vim: set ts=2 sw=2 et:
