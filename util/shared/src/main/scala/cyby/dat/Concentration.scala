/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

import cats.instances.all._

/**
  * Refined Double representing molar concentrations of solutions.
  * Only values in the range [0,1000] are accepted
  */
final class Concentration private(val v: Double) extends AnyVal {
  override def toString = v.toString
}

object Concentration extends RefinedDef[Double,Concentration]("Concentration", _.v){
  val MaxValue: Double = 1000D
  val default: Concentration = unsafe(0D)


  def apply(v: Double): Option[Concentration] =
    if (0D <= v && v <= MaxValue) Some(new Concentration(v)) else None
}

// vim: set ts=2 sw=2 et:
