/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

import cats.instances.all._

/**
  * Refined Double representing density values of liquids and solid.
  * Only "reasonable" values in the range [0,50] are accepted.
  */
final class Density private(val v: Double) extends AnyVal {
  override def toString = v.toString
}

object Density extends RefinedDef[Double,Density]("Density", _.v){
  val MaxValue: Double = 50D
  val default: Density = unsafe(0D)

  def apply(v: Double): Option[Density] =
    if (0D <= v && v <= MaxValue) Some(new Density(v)) else None
}

// vim: set ts=2 sw=2 et:
