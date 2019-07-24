/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat


/**
  * Wrapper type for non-empty lists of values from which
  * different types of statistics are calculated and memoized
  * on demand.
  */
@io.circe.generic.JsonCodec case class Stats(values: Nel[Double]) {
  lazy val minimum: Double = values.head
  lazy val maximum: Double = values.last
  lazy val sum: Double     = values.toList.sum
  lazy val mean: Double    = sum / values.length
  lazy val median: Double  = Stats median values
}

object Stats {
  private def median(vs: Nel[Double]): Double = {
    val vsl = vs.toList
    val l = vsl.length
    val l2 = l/2

    if (even(l)) (vsl(l2-1) + vsl(l2))/2
    else vsl(l2)
  }

  private def even(n: Int): Boolean = n % 2 == 0
}

// vim: set ts=2 sw=2 et:
