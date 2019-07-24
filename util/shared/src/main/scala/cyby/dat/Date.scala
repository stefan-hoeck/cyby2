/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

import cats.instances.all._

/**
  * Data wrapper for date values encoded as milliseconds since 1.1.1970.
  */
final class Date private(val v: Long) extends AnyVal {
  override def toString = v.toString
}

object Date extends Refined[Long,Date]("Date", _.v){
  def apply(v: Long): Option[Date] = some(mk(v))

  def mk(v: Long): Date = new Date(v)
}

// vim: set ts=2 sw=2 et:

