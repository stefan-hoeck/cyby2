/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

import cats.instances.all._

/**
  * Timestamps similar to Date
  */
final class TimeStamp private(val v: Long) extends AnyVal {
  override def toString = v.toString
}

object TimeStamp extends Refined[Long,TimeStamp]("TimeStamp", _.v){
  def apply(v: Long): Option[TimeStamp] = some(mk(v))

  def mk(v: Long): TimeStamp = new TimeStamp(v)
}

// vim: set ts=2 sw=2 et:
