/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

import cats.instances.all._

/**
  * Refined strings representing CAS numbers. Only properly formated
  * CAS numbers or an empty string are accepted.
  */
final class CasNr private(val v: String) extends AnyVal {
  override def toString = v
}

object CasNr extends RefinedDef[String,CasNr]("CasNr", _.v){
  val CasNrR = """(\d{1,7})-(\d\d)-(\d)""".r
  val default = unsafe("")

  def apply(v: String): Option[CasNr] =
    if (isCasNr(v)) Some(new CasNr(v)) else None

  private def isCasNr(s: String): Boolean = s match {
    case ""                ⇒ true
    case x@CasNrR(a, b, c) ⇒ calc(a.toLong, b.toLong) == c.toLong
    case _                 ⇒ false
  }

  def calc(a: Long, b: Long): Long = {
    def sum (rem: Long, fac: Long, res: Long): Long = rem match {
      case 0L ⇒ res
      case x  ⇒ sum(x / 10L, fac + 1, res + fac * (x % 10))
    }

    sum(a * 100L + b, 1, 0L) % 10L
  }
}

// vim: set ts=2 sw=2 et:
