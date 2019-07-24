/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat
package format

/**
  * Sum type representing formatting rules (back ground colors)
  * used in table columns
  */
@io.circe.generic.JsonCodec sealed trait Format[A]

case class Gradient[A](pairs: List[(A,Color)], nod: Int) extends Format[A]

object Format {
  def applyNum(d: Double): Format[Double] ⇒ Option[Color] = {
    case Gradient(ps,_) ⇒ appGrad(d, ps.toList)
  }

  def appGrad(d: Double, ps: List[(Double,Color)]): Option[Color] = ps match {
    case Nil        ⇒ None
    case (_,c)::Nil ⇒ Some(c)
    case (v1,c1)::(v2,c2)::t ⇒ 
      if (d < v1) Some(c1)
      else if (v1 <= d && d <= v2) Some(Color.interpolate((d-v1)/(v2-v1))(c1,c2))
      else appGrad(d, (v2 -> c2)::t)
  }

  def formatNum(d: Double): Format[Double] ⇒ String = {
    case Gradient(_,0) ⇒ "%f" format d
    case Gradient(_,x) ⇒ ("%." + x + "f") format d
  }
}

// vim: set ts=2 sw=2 et:
