/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

import cats.instances.all._

/**
  * Refined strings for representing user aliases allowing
  * only a very limited subset of possible characters. Use
  * Plain or Name or create a similar refinement type
  * if you need greater flexibility.
  */
final class Alias private(val v: String) extends AnyVal {
  def name: Name = Name unsafe v
  override def toString = v
}

object Alias extends Refined[String,Alias]("Alias", _.v){
  val MaxLength = 50

  def validChar(c: Char) =
    ('A' <= c && c <= 'Z') ||
    ('a' <= c && c <= 'z') ||
    ('0' <= c && c <= '9') ||
    (c == '_')
  
  def apply(s: String): Option[Alias] = if (
    s.length <= MaxLength &&
    s.nonEmpty            &&
    s.forall(validChar)
  ) some(new Alias(s)) else none
}
