/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

import cats.instances.all._

/**
  * Refined strings representing the name of a file (not a full path)
  * on disk. Only a small subset of valid characters are accepted.
  */
final class FileName private(val v: String) extends AnyVal {
  override def toString = v
}

object FileName extends Refined[String,FileName]("FileName", _.v){
  final val MaxLength: Int = 200

  def apply(s: String): Option[FileName] = if (
    s.nonEmpty            &&
    s.length <= MaxLength &&
    s.forall(validChar)
  ) Some(new FileName(s)) else None

  final val validChar: Set[Char] =
    Set('.', '_', '-') ++
    ('a' to 'z').toSet ++
    ('A' to 'Z').toSet ++
    ('0' to '9').toSet

  def sanitizePath(s: String): FileName = unsafe(
    s flatMap {
      case ' '               ⇒ "_"
      case 'ä'               ⇒ "ae"
      case 'ü'               ⇒ "ue"
      case 'ö'               ⇒ "oe"
      case c if validChar(c) ⇒ s"$c"
      case _                 ⇒ ""
    } take MaxLength
  )
}

// vim: set ts=2 sw=2 et:
