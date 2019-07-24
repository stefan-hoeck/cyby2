/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

/**
  * Wrapper type for Svg values with a similar use case and behavior
  * as MolFile.
  */
final class Svg private(val v: String) extends AnyVal {
  override def toString = Svg escape v
}

object Svg extends RefinedStringWithLineBreaks[Svg]("Svg", _.v){
  def fromString(s: String): Svg = new Svg(s)

  def apply(s: String): Option[Svg] = Some(fromString(s))

  val default = new Svg("")
}

