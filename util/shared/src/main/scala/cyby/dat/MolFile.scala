/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

/**
  * Wrapper type for strings representing molecules in Mol format.
  * Correct formatting of the strings cannot be enfored here. This is
  * mainly used to escape line breaks for storing multiline formats
  * in a single line.
  */
final class MolFile private(val v: String) extends AnyVal {
  override def toString = MolFile escape v
}

object MolFile extends RefinedStringWithLineBreaks[MolFile]("MolFile", _.v){
  def fromString(s: String): MolFile = new MolFile(unescape(s))

  def apply(s: String): Option[MolFile] = Some(fromString(s))

  val default = new MolFile("")
}
