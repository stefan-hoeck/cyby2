/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package export

/**
  * Enum type representing supported export formats.
  */
sealed trait Format

case object Sdf extends Format { override def toString = "sdf" }
case object Txt extends Format { override def toString = "csv" }
case object Odf extends Format { override def toString = "ods" }
  
object Format extends EnumHelper[Format]{
  val name = "cyby.export.Format"
  val values = Nel.of(Sdf, Txt, Odf)
  def encode(f: Format) = lowerHeadEncode(f)

  def fromPath(pth: String): Option[Format] = pth.split("\\.").toList match {
    case Nil ⇒ None
    case ls  ⇒ Read[Format].read(ls.last)
  }
}
