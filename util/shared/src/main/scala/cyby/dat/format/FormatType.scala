/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat
package format

/**
  * Types of formatting rules supported in table columns
  */
sealed trait FormatType

case object GradientFormat extends FormatType

case object NoFormat extends FormatType

object FormatType extends EnumHelper[FormatType] {
  val name = "cyby.dat.format.FormatType"
  val values = Nel.of(GradientFormat, NoFormat)
  def encode(f: FormatType) = lowerHeadEncode(f)
}

// vim: set ts=2 sw=2 et:
