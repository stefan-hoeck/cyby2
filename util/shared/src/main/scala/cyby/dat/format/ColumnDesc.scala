/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat
package format

import cats.Eq

/**
  * Sum type used for describing columns in the UI.
  *
  * From this information the CSS of a column is derived as well
  * as whether a table can be sorted by this column and
  * what type of conditional formatting to use for this column.
  */
sealed trait ColumnDesc {
  def sortable: Boolean

  def formattable: Boolean = this match {
    case StatsDesc                     ⇒ true
    case StrDesc(_, GradientFormat, _) ⇒ true
    case _                             ⇒ false
  }
}

case object StructDesc extends ColumnDesc { def sortable = false }

case class StrDesc(
  name:     String,
  format:   FormatType,
  sortable: Boolean
) extends ColumnDesc

case object StatsDesc extends ColumnDesc { def sortable = true }

case object NoDesc extends ColumnDesc { def sortable = false }

object ColumnDesc {
  implicit lazy val eqI: Eq[ColumnDesc] = Eq.fromUniversalEquals
}

// vim: set ts=2 sw=2 et:

