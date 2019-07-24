/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat
package example

/**
  * Enum of data types stored in the data tree.
  *
  * These are used in several places: In URLs at the server and
  * client and in drop-down views in the client to offer users
  * a selection of categories to choose from, for instance when
  * exporting data or when defining queries.
  */
sealed abstract class DataType(
  override val toString: String,
  val inSubTable:   Boolean,
  val inStatsTable: Boolean,
  val canExport:    Boolean,
  val canQuery:     Boolean,
) {
  def locName(l: Loc): String = l dataType this
  def locPlural(l: Loc): String = l dataTypePlural this
}

case object BioT   extends DataType("bio",false, false, true, true)
case object ConT   extends DataType("con",false, true, true, true)
case object FilT   extends DataType("fil",false, false, false, false)
case object MetT   extends DataType("met",false, false, false, false)
case object ProT   extends DataType("pro",false, false, false, false)
case object StatsT extends DataType("stats",false, true, true, true)
case object StoT   extends DataType("sto",false, false, false, false)
case object SubT   extends DataType("sub",true, true, true, true)
case object SupT   extends DataType("sup",false, false, false, false)
case object UseT   extends DataType("use",false, false, false, false)

object DataType extends EnumHelper[DataType] {
  def unapply(s: String): Option[DataType] = read[DataType](s)
  val name = "cyby.dat.example.DataType"
  val values = Nel.of(SubT, ConT, StatsT, BioT, FilT, MetT, ProT, StoT, SupT, UseT)
  def encode(t: DataType) = Nel of t.toString
}

