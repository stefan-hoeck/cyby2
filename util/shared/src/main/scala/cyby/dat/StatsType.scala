/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

/**
  * Enum of basic statistics supported in CyBy.
  */
sealed abstract class StatsType(
  override val toString: String,
  val get: Stats ⇒ Double
){
  def locName(l: Loc): String = l statsType this
}

case object MeanStat extends StatsType(Mean, _.mean)

case object MedianStat extends StatsType(Median, _.median)

case object MinStat extends StatsType(Min, _.minimum)

case object MaxStat extends StatsType(Max, _.maximum)

object StatsType extends EnumHelper[StatsType] {
  val name = "cyby.dat.StatsType"
  val values = Nel.of(MeanStat, MedianStat, MinStat, MaxStat)
  def encode(s: StatsType) = lowerHeadEncode(s)
}

// vim: set ts=2 sw=2 et:
