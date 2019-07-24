/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui

/**
  * Enum type representing query shortcuts in the navigator.
  */
sealed abstract class NavSearchMode(
  override val toString: String
){
  def c: CyByClass = IconType.NavSearch(this).c
  def cd: CyByClass = IconType.NavSearch(this).cd
}

/**
  * Used to list bio entries linked to a given item in the
  * navigator.
  */
case object NSBio extends NavSearchMode("bio")

/**
  * Used to list containers linked to a given item in the
  * navigator.
  */
case object NSConAll extends NavSearchMode("conall")

/**
  * Used to list non-empty containers linked to a given item in the
  * navigator.
  */
case object NSConNonEmpty extends NavSearchMode("connonempty")

/**
  * Used to list substances linked to a given item in the
  * navigator.
  */
case object NSSub extends NavSearchMode("sub")

object NavSearchMode extends EnumHelper[NavSearchMode]{
  val name = "cyby.ui.NavSearchMode"
  def unapply(s: String): Option[NavSearchMode] = readI read s
  def encode(n: NavSearchMode) = Nel of n.toString
  def values = Nel.of(NSBio,NSConAll,NSConNonEmpty,NSSub)
}

