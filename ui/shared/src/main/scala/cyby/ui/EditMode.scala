/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui

import cyby.msf.js.{Node, nodes}

/**
  * Enum declaring whether the UI is in editing mode or not.
  */
sealed trait EditMode{
  val isEditing: Boolean
  def ifEditing(s: ⇒ Node): Node = if (isEditing) s else nodes()
}

case object Editing extends EditMode{ val isEditing = true }

case object NotEditing extends EditMode{ val isEditing = false }


/**
  * Sum type representing the actual editing state.
  * This is used to adjust the UI if a user starts or ends
  * editing data entries.
  */
sealed trait EditSt {
  def c: EditSt = this
  def mode: EditMode
  def hasChanged: Boolean = this match {
    case StartEditing ⇒ true
    case StopEditing  ⇒ true
    case NoChange(_)  ⇒ false
  }
}

case object StartEditing extends EditSt{ val mode = Editing }

case object StopEditing extends EditSt{ val mode = NotEditing }

case class NoChange(mode: EditMode) extends EditSt

object EditSt {
  /**
    * The actual edit state changes whenever certain events
    * (like clicking on the master edit button) occur.
    *
    * @param cmd: the actual edit state
    * @param act: None if no interesting event occured, Some(())
    *             if an event (like clicking a button) to change
    *             the editing state occured.
    * @return   : The adjusted editing state.
    */
  def accum(cmd: EditSt, act: Option[Unit]): EditSt = (cmd,act) match {
    case (n@NoChange(_),None) ⇒ n
    case (cmd,          None) ⇒ NoChange(cmd.mode)
    case (cmd,          Some(())) ⇒ cmd.mode match {
      case Editing    ⇒ StopEditing
      case NotEditing ⇒ StartEditing
    }
  }
}

// vim: set ts=2 sw=2 et:
