/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package msf
package js

import cats.Show

import org.scalajs.dom.raw

sealed trait UIEvent

object UIEvent {
  implicit val showI: Show[UIEvent] = Show.fromToString

  case class Blur(e: raw.FocusEvent) extends UIEvent
  case class Change(e: raw.Event) extends UIEvent
  case class Click(e: raw.MouseEvent) extends UIEvent
  case class DblClick(e: raw.MouseEvent) extends UIEvent
  case class DragEnd(e: raw.DragEvent) extends UIEvent
  case class DragEnter(e: raw.DragEvent) extends UIEvent
  case class Drag(e: raw.DragEvent) extends UIEvent
  case class DragLeave(e: raw.DragEvent) extends UIEvent
  case class DragOver(e: raw.DragEvent) extends UIEvent
  case class DragStart(e: raw.DragEvent) extends UIEvent
  case class Drop(e: raw.DragEvent) extends UIEvent
  case class Focus(e: raw.FocusEvent) extends UIEvent
  case class FocusIn(e: raw.FocusEvent) extends UIEvent
  case class FocusOut(e: raw.FocusEvent) extends UIEvent
  case class HashChange(e: raw.HashChangeEvent) extends UIEvent
  case class Input(e: raw.Event) extends UIEvent
  case class KeyDown(e: raw.KeyboardEvent) extends UIEvent
  case class KeyPress(e: raw.KeyboardEvent) extends UIEvent
  case class KeyUp(e: raw.KeyboardEvent) extends UIEvent
  case class MouseDown(e: raw.MouseEvent) extends UIEvent
  case class MouseMove(e: raw.MouseEvent) extends UIEvent
  case class MouseOut(e: raw.MouseEvent) extends UIEvent
  case class MouseOver(e: raw.MouseEvent) extends UIEvent
  case class MouseUp(e: raw.MouseEvent) extends UIEvent
  case class Scroll(e: raw.UIEvent) extends UIEvent
  case object Init extends UIEvent
}

// vim: set ts=2 sw=2 et:
