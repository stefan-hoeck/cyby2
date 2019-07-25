/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package msf
package js

import cats.Show

import org.scalajs.dom

sealed trait UIEvent

object UIEvent {
  implicit val showI: Show[UIEvent] = Show.fromToString

  case class Blur(e: dom.raw.FocusEvent) extends UIEvent
  case class Change(e: dom.raw.Event) extends UIEvent
  case class Click(e: dom.raw.MouseEvent) extends UIEvent
  case class DblClick(e: dom.raw.MouseEvent) extends UIEvent
  case class DragEnd(e: dom.raw.DragEvent) extends UIEvent
  case class DragEnter(e: dom.raw.DragEvent) extends UIEvent
  case class Drag(e: dom.raw.DragEvent) extends UIEvent
  case class DragLeave(e: dom.raw.DragEvent) extends UIEvent
  case class DragOver(e: dom.raw.DragEvent) extends UIEvent
  case class DragStart(e: dom.raw.DragEvent) extends UIEvent
  case class Drop(e: dom.raw.DragEvent) extends UIEvent
  case class Focus(e: dom.raw.FocusEvent) extends UIEvent
  case class FocusIn(e: dom.raw.FocusEvent) extends UIEvent
  case class FocusOut(e: dom.raw.FocusEvent) extends UIEvent
  case class HashChange(e: dom.raw.HashChangeEvent) extends UIEvent
  case class Input(e: dom.raw.Event) extends UIEvent
  case class KeyDown(e: dom.raw.KeyboardEvent) extends UIEvent
  case class KeyPress(e: dom.raw.KeyboardEvent) extends UIEvent
  case class KeyUp(e: dom.raw.KeyboardEvent) extends UIEvent
  case class MouseDown(e: dom.raw.MouseEvent) extends UIEvent
  case class MouseMove(e: dom.raw.MouseEvent) extends UIEvent
  case class MouseOut(e: dom.raw.MouseEvent) extends UIEvent
  case class MouseOver(e: dom.raw.MouseEvent) extends UIEvent
  case class MouseUp(e: dom.raw.MouseEvent) extends UIEvent
  case class Scroll(e: dom.raw.UIEvent) extends UIEvent
  case object Init extends UIEvent
}

// vim: set ts=2 sw=2 et:
