/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package msf
package js

import cats.Show

import org.scalajs.dom.{FormData, XMLHttpRequest}

sealed trait HttpEvent

case class HttpError(url: String) extends HttpEvent
case class HttpResponseText(s: String) extends HttpEvent
case class HttpTimedOut(url: String) extends HttpEvent

object HttpEvent {
  implicit val showI: Show[HttpEvent] = Show.fromToString
}

case class HttpRequest(
  url:     String,
  body:    RequestContent = StringContent(""),
  timeout: Long = 10000L,
  method:  HttpMethod = HttpMethod.GET,
  headers: List[(String,String)] = Nil,
)

sealed trait HttpMethod

object HttpMethod {
  case object GET extends HttpMethod
  case object PUT extends HttpMethod
  case object POST extends HttpMethod
  case object DELETE extends HttpMethod
}

sealed trait RequestContent {
  def send(r: XMLHttpRequest): Unit
}

case class StringContent(s: String) extends RequestContent {
  def send(r: XMLHttpRequest) = r.send(s)
}

case class FormContent(f: FormData) extends RequestContent {
  def send(r: XMLHttpRequest) = r.send(f)
}

// vim: set ts=2 sw=2 et:
