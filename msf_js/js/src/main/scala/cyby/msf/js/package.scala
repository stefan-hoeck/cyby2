/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package msf

import org.scalajs.dom.{document, XMLHttpRequest, DOMList, window}

package object js extends cyby.msf.js.util with cyby.msf.js.shared {
  type MkUIHttpSF = (Handler[HttpRequest], Handler[UIEvent]) ⇒
                     IO[SF[IO,Either[HttpEvent,UIEvent],Unit]]

  type MkUIHttpEF = (Handler[HttpRequest], Handler[UIEvent]) ⇒
                     IO[EF[IO,Either[HttpEvent,UIEvent],Unit]]

  def domListToList[T](dl: DOMList[T]): List[T] =
    (0 until dl.length).toList map dl.apply
 
  def runUI(mkSF: MkUIHttpSF): IO[Unit] = run(mkSF, List(basic))
 
  def runUIE(mkEF: MkUIHttpEF): IO[Unit] =
    runUI((a,b) ⇒ mkEF(a,b) map (_ hold unit))
 
  def withMouseUI(mkSF: MkUIHttpSF): IO[Unit] =
    run(mkSF, List(basic, mouse))
 
  private[this] def run(
    mkSf: MkUIHttpSF,
    handles: List[(UIEvent ⇒ Unit) ⇒ Unit]
  ): IO[Unit] = delay {
    var sf: SF[IO,Either[HttpEvent,UIEvent],Unit] = null

    def runUI(e: UIEvent): Unit = sf = sf.tail(Right(e)).value

    def runHttp(e: HttpEvent): Unit = sf = sf.tail(Left(e)).value

    val uiHandler: Handler[UIEvent] = e ⇒ delay(runUI(e))

    sf = mkSf(http(runHttp), uiHandler).value
 
    handles foreach { h ⇒ h(runUI) }
 
    runUI(UIEvent.Init)
  }
 
  private[this] val basic: (UIEvent ⇒ Unit) ⇒ Unit = run ⇒ {
    document.onchange   = e ⇒ run(UIEvent.Change(e))
    document.onfocus    = e ⇒ run(UIEvent.Focus(e))
    document.onfocusin  = e ⇒ run(UIEvent.FocusIn(e))
    document.onfocusout = e ⇒ run(UIEvent.FocusOut(e))
    document.ondblclick = e ⇒ run(UIEvent.DblClick(e))
    document.onclick    = e ⇒ run(UIEvent.Click(e))
    document.onkeydown  = e ⇒ run(UIEvent.KeyDown(e))
    document.onkeyup    = e ⇒ run(UIEvent.KeyUp(e))
    document.onkeypress = e ⇒ run(UIEvent.KeyPress(e))
    document.onscroll   = e ⇒ run(UIEvent.Scroll(e))
    document.oninput    = e ⇒ run(UIEvent.Input(e))
    window.onhashchange = e ⇒ run(UIEvent.HashChange(e))
  }
 
  private[this] val mouse: (UIEvent ⇒ Unit) ⇒ Unit = run ⇒ {
    document.onmousedown = e ⇒ run(UIEvent.MouseDown(e))
    document.onmouseup   = e ⇒ run(UIEvent.MouseUp(e))
    document.onmouseover = e ⇒ run(UIEvent.MouseOver(e))
    document.onmouseout  = e ⇒ run(UIEvent.MouseOut(e))
    document.onmousemove = e ⇒ run(UIEvent.MouseMove(e))
  }
 
  private[this] def http(he: HttpEvent ⇒ Unit): Handler[HttpRequest] = {
    case HttpRequest(u, b, t, m, hs) ⇒ delay {
      val xhr = new XMLHttpRequest()
      xhr.open(m.toString, u)
      hs foreach { case (h,v) ⇒ xhr.setRequestHeader(h, v) }
 
      xhr.timeout = t
 
      def cleanup() = {
        xhr.ontimeout = null
        xhr.onload = null
        xhr.onerror = null
      }
 
      xhr.ontimeout = _ ⇒ { he(HttpTimedOut(u)); cleanup() }
      xhr.onload    = _ ⇒ { he(HttpResponseText(xhr.responseText)); cleanup() }
      xhr.onerror   = e ⇒ { he(HttpError(u)); cleanup() }
    
      b.send(xhr)
    }
  }
}
