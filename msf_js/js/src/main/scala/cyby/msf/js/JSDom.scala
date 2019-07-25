/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package msf
package js

import cats.implicits._

import org.scalajs.dom.{html, document, raw ⇒ hraw}

trait JSDom[F[_],ID,Cls,EV] extends JSHelper[F,ID,Cls,EV] { self ⇒ 
  private def uid[A<:Elem]: A ⇒ Html[Unit] = a ⇒
    unique >>= (i ⇒ local(set.attribute(id := i))(a))

  /**
    * Every element gets a unique id if not overriden by the
    * id attribute
    */
  def elem[A<:Elem](n: String, mods: List[A ⇒ Html[Unit]]): Html[A] = for {
    res <- Html liftIO delay(document.createElement(n).asInstanceOf[A])
    _   <- uid(res)
    _   <- mods traverse_ (_ apply res)
    _   <- append(res)
  } yield res

  def input(t: InputType, mods: List[Input ⇒ Html[Unit]]): Html[Input] =
    elem("input", attrAsMod(typ := t) :: mods)

  def input(t: InputType, mods: Input ⇒ Html[Unit]*): Html[Input] =
    input(t, mods.toList)

  def button(mods: html.Button ⇒ Html[Unit]*): Html[html.Button] = elem("button", mods.toList)

  def checkbox(mods: Input ⇒ Html[Unit]*): Html[Input] = input(InputType.CheckBox, mods.toList)

  def select(mods: Select ⇒ Html[Unit]*): Html[Select] = elem("select", mods.toList)

  def textfield(mods: Input ⇒ Html[Unit]*): Html[Input] = input(InputType.Text, mods.toList)

  def txt(v: String, c: Cls): Html[Input] = textfield(cls := c, value := v)

  def selectEntries(es: List[SelectEntry]): Select ⇒ Html[Unit] =
    within(_)(set innerHtml T.options(es))
    
  def combo(es: List[SelectEntry], c: Cls): Html[Select] =
    select(cls := c, selectEntries(es))

  def txtValidated[A](
    f:   String ⇒ Option[A],
    msg: String ⇒ String,
    c:   Cls,
    v:   String = "",
  ): Html[Signal[Option[A]]] = 
    txt(v, c) map Src.valueValidated[A,Input](f, msg)

  def onChange[A,E:HasID](elem: Html[E])(signal: E ⇒ Signal[Option[A]])
    : Html[Src[Option[A]]] =
    elem map { e ⇒ signal(e) on Src.change(e) }

  def canvas(mods: html.Canvas ⇒ Html[Unit]*): Html[html.Canvas] = elem("canvas", mods.toList)
  def a(mods: Elem ⇒ Html[Unit]*): Html[Elem] = elem("a", mods.toList)
  def aside(mods: Elem ⇒ Html[Unit]*): Html[Elem] = elem("aside", mods.toList)
  def div(mods: Elem ⇒ Html[Unit]*): Html[Elem] = elem("div", mods.toList)
  def footer(mods: Elem ⇒ Html[Unit]*): Html[Elem] = elem("footer", mods.toList)
  def h1(mods: Elem ⇒ Html[Unit]*): Html[Elem] = elem("h1", mods.toList)
  def h2(mods: Elem ⇒ Html[Unit]*): Html[Elem] = elem("h2", mods.toList)
  def h3(mods: Elem ⇒ Html[Unit]*): Html[Elem] = elem("h3", mods.toList)
  def h4(mods: Elem ⇒ Html[Unit]*): Html[Elem] = elem("h4", mods.toList)
  def h5(mods: Elem ⇒ Html[Unit]*): Html[Elem] = elem("h5", mods.toList)
  def h6(mods: Elem ⇒ Html[Unit]*): Html[Elem] = elem("h6", mods.toList)
  def header(mods: Elem ⇒ Html[Unit]*): Html[Elem] = elem("header", mods.toList)
  def img(mods: Elem ⇒ Html[Unit]*): Html[Elem] = elem("img", mods.toList)
  def label(mods: Elem ⇒ Html[Unit]*): Html[Elem] = elem("label", mods.toList)
  def li(mods: Elem ⇒ Html[Unit]*): Html[Elem] = elem("li", mods.toList)
  def main(mods: Elem ⇒ Html[Unit]*): Html[Elem] = elem("main", mods.toList)
  def ol(mods: Elem ⇒ Html[Unit]*): Html[Elem] = elem("ol", mods.toList)
  def option(mods: Elem ⇒ Html[Unit]*): Html[Elem] = elem("option", mods.toList)
  def p(mods: Elem ⇒ Html[Unit]*): Html[Elem] = elem("p", mods.toList)
  def section(mods: Elem ⇒ Html[Unit]*): Html[Elem] = elem("section", mods.toList)
  def span(mods: Elem ⇒ Html[Unit]*): Html[Elem] = elem("span", mods.toList)
  def strong(mods: Elem ⇒ Html[Unit]*): Html[Elem] = elem("strong", mods.toList)
  def table(mods: Elem ⇒ Html[Unit]*): Html[Elem] = elem("table", mods.toList)
  def td(mods: Elem ⇒ Html[Unit]*): Html[Elem] = elem("td", mods.toList)
  def th(mods: Elem ⇒ Html[Unit]*): Html[Elem] = elem("th", mods.toList)
  def tr(mods: Elem ⇒ Html[Unit]*): Html[Elem] = elem("tr", mods.toList)
  def ul(mods: Elem ⇒ Html[Unit]*): Html[Elem] = elem("ul", mods.toList)

  def Aside[A](mods: Elem ⇒ Html[Unit]*)(h: Html[A]): Html[A] = withinH(aside(mods: _*))(h)
  def Div[A](mods: Elem ⇒ Html[Unit]*)(h: Html[A]): Html[A] = withinH(div(mods: _*))(h)
  def Footer[A](mods: Elem ⇒ Html[Unit]*)(h: Html[A]): Html[A] = withinH(footer(mods: _*))(h)
  def H1[A](mods: Elem ⇒ Html[Unit]*)(h: Html[A]): Html[A] = withinH(h1(mods: _*))(h)
  def H2[A](mods: Elem ⇒ Html[Unit]*)(h: Html[A]): Html[A] = withinH(h2(mods: _*))(h)
  def H3[A](mods: Elem ⇒ Html[Unit]*)(h: Html[A]): Html[A] = withinH(h3(mods: _*))(h)
  def H4[A](mods: Elem ⇒ Html[Unit]*)(h: Html[A]): Html[A] = withinH(h4(mods: _*))(h)
  def H5[A](mods: Elem ⇒ Html[Unit]*)(h: Html[A]): Html[A] = withinH(h5(mods: _*))(h)
  def H6[A](mods: Elem ⇒ Html[Unit]*)(h: Html[A]): Html[A] = withinH(h6(mods: _*))(h)
  def Header[A](mods: Elem ⇒ Html[Unit]*)(h: Html[A]): Html[A] = withinH(header(mods: _*))(h)
  def Img[A](mods: Elem ⇒ Html[Unit]*)(h: Html[A]): Html[A] = withinH(img(mods: _*))(h)
  def Label[A](mods: Elem ⇒ Html[Unit]*)(h: Html[A]): Html[A] = withinH(label(mods: _*))(h)
  def Li[A](mods: Elem ⇒ Html[Unit]*)(h: Html[A]): Html[A] = withinH(li(mods: _*))(h)
  def Main[A](mods: Elem ⇒ Html[Unit]*)(h: Html[A]): Html[A] = withinH(main(mods: _*))(h)
  def Ol[A](mods: Elem ⇒ Html[Unit]*)(h: Html[A]): Html[A] = withinH(ol(mods: _*))(h)
  def Option[A](mods: Elem ⇒ Html[Unit]*)(h: Html[A]): Html[A] = withinH(option(mods: _*))(h)
  def P[A](mods: Elem ⇒ Html[Unit]*)(h: Html[A]): Html[A] = withinH(p(mods: _*))(h)
  def Section[A](mods: Elem ⇒ Html[Unit]*)(h: Html[A]): Html[A] = withinH(section(mods: _*))(h)
  def Span[A](mods: Elem ⇒ Html[Unit]*)(h: Html[A]): Html[A] = withinH(span(mods: _*))(h)
  def Strong[A](mods: Elem ⇒ Html[Unit]*)(h: Html[A]): Html[A] = withinH(strong(mods: _*))(h)
  def Table[A](mods: Elem ⇒ Html[Unit]*)(h: Html[A]): Html[A] = withinH(table(mods: _*))(h)
  def Td[A](mods: Elem ⇒ Html[Unit]*)(h: Html[A]): Html[A] = withinH(td(mods: _*))(h)
  def Th[A](mods: Elem ⇒ Html[Unit]*)(h: Html[A]): Html[A] = withinH(th(mods: _*))(h)
  def Tr[A](mods: Elem ⇒ Html[Unit]*)(h: Html[A]): Html[A] = withinH(tr(mods: _*))(h)
  def Ul[A](mods: Elem ⇒ Html[Unit]*)(h: Html[A]): Html[A] = withinH(ul(mods: _*))(h)

  def text(s: String): Html[hraw.Text] = for {
    t <- Html liftIO delay(document createTextNode s)
    _ <- appendText(t)
  } yield t

  implicit def attrAsMod[A<:Elem](a: (String,String))
    : A ⇒ Html[Unit] = local(set attribute a)

  implicit def elemAsMod[A<:Elem](el: Elem)
    : A ⇒ Html[Unit] = local(append(el))(_).void

  implicit def txtAsMod[A<:Elem](t: Html[hraw.Text])
    : A ⇒ Html[Unit] = local(t)(_).void

  implicit def elemHAsMod[A<:Elem,B<:Elem](el: Html[B])
    : A ⇒ Html[Unit] = local(el)(_).void
}

// vim: set ts=2 sw=2 et:
