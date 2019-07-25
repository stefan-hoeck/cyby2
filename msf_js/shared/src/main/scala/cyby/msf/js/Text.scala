/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package msf
package js

sealed trait Node { def n: Node = this }

case class El(tag: String, atts: Seq[Attribute], elems: Seq[Node]) extends Node {
  override def toString: String = {
    val astr = dispAttributes(atts)
    val estr = elems mkString ""

    if (astr.isEmpty) s"<${tag}>${estr}</${tag}>"
    else s"<${tag} ${astr}>${estr}</${tag}>"
  }
}

case class Raw(override val toString: String) extends Node

case class Nodes(ns: Seq[Node]) extends Node {
  override def toString = ns mkString ""
}

case class Text(txt: String) extends Node {
  override def toString = escape(txt)
}

trait TextHelper[ID,Cls] extends Attributes[ID,Cls] {

  def button(as: Attribute*)(es: Node*): Node = El("button", as, es)
  def canvas(as: Attribute*)(es: Node*): Node = El("canvas", as, es)
  def select(as: Attribute*)(es: Node*): Node = El("select", as, es)
  def datalist(as: Attribute*)(es: Node*): Node = El("datalist", as, es)
  def a(as: Attribute*)(es: Node*): Node = El("a", as, es)
  def aside(as: Attribute*)(es: Node*): Node = El("aside", as, es)
  def div(as: Attribute*)(es: Node*): Node = El("div", as, es)
  def footer(as: Attribute*)(es: Node*): Node = El("footer", as, es)
  def h1(as: Attribute*)(es: Node*): Node = El("h1", as, es)
  def h2(as: Attribute*)(es: Node*): Node = El("h2", as, es)
  def h3(as: Attribute*)(es: Node*): Node = El("h3", as, es)
  def h4(as: Attribute*)(es: Node*): Node = El("h4", as, es)
  def h5(as: Attribute*)(es: Node*): Node = El("h5", as, es)
  def h6(as: Attribute*)(es: Node*): Node = El("h6", as, es)
  def header(as: Attribute*)(es: Node*): Node = El("header", as, es)
  def img(as: Attribute*)(es: Node*): Node = El("img", as, es)
  def label(as: Attribute*)(es: Node*): Node = El("label", as, es)
  def li(as: Attribute*)(es: Node*): Node = El("li", as, es)
  def main(as: Attribute*)(es: Node*): Node = El("main", as, es)
  def ol(as: Attribute*)(es: Node*): Node = El("ol", as, es)
  def optgroup(as: Attribute*)(es: Node*): Node = El("optgroup", as, es)
  def option(as: Attribute*)(es: Node*): Node = El("option", as, es)
  def p(as: Attribute*)(es: Node*): Node = El("p", as, es)
  def section(as: Attribute*)(es: Node*): Node = El("section", as, es)
  def span(as: Attribute*)(es: Node*): Node = El("span", as, es)
  def strong(as: Attribute*)(es: Node*): Node = El("strong", as, es)
  def table(as: Attribute*)(es: Node*): Node = El("table", as, es)
  def td(as: Attribute*)(es: Node*): Node = El("td", as, es)
  def th(as: Attribute*)(es: Node*): Node = El("th", as, es)
  def tr(as: Attribute*)(es: Node*): Node = El("tr", as, es)
  def input(t: InputType, as: Attribute*): Node = El("input", (typ := t) +: as, Nil)
  def ul(as: Attribute*)(es: Node*): Node = El("ul", as, es)
  def text(s: String): Node = Text(s)

  def options(vs: List[SelectEntry]): Node = Nodes(vs map {
    case SelectEntry(Some(v),s,t)  ⇒ option(value := v, selected := s)(text(t))
    case SelectEntry(None,   _,t)  ⇒ optgroup(optlabel := t)()
  })
}

// vim: set ts=2 sw=2 et:
