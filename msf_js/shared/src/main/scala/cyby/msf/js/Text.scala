/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package msf
package js

trait Text[ID,Cls] extends Attributes[ID,Cls] {
  def elem(n: String, atts: Seq[Attribute], elems: Seq[String]): String = {
    val astr = atts collect {
                 case (a,v) if v.nonEmpty ⇒ s"""${a}="${v}""""
               } mkString " "
    val estr = elems mkString ""

    if (astr.isEmpty) s"<${n}>${estr}</${n}>"
    else s"<${n} ${astr}>${estr}</${n}>"
  }

  def button(as: Attribute*)(es: String*): String = elem("button", as, es)
  def canvas(as: Attribute*)(es: String*): String = elem("canvas", as, es)
  def select(as: Attribute*)(es: String*): String = elem("select", as, es)
  def datalist(as: Attribute*)(es: String*): String = elem("datalist", as, es)
  def a(as: Attribute*)(es: String*): String = elem("a", as, es)
  def aside(as: Attribute*)(es: String*): String = elem("aside", as, es)
  def div(as: Attribute*)(es: String*): String = elem("div", as, es)
  def footer(as: Attribute*)(es: String*): String = elem("footer", as, es)
  def h1(as: Attribute*)(es: String*): String = elem("h1", as, es)
  def h2(as: Attribute*)(es: String*): String = elem("h2", as, es)
  def h3(as: Attribute*)(es: String*): String = elem("h3", as, es)
  def h4(as: Attribute*)(es: String*): String = elem("h4", as, es)
  def h5(as: Attribute*)(es: String*): String = elem("h5", as, es)
  def h6(as: Attribute*)(es: String*): String = elem("h6", as, es)
  def header(as: Attribute*)(es: String*): String = elem("header", as, es)
  def img(as: Attribute*)(es: String*): String = elem("img", as, es)
  def label(as: Attribute*)(es: String*): String = elem("label", as, es)
  def li(as: Attribute*)(es: String*): String = elem("li", as, es)
  def main(as: Attribute*)(es: String*): String = elem("main", as, es)
  def ol(as: Attribute*)(es: String*): String = elem("ol", as, es)
  def optgroup(as: Attribute*)(es: String*): String = elem("optgroup", as, es)
  def option(as: Attribute*)(es: String*): String = elem("option", as, es)
  def p(as: Attribute*)(es: String*): String = elem("p", as, es)
  def section(as: Attribute*)(es: String*): String = elem("section", as, es)
  def span(as: Attribute*)(es: String*): String = elem("span", as, es)
  def strong(as: Attribute*)(es: String*): String = elem("strong", as, es)
  def table(as: Attribute*)(es: String*): String = elem("table", as, es)
  def td(as: Attribute*)(es: String*): String = elem("td", as, es)
  def th(as: Attribute*)(es: String*): String = elem("th", as, es)
  def tr(as: Attribute*)(es: String*): String = elem("tr", as, es)
  def input(t: InputType, as: Attribute*): String = elem("input", (typ := t) +: as, Nil)
  def ul(as: Attribute*)(es: String*): String = elem("ul", as, es)
  def text(s: String): String = escape(s)

  def options(vs: List[SelectEntry]): String = vs map {
    case SelectEntry(Some(v),s,t)  ⇒ option(value := v, selected := s)(text(t))
    case SelectEntry(None,   _,t)  ⇒ optgroup(optlabel := text(t))()
  } mkString ""
  
  def escape(text: String): String = text flatMap {
    case '<'          ⇒ "&lt;"
    case '>'          ⇒ "&gt;"
    case '&'          ⇒ "&amp;"
    case '"'          ⇒ "&quot;"
    case '\n'         ⇒ "\n"
    case '\r'         ⇒ "\r"
    case '\t'         ⇒ "\t"
    case c if c < ' ' ⇒ ""
    case c            ⇒ s"$c"
  }

//  /**
//    * Code to escape text HTML nodes. Taken from scala.xml
//    */
//  def escape(text: String, s: StringBuilder) = {
//    // Implemented per XML spec:
//    // http://www.w3.org/International/questions/qa-controls
//    // imperative code 3x-4x faster than current implementation
//    // dpp (David Pollak) 2010/02/03
//    val len = text.length
//    var pos = 0
//
//    while (pos < len) {
//      text.charAt(pos) match {
//        case '<' ⇒ s.append("&lt;")
//        case '>' ⇒ s.append("&gt;")
//        case '&' ⇒ s.append("&amp;")
//        case '"' ⇒ s.append("&quot;")
//        case '\n' ⇒ s.append('\n')
//        case '\r' ⇒ s.append('\r')
//        case '\t' ⇒ s.append('\t')
//        case c if c < ' ' ⇒
//        case c ⇒ s.append(c)
//      }
//      pos += 1
//    }
//  }
}

// vim: set ts=2 sw=2 et:
