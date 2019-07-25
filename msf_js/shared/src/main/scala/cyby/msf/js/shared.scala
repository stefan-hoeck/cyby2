/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package msf
package js

trait shared {
  def nodes(ns: Node*): Node = Nodes(ns)

  def raw(s: String): Node = Raw(s)

  type Attribute = (String,String)

  def dispAttribute(a: Attribute): String = s"""${a._1}="${a._2}""""

  def dispAttributes(as: Seq[Attribute]): String = as collect {
    case p@(a,v) if v.nonEmpty ⇒ dispAttribute(p)
  } mkString " "

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

