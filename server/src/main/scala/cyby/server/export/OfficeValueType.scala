/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package export

import cyby.dat.{Date, TimeStamp, Link, Links}

sealed trait OfficeValueType {
  def valueType: String
  def valueTag: String
  final override def toString = valueType

  def styleName: Option[String] = None

  def wrap(vu: String): String = {
    val v = OfficeValueType escape vu
    val style = styleName.fold("")(n ⇒ s"""table:style-name="${n}" """)

    s"""<table:table-cell office:value-type="${valueType}" ${style} calcext:value-type="${valueType}" office:${valueTag}="${v}"><text:p>${v}</text:p></table:table-cell>"""
  }
}

case object OString extends OfficeValueType{
  val valueType = "string"
  val valueTag = "string-value"
}

case object OFloat extends OfficeValueType{
  val valueType = "float"
  val valueTag = "value"
}

case object OBool extends OfficeValueType{
  val valueType = "boolean"
  val valueTag = "boolean-value"

  override def styleName: Option[String] = some("ce1")
}

case object OTime extends OfficeValueType{
  val valueType = "time"
  val valueTag = "time-value"
}

case object ODate extends OfficeValueType{
  val valueType = "date"
  val valueTag = "date-value"

  override def styleName: Option[String] = some("ce2")
}

object OfficeValueType {
  val empty = OString wrap ""

  def fOpt[A](v: Option[A])(f: A ⇒ String): String = v.fold(empty)(f)

  def fString(s: String): String = OString wrap s

  def fInt(i: Int): String = OFloat wrap i.toString

  def fLong(l: Long): String = OFloat wrap l.toString

  def fDouble(d: Double): String = OFloat wrap d.toString

  def fDate(n: Date): String = ODate wrap date(n.v)

  def fTimeStamp(n: TimeStamp): String = ODate wrap date(n.v)

  def fBool(b: Boolean): String = OBool wrap b.toString

  def link[A](l: Link[A]): String = fString(l._2.v)

  def links[A](ls: Links[A]): String = fString(ls map (_._2.v) mkString ", ")

  def linksNel[A](ls: Nel[Link[A]]): String = links(ls.toList)

  def created(ts: TimeStamp): String = fTimeStamp(ts)

  def escape(text: String): String = text flatMap {
    case '<'          ⇒ "&lt;"
    case '>'          ⇒ "&gt;"
    case '&'          ⇒ "&amp;"
    case '"'          ⇒ "&quot;"
    case '\n'         ⇒ ""
    case '\r'         ⇒ ""
    case '\t'         ⇒ ""
    case c if c < ' ' ⇒ ""
    case c            ⇒ s"$c"
  }

  def odfRow(ls: List[List[String]]): List[String] =
    ls map { fs ⇒ tableRow(fs mkString "") }

  def tableRow(s: String): String = s"""<table:table-row>${s}</table:table-row>"""

  lazy val prefix = """<?xml version="1.0" encoding="UTF-8"?><office:document-content xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0" xmlns:style="urn:oasis:names:tc:opendocument:xmlns:style:1.0" xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0" xmlns:table="urn:oasis:names:tc:opendocument:xmlns:table:1.0" xmlns:draw="urn:oasis:names:tc:opendocument:xmlns:drawing:1.0" xmlns:fo="urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:meta="urn:oasis:names:tc:opendocument:xmlns:meta:1.0" xmlns:number="urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0" xmlns:presentation="urn:oasis:names:tc:opendocument:xmlns:presentation:1.0" xmlns:svg="urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0" xmlns:chart="urn:oasis:names:tc:opendocument:xmlns:chart:1.0" xmlns:dr3d="urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0" xmlns:math="http://www.w3.org/1998/Math/MathML" xmlns:form="urn:oasis:names:tc:opendocument:xmlns:form:1.0" xmlns:script="urn:oasis:names:tc:opendocument:xmlns:script:1.0" xmlns:ooo="http://openoffice.org/2004/office" xmlns:ooow="http://openoffice.org/2004/writer" xmlns:oooc="http://openoffice.org/2004/calc" xmlns:dom="http://www.w3.org/2001/xml-events" xmlns:xforms="http://www.w3.org/2002/xforms" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:rpt="http://openoffice.org/2005/report" xmlns:of="urn:oasis:names:tc:opendocument:xmlns:of:1.2" xmlns:xhtml="http://www.w3.org/1999/xhtml" xmlns:grddl="http://www.w3.org/2003/g/data-view#" xmlns:tableooo="http://openoffice.org/2009/table" xmlns:drawooo="http://openoffice.org/2010/draw" xmlns:calcext="urn:org:documentfoundation:names:experimental:calc:xmlns:calcext:1.0" xmlns:loext="urn:org:documentfoundation:names:experimental:office:xmlns:loext:1.0" xmlns:field="urn:openoffice:names:experimental:ooo-ms-interop:xmlns:field:1.0" xmlns:formx="urn:openoffice:names:experimental:ooxml-odf-interop:xmlns:form:1.0" xmlns:css3t="http://www.w3.org/TR/css3-text/" office:version="1.2"><office:automatic-styles><style:style style:name="co1" style:family="table-column"><style:table-column-properties fo:break-before="auto" style:column-width="64.01pt"/></style:style><style:style style:name="ro1" style:family="table-row"><style:table-row-properties style:row-height="12.81pt" fo:break-before="auto" style:use-optimal-row-height="true"/></style:style><style:style style:name="ta1" style:family="table" style:master-page-name="Default"><style:table-properties table:display="true" style:writing-mode="lr-tb"/></style:style><number:date-style style:name="N84"><number:year number:style="long"/><number:text>-</number:text><number:month number:style="long"/><number:text>-</number:text><number:day number:style="long"/></number:date-style><style:style style:name="ce2" style:family="table-cell" style:parent-style-name="Default" style:data-style-name="N84"/><style:style style:name="ce3" style:family="table-cell" style:parent-style-name="Default" style:data-style-name="N84"/></office:automatic-styles><office:body><office:spreadsheet><table:table table:name="Sheet1" table:style-name="ta1"><table:table-column table:style-name="co1" table:number-columns-repeated="3" table:default-cell-style-name="Default"/><table:table-column table:style-name="co1" table:default-cell-style-name="ce3"/>"""
 //<office:automatic-styles><number:date-style style:name="N84"><number:year number:style="long"/><number:text>-</number:text><number:month number:style="long"/><number:text>-</number:text><number:day number:style="long"/></number:date-style><number:boolean-style style:name="N99"><number:boolean/></number:boolean-style><style:style style:name="ce1" style:family="table-cell" style:data-style-name="N99"/><style:style style:name="ce2" style:family="table-cell" style:data-style-name="N84"/></office:automatic-styles>
  lazy val postfix = """</table:table></office:spreadsheet></office:body></office:document-content>"""
}
