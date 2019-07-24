/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server

import cats.implicits._
import cats.effect.ContextShift

import java.io.OutputStream
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}
import java.text.SimpleDateFormat

import cyby.export.{Format, Sdf, Odf, Txt}

import fs2.Stream
import fs2.io.writeOutputStream
import fs2.text.utf8Encode

import org.http4s.MediaType

package object export {
  def textLines(ls: List[List[String]]): List[String] = ls map { fs ⇒
    val res = fs mkString "\t"
    s"${res}\n"
  }

  val SdfDelimiter = "$$$$"

  def sdfField[F,A](txt: F ⇒ A ⇒ String)(locName: F ⇒ String)
    : F ⇒ A ⇒ String = f ⇒ a ⇒ sdfEntry(locName(f), txt(f)(a))

  def sdfEntry(name: String, v: String): String =
    s"> <${name}>\n${v}\n"

  def sdfBlock(ls: List[List[String]]): List[String] = ls map { fs ⇒
    val res = fs mkString "\n"
    s"${res}\n${SdfDelimiter}\n"
  }


  def optTxt(s: Option[String]): String = s getOrElse ""

  def date(v: Long): String = format format v

  private val format = new SimpleDateFormat("yyyy-MM-dd")

  def mediaType(pth: String) = Format.fromPath(pth) match {
    case Some(Sdf) ⇒  new MediaType("application", "sdf")
    case Some(Txt) ⇒  new MediaType("text", "csv")
    case Some(Odf) ⇒  new MediaType("appliaction", "vnd.oasis.opendocument.spreadsheet")
    case None      ⇒  new MediaType("text", "plain")
  }

  def zipOds(s: IStream[String], fn: Path)(implicit cs: ContextShift[IO]): IO[Unit] = for {
    zos <- delay(new ZipOutputStream(Files newOutputStream fn))
    _   <- zipStrings(Stream emit manifest, "META-INF/manifest.xml", zos, false)
    _   <- zipStrings(Stream emit styles, "styles.xml", zos, false)
    _   <- zipStrings(s, "content.xml", zos, true)
  } yield ()

  def zipStrings(
    s: IStream[String],
    name: String,
    zos: ZipOutputStream,
    close: Boolean
  )(implicit cs: ContextShift[IO]): IO[Unit] =
    copyToZip(s through utf8Encode, name, zos, close)

  private def copyToZip(
    fs:         IStream[Byte],
    fileName:   String,
    zos:        ZipOutputStream,
    close:      Boolean,
  )(implicit cs: ContextShift[IO]): IO[Unit] = blocking.flatMap { b ⇒
    val makeFos = delay{
      zos.putNextEntry(new ZipEntry(fileName))
      zos : OutputStream
    }
    
    fs.through(writeOutputStream(makeFos, b, close))
  }.compile.drain *> delay{ if (!close) zos.closeEntry }

  lazy val manifest: String = """<?xml version="1.0" encoding="UTF-8"?><manifest:manifest xmlns:manifest="urn:oasis:names:tc:opendocument:xmlns:manifest:1.0" manifest:version="1.2" xmlns:loext="urn:org:documentfoundation:names:experimental:office:xmlns:loext:1.0"><manifest:file-entry manifest:full-path="/" manifest:version="1.2" manifest:media-type="application/vnd.oasis.opendocument.spreadsheet"/><manifest:file-entry manifest:full-path="content.xml" manifest:media-type="text/xml"/><manifest:file-entry manifest:full-path="styles.xml" manifest:media-type="text/xml"/></manifest:manifest>"""
  lazy val styles: String = """<?xml version="1.0" encoding="UTF-8"?><office:document-styles xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0" xmlns:style="urn:oasis:names:tc:opendocument:xmlns:style:1.0" xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0" xmlns:table="urn:oasis:names:tc:opendocument:xmlns:table:1.0" xmlns:draw="urn:oasis:names:tc:opendocument:xmlns:drawing:1.0" xmlns:fo="urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:meta="urn:oasis:names:tc:opendocument:xmlns:meta:1.0" xmlns:number="urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0" xmlns:presentation="urn:oasis:names:tc:opendocument:xmlns:presentation:1.0" xmlns:svg="urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0" xmlns:chart="urn:oasis:names:tc:opendocument:xmlns:chart:1.0" xmlns:dr3d="urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0" xmlns:math="http://www.w3.org/1998/Math/MathML" xmlns:form="urn:oasis:names:tc:opendocument:xmlns:form:1.0" xmlns:script="urn:oasis:names:tc:opendocument:xmlns:script:1.0" xmlns:ooo="http://openoffice.org/2004/office" xmlns:ooow="http://openoffice.org/2004/writer" xmlns:oooc="http://openoffice.org/2004/calc" xmlns:dom="http://www.w3.org/2001/xml-events" xmlns:rpt="http://openoffice.org/2005/report" xmlns:of="urn:oasis:names:tc:opendocument:xmlns:of:1.2" xmlns:xhtml="http://www.w3.org/1999/xhtml" xmlns:grddl="http://www.w3.org/2003/g/data-view#" xmlns:tableooo="http://openoffice.org/2009/table" xmlns:drawooo="http://openoffice.org/2010/draw" xmlns:calcext="urn:org:documentfoundation:names:experimental:calc:xmlns:calcext:1.0" xmlns:loext="urn:org:documentfoundation:names:experimental:office:xmlns:loext:1.0" xmlns:field="urn:openoffice:names:experimental:ooo-ms-interop:xmlns:field:1.0" xmlns:css3t="http://www.w3.org/TR/css3-text/" office:version="1.2"><office:font-face-decls><style:font-face style:name="DejaVu Sans" svg:font-family="&apos;DejaVu Sans&apos;" style:font-family-generic="swiss" style:font-pitch="variable"/><style:font-face style:name="DejaVu Sans1" svg:font-family="&apos;DejaVu Sans&apos;" style:font-family-generic="system" style:font-pitch="variable"/></office:font-face-decls><office:styles><style:default-style style:family="table-cell"><style:paragraph-properties style:tab-stop-distance="36pt"/><style:text-properties style:font-name="DejaVu Sans" fo:language="en" fo:country="US" style:font-name-asian="DejaVu Sans1" style:language-asian="zh" style:country-asian="CN" style:font-name-complex="DejaVu Sans1" style:language-complex="hi" style:country-complex="IN"/></style:default-style><number:number-style style:name="N0"><number:number number:min-integer-digits="1"/></number:number-style><style:style style:name="Default" style:family="table-cell"/><style:style style:name="Heading" style:family="table-cell" style:parent-style-name="Default"><style:text-properties fo:color="#000000" fo:font-size="24pt" fo:font-style="normal" fo:font-weight="bold"/></style:style><style:style style:name="Heading_20_1" style:display-name="Heading 1" style:family="table-cell" style:parent-style-name="Heading"><style:text-properties fo:color="#000000" fo:font-size="18pt" fo:font-style="normal" fo:font-weight="normal"/></style:style><style:style style:name="Heading_20_2" style:display-name="Heading 2" style:family="table-cell" style:parent-style-name="Heading"><style:text-properties fo:color="#000000" fo:font-size="12pt" fo:font-style="normal" fo:font-weight="normal"/></style:style><style:style style:name="Text" style:family="table-cell" style:parent-style-name="Default"/><style:style style:name="Note" style:family="table-cell" style:parent-style-name="Text"><style:table-cell-properties fo:background-color="#ffffcc" style:diagonal-bl-tr="none" style:diagonal-tl-br="none" fo:border="0.74pt solid #808080"/><style:text-properties fo:color="#333333" fo:font-size="10pt" fo:font-style="normal" fo:font-weight="normal"/></style:style><style:style style:name="Footnote" style:family="table-cell" style:parent-style-name="Text"><style:text-properties fo:color="#808080" fo:font-size="10pt" fo:font-style="italic" fo:font-weight="normal"/></style:style><style:style style:name="Hyperlink" style:family="table-cell" style:parent-style-name="Text"><style:text-properties fo:color="#0000ee" fo:font-size="10pt" fo:font-style="normal" style:text-underline-style="solid" style:text-underline-width="auto" style:text-underline-color="#0000ee" fo:font-weight="normal"/></style:style><style:style style:name="Status" style:family="table-cell" style:parent-style-name="Default"/><style:style style:name="Good" style:family="table-cell" style:parent-style-name="Status"><style:table-cell-properties fo:background-color="#ccffcc"/><style:text-properties fo:color="#006600" fo:font-size="10pt" fo:font-style="normal" fo:font-weight="normal"/></style:style><style:style style:name="Neutral" style:family="table-cell" style:parent-style-name="Status"><style:table-cell-properties fo:background-color="#ffffcc"/><style:text-properties fo:color="#996600" fo:font-size="10pt" fo:font-style="normal" fo:font-weight="normal"/></style:style><style:style style:name="Bad" style:family="table-cell" style:parent-style-name="Status"><style:table-cell-properties fo:background-color="#ffcccc"/><style:text-properties fo:color="#cc0000" fo:font-size="10pt" fo:font-style="normal" fo:font-weight="normal"/></style:style><style:style style:name="Warning" style:family="table-cell" style:parent-style-name="Status"><style:text-properties fo:color="#cc0000" fo:font-size="10pt" fo:font-style="normal" fo:font-weight="normal"/></style:style><style:style style:name="Error" style:family="table-cell" style:parent-style-name="Status"><style:table-cell-properties fo:background-color="#cc0000"/><style:text-properties fo:color="#ffffff" fo:font-size="10pt" fo:font-style="normal" fo:font-weight="bold"/></style:style><style:style style:name="Accent" style:family="table-cell" style:parent-style-name="Default"><style:text-properties fo:color="#000000" fo:font-size="10pt" fo:font-style="normal" fo:font-weight="bold"/></style:style><style:style style:name="Accent_20_1" style:display-name="Accent 1" style:family="table-cell" style:parent-style-name="Accent"><style:table-cell-properties fo:background-color="#000000"/><style:text-properties fo:color="#ffffff" fo:font-size="10pt" fo:font-style="normal" fo:font-weight="normal"/></style:style><style:style style:name="Accent_20_2" style:display-name="Accent 2" style:family="table-cell" style:parent-style-name="Accent"><style:table-cell-properties fo:background-color="#808080"/><style:text-properties fo:color="#ffffff" fo:font-size="10pt" fo:font-style="normal" fo:font-weight="normal"/></style:style><style:style style:name="Accent_20_3" style:display-name="Accent 3" style:family="table-cell" style:parent-style-name="Accent"><style:table-cell-properties fo:background-color="#dddddd"/></style:style></office:styles><office:automatic-styles><style:page-layout style:name="Mpm1"><style:page-layout-properties style:writing-mode="lr-tb"/><style:header-style><style:header-footer-properties fo:min-height="21.26pt" fo:margin-left="0pt" fo:margin-right="0pt" fo:margin-bottom="7.09pt"/></style:header-style><style:footer-style><style:header-footer-properties fo:min-height="21.26pt" fo:margin-left="0pt" fo:margin-right="0pt" fo:margin-top="7.09pt"/></style:footer-style></style:page-layout><style:page-layout style:name="Mpm2"><style:page-layout-properties style:writing-mode="lr-tb"/><style:header-style><style:header-footer-properties fo:min-height="21.26pt" fo:margin-left="0pt" fo:margin-right="0pt" fo:margin-bottom="7.09pt" fo:border="2.49pt solid #000000" fo:padding="0.51pt" fo:background-color="#c0c0c0"><style:background-image/></style:header-footer-properties></style:header-style><style:footer-style><style:header-footer-properties fo:min-height="21.26pt" fo:margin-left="0pt" fo:margin-right="0pt" fo:margin-top="7.09pt" fo:border="2.49pt solid #000000" fo:padding="0.51pt" fo:background-color="#c0c0c0"><style:background-image/></style:header-footer-properties></style:footer-style></style:page-layout></office:automatic-styles><office:master-styles><style:master-page style:name="Default" style:page-layout-name="Mpm1"><style:header><text:p><text:sheet-name>???</text:sheet-name></text:p></style:header><style:header-left style:display="false"/><style:footer><text:p>Page <text:page-number>1</text:page-number></text:p></style:footer><style:footer-left style:display="false"/></style:master-page><style:master-page style:name="Report" style:page-layout-name="Mpm2"><style:header><style:region-left><text:p><text:sheet-name>???</text:sheet-name><text:s/>(<text:title>???</text:title>)</text:p></style:region-left><style:region-right><text:p><text:date style:data-style-name="N2" text:date-value="2018-08-30">00/00/0000</text:date>, <text:time>00:00:00</text:time></text:p></style:region-right></style:header><style:header-left style:display="false"/><style:footer><text:p>Page <text:page-number>1</text:page-number><text:s/>/ <text:page-count>99</text:page-count></text:p></style:footer><style:footer-left style:display="false"/></style:master-page></office:master-styles></office:document-styles>"""
}
