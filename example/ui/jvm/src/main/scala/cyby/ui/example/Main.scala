/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui
package example

import msf.js.{Node, raw}

import cyby.ui.{DocType ⇒ DT}

object Main extends util with DocEnv {
  import Txt.{main ⇒ txtMain, _}
  
  def main(args: Array[String]): Unit = args.toList match {
    case path::Nil ⇒ write(path,"")
    case path::prefix::Nil ⇒ write(path,prefix)
    case _        ⇒ println("Run with java -jar </path/to/web/folder> [url prefix]")
  }

  def write(path: String, prefix: String) = {
    val pp = if (path.endsWith("/")) path else s"${path}/"
    val indexF = new java.io.FileWriter(s"${pp}index.html", false)
    val docF = new java.io.FileWriter(s"${pp}doc_${version}.html", false)
    try { docF.write(docHtml) }
    finally docF.close()
    try { indexF.write(index(prefix)) }
    finally indexF.close()
  }

  def docHtml = s"""
<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>CyBy2: Documentation</title>
  <link rel="stylesheet" type="text/css" href="css/cyby_${version}.css">
  <link rel="stylesheet" type="text/css" href="css/doc_${version}.css">
  <style>${expStyle(expEnv, 790)}</style>
</head>
<body>
  ${doc}
</body>
</html>
  """

  def index(prefix: String) = s"""
<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>CyBy2</title>
  <link rel="stylesheet" type="text/css" href="css/cyby_${version}.css">
  <link rel="stylesheet" type="text/css" href="chemdoodle/install/ChemDoodleWeb.css">
  <link rel="stylesheet" type="text/css" href="chemdoodle/install/uis/jquery-ui-1.11.4.custom.css">


  <script type="text/javascript" src="chemdoodle/install/ChemDoodleWeb.js"></script>
  <script type="text/javascript" src="chemdoodle/install/uis/ChemDoodleWeb-uis.js"></script>
  <script type="text/javascript" src="js/cyby-ui_${version}.js"></script>
  <style id="explorerstyle">
    .cell.sub.structure svg { width: 100px; height: 100px; }
    .cell.sub-header.structure { width: 110px; }
    .cell.sub.structure { width: 110px; }
    .row.explorer-sub { height: 110px; }
  </style>
</head>
<body>
<div id="content"></div>
<script>CyBy2.main("${prefix}", document.getElementById("content"))</script>
</body>
</html>

  """


  def doc = div(cls := DT.Outer.c)(
    div(cls := DT.Inner.c)(
      div(cls := DT.Header.c)(
        h1(cls := DT.Title.c)(raw(s"${cyby}: Documentation")),
      ),
      txtMain(cls := DT.Main.c)(
        aside(cls := DT.Toc.c)(
          h1(cls := DT.TocTitle.c)(raw("Content")),
          tocRef1(UId.ExplorerDoc, "Explorer"),
            tocRef2(UId.TableViewDoc, "TableView"),
              tocRef3(UId.ColumnsDoc, "Customizing Columns"),
              tocRef3(UId.FormattingDoc, "Conditional Formatting"),
            tocRef2(UId.StatisticsDoc, "SAR"),
            tocRef2(UId.GridViewDoc, "Grid View"),

          tocRef1(UId.NavigatorDoc, "Navigator"),
            tocRef2(UId.UsersDoc, "Users"),
            tocRef2(UId.ProjectsDoc, "Projects"),
            tocRef2(UId.TagSupDoc, "Suppliers, Tags, Locations"),

          tocRef1(UId.QueryDoc, "Queries"),
            tocRef2(UId.NavSearchDoc, "Navigator Shortcuts"),
            tocRef2(UId.QuickSearchDoc, "Quick Search"),
            tocRef2(UId.CombSearchDoc, "Combined Queries"),
              tocRef3(UId.SingleQueriesDoc, "Single Row Queries"),
              tocRef3(UId.CombiningQueriesDoc, "Combining Queries"),
              tocRef3(UId.QueryManagementDoc, "Query Management"),

          tocRef1(UId.EditingDoc, "Editing Data"),
            tocRef2(UId.CreateDoc, "Adding Data"),
            tocRef2(UId.UpdateDoc, "Updating Data"),
            tocRef2(UId.DeleteDoc, "Deleting Data"),
            tocRef2(UId.ValidateDoc, "Input Validation"),

          tocRef1(UId.ExportDoc, "Exporting Data"),
            tocRef2(UId.ExportSetDoc, "Choosing What to Export"),
            tocRef2(UId.ExportSettingsDoc, "Export Settings"),
            tocRef2(UId.FileFormatsDoc, "Supported File Formats"),
        ),
        section(cls := DT.Article.c)(
          intro,
          Explorer.doc,
          Navigator.doc,
          Query.doc,
          EditingDoc.doc,
          Export.doc,
        )
      )
    )
  )

  def intro: Node =
    div(cls := DT.SectionIntro.c)(
      div(cls := DT.SectionContent.c)(
        div(cls := DT.Paragraph.c)(
          raw(s"""
          ${cyby} is a structure based data management tool developed in
          Rainer Riedl's group at ZHAW. It is being used
          both as an electronic lab inventory with powerful querying capabilities,
          as well as a tool for interactively visualizing structure activity
          relations in drug discovery. Entries in ${cyby} can be linked to
          static files containing spectroscopic data as well as synthetic
          procedures, turning the application into a convenient alternative
          to a full-fledged electronic lab journal.
          """)
        ),
        div(cls := DT.Paragraph.c)(
          raw(s"""
          This document describes in detail the main functionalities of ${cyby}.
          It is generated from the same code base used to compile
          ${cyby}'s web frontend. As such, the format and content of the many
          examples in the document consist of the same HTML elements
          as the real ${cyby} and should stay in sync with
          updates made to the core application. Still, if information in this document
          is erroneous, outdated or unclear, please get in touch with
          Stefan Höck (hock@zhaw.ch), the main developer of ${cyby}.
          """)
        )
      )
    )
}
