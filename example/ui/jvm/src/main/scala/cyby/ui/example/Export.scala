/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui
package example

import cyby.ui.{DocType ⇒ DT}

object Export extends util {
  import Txt._

  def doc = section(id := UId.ExportDoc, cls := DT.Section.c)(
    h1(cls := DT.H1.c)("Exporting Data"),
    div(cls := DT.SectionContent.c)(
      div(cls := DT.SectionContent.c)(
        div(cls := DT.Paragraph.c)(
          s"""
          The actual hit set of a ${cyby} query can be exported
          to several different file formats. Users get to exactly
          choose the pieces of information to be exported. In
          addition, users can choose to only export the selected
          compounds instead of the whole hit set.
          """
        )
      ),
      h2(id := UId.ExportSetDoc, cls := DT.H2.c)("Choosing What to Export"),
      div(cls := DT.Paragraph.c)(
        s"""
        The first thing to understand is, that the data being exported
        corresponds always to the hit set of the most recent query,
        no matter whether you used the
        ${link(UId.QuickSearchDoc, "quick search")},
        a ${link(UId.CombSearchDoc, "combined query")} or
        a ${link(UId.NavSearchDoc, "query shortcut")} from the navigator.
        Have a look at the Log in the lower left window. There you will
        usually see some information about the total number of compounds
        in the latest query. If it says something along the lines of
        "120 substances out of 1245 loaded", then the second number (1245
        in this case)
        is the number of compounds that will be exported. You therefore
        do not need to load thousands of compounds into your browser's memory
        in order to export them all.
        """
      ),
      div(cls := DT.Paragraph.c)(
        s"""
        Sometimes you may want some additional control over the compounds
        to be exported. In this case you can use the
        ${link(UId.GridViewDoc, "grid view")} to select a subset
        of compounds. In the export section you can then choose to
        only export selected compounds.
        """
      ),
      h2(id := UId.ExportSettingsDoc, cls := DT.H2.c)("Export Settings"),
      div(cls := DT.Paragraph.c)(
        s"""
        In the title bar of the explorer, click the "Export" button
        to open an addtional screen to configure the content and format of
        the exported file. Select the desired file format and whether
        to only export selected compounds. In the list of exported fields,
        use the blue "plus" icon to add additional fields and the red
        crosses to remove fields. Once you are done, click the green
        check mark to start the file download.
        """
      ),
      example(explorerDoc(NotEditing)(export(expSt.settings, exportF), "", "", "")),
      h2(id := UId.FileFormatsDoc, cls := DT.H2.c)("Supported File Formats"),
      div(cls := DT.Paragraph.c)(
        s"""
        Below is a list of supported file formats. All of them are
        clear text formats, editable with your editor of choice
        if necessary.
        """
      ),
      div(cls := DT.DescList.c)(
        div(cls := DT.DescRow.c)(
          div(cls := DT.DescName.c)("SDF"),
          div(cls := DT.DescContent.c)(
            s"""
            This is a chemical file format, which can be read by
            most chemical drawing applications like ChemDraw or
            ChemAxon's MarvinSketch and MarvinView. Several compounds
            annotated with additional data fields
            can be stored in a single .sdf file.
            """
          )
        ),
        div(cls := DT.DescRow.c)(
          div(cls := DT.DescName.c)("Text"),
          div(cls := DT.DescContent.c)(
            s"""
            Probably the most universably useable file format.
            It contains one entry per line, and columns are delimited
            by tabs. If you want to inlude molecular structures,
            chose the SMILES or InCHi field.
            This format can be imported into spreadsheet
            applications like LibreOffice Calc or MS Excel,
            and is the one best suited for further processing
            by algorithms of your own writing.
            """
          )
        ),
        div(cls := DT.DescRow.c)(
          div(cls := DT.DescName.c)("ODF"),
          div(cls := DT.DescContent.c)(
            s"""
            If you want to process exported data in your
            spreadsheet application of choice (LibreOffice Calc
            or MS Excel or the like), this is probably the format
            you want. Every field is written to its own column
            in the table. While you cannot export structures (as pictures
            or graphics) directly, you can still export their SMILES
            string or InCHi key and use an appropriate plugin
            if available.
            """
          )
        ),
      ),
    )
  )
}

