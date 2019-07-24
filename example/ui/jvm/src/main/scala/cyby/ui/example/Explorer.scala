/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui
package example

import cyby.ui.{DocType ⇒ DT, IconType ⇒ IT, CompType ⇒ CT}
import cyby.dat.{EditInfo, example, MedianStat}, example._

object Explorer extends util {
  import Txt._

  def doc = section(id := UId.ExplorerDoc, cls := DT.Section.c)(
    h1(cls := DT.H1.c)("Explorer: Visualizing Data"),
    div(cls := DT.SectionContent.c)(
      div(cls := DT.Paragraph.c)(
        s"""
        Most interesting pieces of information (especially results
        from ${link(UId.QueryDoc, "queries")}) are displayed in the Explorer.
        The way data is
        being displayed can be customized in several ways. There
        are two tabular views, grouping data differently.
        Users can choose the order and type of ${link(UId.ColumnsDoc, "columns")}
        in these views and can define rules for
        ${link(UId.FormattingDoc, "background coloring")}.
        In addition, a ${link(UId.GridViewDoc, "grid view")}
        gives a quick overview over compounds
        in the hit set, with a convenient way to select a subset
        for exporting.
        """
      ),
      h2(id := UId.TableViewDoc, cls := DT.H2.c)(
        "Table View: Compounds, Files, and Containers"
      ),
      div(cls := DT.Paragraph.c)(
        s"""
        The default view in the Explorer displays compounds, containers
        and connected data as an expandable tree. Lists of nodes can
        be expanded or collapsed by clicking the corresponding icons.
        """
      ),
      example(
        explorerDoc(NotEditing)("", "", mkHeadRow(expEnv), dispSubs(expEnv))
      ),
      div(cls := DT.Paragraph.c)(
        s"""
        Below is a description of Explorer control icons:
        """
      ),
      div(cls := DT.IconList.c)(
        div(cls := DT.IconRow.c)(
          div(cls := DT.IconCell.c)(icon(IT.Collapsed)),
          div(cls := DT.IconDesc.c)(
            """
            Click one of these icons to expand the corresponding
            list of child nodes.
            """
          )
        ),
        div(cls := DT.IconRow.c)(
          div(cls := DT.IconCell.c)(icon(IT.Expanded)),
          div(cls := DT.IconDesc.c)(
            s"""
            Click one of these icons to collapse (= hide)
            the corresponding list of child nodes.
            """
          )
        ),
        div(cls := DT.IconRow.c)(
          div(cls := DT.IconCell.c)(icon(IT.SortNone)),
          div(cls := DT.IconDesc.c)(
            s"""
            Marks a sortable column. Click one of these
            to sort data accordingly.
            """
          )
        ),
        div(cls := DT.IconRow.c)(
          div(cls := DT.IconCell.c)(icon(IT.SortInc)),
          div(cls := DT.IconDesc.c)(
            s"""
            Marks the column, by which the dataset is currently
            sorted (in increasing order).
            """
          )
        ),
        div(cls := DT.IconRow.c)(
          div(cls := DT.IconCell.c)(icon(IT.SortDec)),
          div(cls := DT.IconDesc.c)(
            s"""
            Marks the column, by which the dataset is currently
            sorted (in decreasing order).
            """
          )
        ),
        div(cls := DT.IconRow.c)(
          div(cls := DT.IconCell.c)(icon(IT.Minus)),
          div(cls := DT.IconDesc.c)(
            s"""
            Reduce the size of molecular structures.
            """
          )
        ),
        div(cls := DT.IconRow.c)(
          div(cls := DT.IconCell.c)(icon(IT.Plus)),
          div(cls := DT.IconDesc.c)(
            s"""
            Increase the size of molecular strucures.
            """
          )
        ),
      ),
      h3(id := UId.ColumnsDoc, cls := DT.H3.c)("Customizing Columns"),
      div(cls := DT.Paragraph.c)(
        s"""
        In addition to sorting data, column headers provide
        a couple of additional utility buttons. These appear
        when hovering the mouse pointer over a column header.
        All changes thus made to the column layout are
        persisted at the server and reloaded upon next
        login.
        """
      ),
      div(cls := DT.IconList.c)(
        div(cls := DT.IconRow.c)(
          div(cls := DT.IconCell.c)(icon(IT.AddColumn)),
          div(cls := DT.IconDesc.c)(
            """
            Opens an additional view to select a new table
            column. This new column will be added to the right
            of the column where the "Add Column" icon
            was clicked.
            """
          )
        ),
        div(cls := DT.IconRow.c)(
          div(cls := DT.IconCell.c)(icon(IT.DeleteColumn)),
          div(cls := DT.IconDesc.c)(
            s"""
            Removes the selected column from the table.
            """
          )
        ),
        div(cls := DT.IconRow.c)(
          div(cls := DT.IconCell.c)(icon(IT.EditFormat)),
          div(cls := DT.IconDesc.c)(
            s"""
            Opens a new view where
            ${link(UId.FormattingDoc, "conditional formatting")}
            rules for the selected column can be defined.
            Right now, this is only available for numeric
            columns.
            """
          )
        ),
      ),
      div(cls := DT.Paragraph.c)(
        s"""
        Below is an example for adding another column. In order to confirm
        your selection, click the icon on the left, the one on the right
        will cancel the process without changing the table's columns.
        """
      ),
      example(
        explorerDoc(NotEditing)("", columnT(SubEditInfo(EditInfo.Summary).ef), "","")
      ),
      h3(id := UId.FormattingDoc, cls := DT.H3.c)("Conditional Formatting"),
      div(cls := DT.Paragraph.c)(
        s"""
        For numeric columns it is possible to define a background color
        gradient to add some visual depth to the numbers. Below
        are two example gradients. The resulting colorings can be seen
        in the the ${link(UId.TableViewDoc, "table view example")} and
        the ${link(UId.StatisticsDoc, "statistics example")}.
        """
      ),
      div(cls := DT.Paragraph.c)(
        s"""
        The first example describes a gradient for a column where
        the ideal value is not the theoretic maximum or minimum
        but lies somewhere in between. In drug development, molecules
        with a molar weight in the range of 250 to 500 g/mol are
        desirable. The gradient below reflects this.
        While colors are interpolated between the predefined
        values, molecules with a molar mass blelow 50 g/mol or above
        900 g/mol will just get a red background color.
        Note that all values must be given either
        in increasing or decreasing order for the gradient to
        be well defined.
        """
      ),
      example(
        explorerDoc(NotEditing)("", gradient(st, massCol, massGrad) ,"","")
      ),
      div(cls := DT.Paragraph.c)(
        s"""
        The second gradient is an example for a numeric column
        where lower values are "better". Other colorings are possible
        of course. It is also possible to add additional rows for
        intermediate values to simulate non-linear color interpolations.
        Feel free to experiment!
        """
      ),
      example(
        explorerDoc(NotEditing)("", gradient(st, mmp10Col, mmp10Grad) ,"","")
      ),

      h2(id := UId.StatisticsDoc, cls := DT.H2.c)(
        "Statistics: Analyzing Structure-Activity Relations"
      ),
      div(cls := DT.Paragraph.c)(
        s"""
        While the default view of the data tree is useful when using
        ${cyby} as an inventory or a static lab journal, it is not
        well suited for analyzing structure activity relations. For this task,
        the "Statistics" view was added. It comes with the same types
        of interactive column headers for sorting and formatting, but
        it groups biodata by batch (container). In addition to
        the columns available in the table view,
        columns with statistical summaries of bioactivities can be selected.
        """
      ),
      example(
        explorerDoc(NotEditing)("", "", mkHeadRow(statsEnv), dispStatsList(statsEnv))
      ),
      div(cls := DT.Paragraph.c)(
        s"""
        Below is an example for adding a column displaying a statistic
        summary with regard to the selected bioassay.
        """
      ),
      example(
        explorerDoc(NotEditing)(columnS(ExportStats(mmp10.id, MedianStat)),
          "", "", "")
      ),

      h2(id := UId.GridViewDoc, cls := DT.H2.c)("Grid View"),
      div(cls := DT.Paragraph.c)(
        s"""
        If a query returns a large hit set, it can be helpful to
        gain a quick overview over all compounds, for instance to
        facilitate selecting compounds for exporting. The grid view
        was designed with this use case in mind.
        """
      ),
      example(
        explorerDoc(NotEditing)("", "", mkHeadRow(gridEnv), dispSubs(gridEnv))
      ),
      div(cls := DT.Paragraph.c)(
        s"""
        Selecting compounds in the grid is done by left-clicking
        on a compound's code. Holding down Ctrl while clicking
        toggles adding additional compounds, holding down Shift
        lets users select ranges of compounds.
        """
      ),

    )
  )

  def dispSubs(e: ExpEnv): String = Txt.ul(cls := CT.SubTable.c)(
    Txt.subItems[Sub.Cli](e, dispSub, s ⇒ UId Select subIdString(s.id))(e.dispEnv.st.subs) : _*
  )

  def dispStatsList(e: ExpEnv): String = Txt.ul(cls := CT.SubTable.c)(
    Txt.subItems[BioStats](e, dispStats, s ⇒ UId Select statsIdString(statsId(s)))(e.dispEnv.st.bio) : _*
  )

  def columnT(e: ExportField): String = columnM(e, SubstanceTable)

  def columnS(e: ExportField): String = columnM(e, MethodTable)

  def columnM(e: ExportField, m: ExpMode): String =
    columnsTitle ++ Txt.li(cls := CT.NavRow.c)(columnF(e, m)) ++ confirmRow
}
