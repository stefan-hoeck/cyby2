/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui
package example

import cyby.query.{Comp ⇒ QComp, ReadPred ⇒ RP, Contains}
import cyby.dat._, example._
import cyby.ui.{IconType ⇒ IT, DocType ⇒ DT, CompType ⇒ CT, WidgetType ⇒ WT}

object Query extends util with QueryDocEnv {
  import Txt._

  def doc = section(id := UId.QueryDoc, cls := DT.Section.c)(
    h1(cls := DT.H1.c)("Queries: Collecting Data"),
    div(cls := DT.SectionContent.c)(
      intro,
      shortCuts,
      quick,
      combined,
    )
  )

  def intro = div(cls := DT.Paragraph.c)(
    s"""${cyby} provides several convenient ways to
    get information from the underlying database.
    Its ${quickSearchRef} functionality and the navigator's
    ${navSearchRef} are best used to quickly view a specific subset
    of the data. The highly versatile ${combSearchRef}
    allow for a finer grain of control over the information
    the server returns.
    """
  ) ++ div(cls := DT.Paragraph.c)(
    """When running quick searches or combined queries, all
    settings are persisted at the server and loaded upon next
    login. Additionally, combined queries can be given a name
    and stored in a drop-down menu, from which they can be
    quickly reloaded."""
  )

  def quick = section(id := UId.QuickSearchDoc)(
    h2(cls := DT.H2.c)("Quick Search"),
    div(cls := DT.Paragraph.c)(
      s"""
      The quick search is located in the title bar
      of the ${explorerRef}:
      """
    ),
    example(explorerDoc(NotEditing)("","","","")),
    div(cls := DT.Paragraph.c)(
      s"""
      The text field accepts several
      types of queries, which can be sent to the server
      by pressing ${enter}. In addition, two icons let
      users control the exact behavior of the quick search.
      """
    ),
    h3(cls := DT.H3.c)("Search by Compound ID"),
    div(cls := DT.Paragraph.c)(
      s"""
      Entering a single integer or a list of space-delimited
      integers will query compounds by their unique ID.
      ${hint}: IDs can be copied and pasted directly from
      spreadsheet applications like Excel or LibreOffice Calc.
      """
    ),
    h3(cls := DT.H3.c)("Search by CAS-Nr"),
    div(cls := DT.Paragraph.c)(
      s"""
      Entering a single valid CAS number will activate this
      type of query. No other fields are searched.
      """
    ),
    h3(cls := DT.H3.c)("Search by Regular Expression"),
    div(cls := DT.Paragraph.c)(
      s"""
      Any other kind of input will be interpreted as a ${regexp},
      and will be used to query all textual fields of compounds, containers,
      and biological data. This is useful to give a quick overview
      over content linked to certain topics.
      """
    ),
    h3(cls := DT.H3.c)("Control Icons"),
    div(cls := DT.Paragraph.c)(
      s"""
      There are two sets of icons controlling the behavior of the quick search.
      You can switch between different behaviors by clicking
      the icons in question.
      Note that toggling the crossed-flask icon can lead to surprising
      results, as some known compounds might be missing from the hit set.
      """
    ),
    div(cls := DT.IconList.c)(
      div(cls := DT.IconRow.c)(
        div(cls := DT.IconCell.c)(icon(IT.QuickExcludeEmpty)),
        div(cls := DT.IconDesc.c)(
          s"""
          When the crossed-flask icon is active, only non-empty
          containers will be listed. Compounds with only empty
          containers or with no containers at all will be excluded
          from the hit set. Choose this option when using ${cyby}
          as a lab inventory.
          """
        )
      ),
      div(cls := DT.IconRow.c)(
        div(cls := DT.IconCell.c)(icon(IT.QuickIncludeEmpty)),
        div(cls := DT.IconDesc.c)(
          """
          With this icon being active, all containers will be
          returned as will be compounds without any containers.
          Choose this option whenever you are not explicitly looking
          for physical containers.
          """
        )
      ),
      div(cls := DT.IconRow.c)(
        div(cls := DT.IconCell.c)(icon(IT.QuickCaseInsensitive)),
        div(cls := DT.IconDesc.c)(
          """
          Textual search is case insensitive. This is the default.
          """
        )
      ),
      div(cls := DT.IconRow.c)(
        div(cls := DT.IconCell.c)(icon(IT.QuickCaseSensitive)),
        div(cls := DT.IconDesc.c)("Toggles case sensitive textual search.")
      ),
    ),
  )

  def shortCuts = section(id := UId.NavSearchDoc)(
    h2(cls := DT.H2.c)("Navigator Shortcuts"),
    div(cls := DT.Paragraph.c)(
      s"""
      Items listed in ${cyby}'s ${navigatorRef} come with several
      utility icons for directly loading linked data objects.
      These icons appear when hovering the mouse pointer over
      a navigator entry. Querying by several entries
      at once is not supported for the time being. If you need this
      kind of control over your queries, use ${combSearchRef}.
      The following table gives an overview of the icons in question:
      """
    ),
    div(cls := DT.IconList.c)(
      div(cls := DT.IconRow.c)(
        div(cls := DT.IconCell.c)(icon(IT.NavSearch(NSSub))),
        div(cls := DT.IconDesc.c)(
          s"""
          Lists all compounds linked to the selected
          project or tag. All additional data like containers and files
          linked to the compound in question are returned as well.
          """
        )
      ),
      div(cls := DT.IconRow.c)(
        div(cls := DT.IconCell.c)(icon(IT.NavSearch(NSConAll))),
        div(cls := DT.IconDesc.c)(
          """
          Lists all containers (empty and non-empty) linked to the
          selected project, tag, supplier, or storage location.
          Compounds without containers are not included in the hit set.
          """
        )
      ),
      div(cls := DT.IconRow.c)(
        div(cls := DT.IconCell.c)(icon(IT.NavSearch(NSConNonEmpty))),
        div(cls := DT.IconDesc.c)(
          """
          Lists all non-empty containers linked to the
          selected project, tag, supplier, or storage location.
          Compounds without containers or with only empty containers
          are not included in the hit set.
          """
        )
      ),
      div(cls := DT.IconRow.c)(
        div(cls := DT.IconCell.c)(icon(IT.NavSearch(NSBio))),
        div(cls := DT.IconDesc.c)(
          """
          Lists all biodata entries linked to the
          selected project, tag, supplier, or method.
          """
        )
      ),
    )
  )

  def combined = section(id := UId.CombSearchDoc)(
    h2(cls := DT.H2.c)("Combined Queries"),
    div(cls := DT.Paragraph.c)(
      s"""
      If navigator shortcuts and quick search are too general
      to suit your needs, you should switch to ${cyby}'s highly
      versatile mechanism for combined queries. Defining such
      queries is more involved than using one of the other
      options but well worth the effort especially since
      often used queries can be saved and reloaded.
      """
    ),
    h3(id := UId.SingleQueriesDoc, cls := DT.H3.c)("Single Row Queries"),
    div(cls := DT.Paragraph.c)(
      s"""
      Press the "plus" icon in the query window at the bottom
      of ${cyby}'s main section to add a new query line:
      """
    ),
    example(queries(storedQueriesExmpl, idExmpl("10"))),
    div(cls := DT.Paragraph.c)(
      s"""
      You will be presented with a sequence of three selection
      boxes followed by a text field. The first selection
      box can be used to negate queries, the second selects
      the type of items to be queried (substance, container, biodata, or
      statistics), and the third lets you choose a field
      by which you want to query items.
      The last item (the text field) will change depending on your
      former selections.
      """
    ),
    h4(cls := DT.H4.c)("Numeric Queries"),
    div(cls := DT.Paragraph.c)(
      s"""
      Queries for numeric values can be defined in several was.
      A list of space-delimited numbers will match any of its values.
      The following query finds compounds with IDs 10, 12, and 14:
      """
    ),
    exampleQ(idExmpl("10 12 14")),
    div(cls := DT.Paragraph.c)(
      s"""
      Additionaly, queries can be defined as pseudo-mathematical
      expressions using comparison operators (one of
      "<", "<=", ">", ">=", "==", or "!="). In this case, operators
      and values must be separated by at least one space character:
      """
    ),
    exampleQ(massExmpl("< 500")),
    div(cls := DT.Paragraph.c)(
      s"""
      Expressions can be combined using logical
      operators ("and", "or", or "not"; again separated by whitespace)
      and grouped using parentheses:
      """
    ),
    exampleQ(massExmpl("(> 10 and <= 100) or (>= 250 and < 400)")),

    h4(cls := DT.H4.c)("Text Queries"),
    div(cls := DT.Paragraph.c)(
      s"""
      For text fields, users can decide with an additional selection
      box, what kind of query to use. The default is
      a case insensitive search by ${regexp}:
      """
    ),
    exampleQ(subRow(SubName, stringQ(Contains, "^3-Amino"))),

    h3(id := UId.CombiningQueriesDoc, cls := DT.H3.c)("Combining Queries"),
    div(cls := DT.Paragraph.c)(
      s"""
      Rows of queries can be combined using logical operators
      and grouped in paretheses to define versatile
      filters for data analysis and maintenance tasks.
      By clicking the "plus" icon several times, new rows
      can be added to the list of queries. Each row except
      the first one will start with a selection box for
      logical combinators. Clicking the "( )" icon
      will add a set of parentheses, together with additional
      buttons for adding new rows. Below are a few examples.
      """
    ),

    h4(cls := DT.H4.c)("Lab Inventory"),
    div(cls := DT.Paragraph.c)(
      s"""
      The following query lists all non-empty containers
      in rooms RT107 and RT109. The resulting list can
      be ${exportRef("exported")} to facilitate checking
      the presence of these containers in the lab:
      """
    ),
    exampleQ(ex1),

    h4(cls := DT.H4.c)("Data Analysis"),
    div(cls := DT.Paragraph.c)(
      s"""
      The following query lists all compounds with
      an IC-50 activity against MMP-13 below 1 mmol/L
      and an IC-50 activity against MMP-10 above 10 mmol/L
      with a molar weight in the range of 250 to 600 g/mol:
      """
    ),
    exampleQ(ex2),

    h4(cls := DT.H4.c)("Data Maintenance"),
    div(cls := DT.Paragraph.c)(
      s"""
      Several queries similar to the following one are used
      by the author to review changes made to
      data. The following query lists all new containers
      added after 2018 but not linked to compounds
      added in the same timespan (because those were already
      checked together with their containers):
      """
    ),
    exampleQ(ex3),
  
    h3(id := UId.QueryManagementDoc, cls := DT.H2.c)("Managing Queries"),
    div(cls := DT.Paragraph.c)(
      s"""
      Queries can be given a name and stored under a user's profile
      for later reuse. Just enter a name in the text field beside
      the "Save Query" button. After clicking said button, the
      query will be added to the list of stored queries.
      """
    ),
    example(queries(storedQueriesExmpl, "")),
    div(cls := DT.Paragraph.c)(
      s"""
      In order to load a query from the dropdown list, select
      its entry and press ${enter}. A selected query can also
      be deleted using the "Delete Query" button.
      """
    ),
  )



  def idExmpl(s: String) = subRow(SubId, txtQ(RP.id_[Sub.type], s))

  def massExmpl(s: String) = subRow(SubMol(Mol.Mass), txtQ(RP.double_, s))

  def subRow(f: SubField, q: String, comp: Option[QComp] = None, neg: Boolean = false) =
    queryRow(comp, neg, queryF(f.ef), q)

  def conRow(f: ConField, q: String, comp: Option[QComp] = None, neg: Boolean = false) =
    queryRow(comp, neg, queryF(f.ef), q)

  def statsRow(m: Met.Cli, q: String, comp: Option[QComp] = None) =
    queryRow(comp, false, queryF(ExportStats(m.id, MeanStat)),
      txtQ(RP.double_, q))

  def exampleQ(ss: String*): String = example(ul(cls := CT.QueryList.c)(ss: _*))

  lazy val ex1 =
    conRow(ConEmpty, boolSel(false, WT.Query)) ++
    parens(some(QComp.And))(
      conRow(ConLocation, loc(rt107)),
      conRow(ConLocation, loc(rt109), some(QComp.Or))
    )

  lazy val ex2 = massExmpl("> 250 and < 600") ++
                 statsRow(mmp10, "> 10", some(QComp.And)) ++
                 statsRow(mmp13, "< 1", some(QComp.And))

  lazy val ex3 =
    conRow(ConCreated, dateQ(">=", "2019-01-01")) ++
    subRow(SubCreated, dateQ(">=", "2019-01-01"), some(QComp.And), true)
}
