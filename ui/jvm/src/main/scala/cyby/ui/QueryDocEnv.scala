/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui

import cats.implicits._
import cyby.query.{Comp ⇒ QComp, ReadPred ⇒ RP}
import cyby.ui.{IconType ⇒ IT, CompType ⇒ CT, WidgetType ⇒ WT, TitleType ⇒ TT}
import msf.js.{SelectEntry, InputType}, InputType.Text

trait QueryDocEnv extends DocEnv {
  import tags._

  def queries(
    sels: List[SelectEntry],
    qs:   String*
  ): String = Txt.section(cls := CT.Queries.c)(
    Txt.h1(cls := TT.BorderBtnTitle.c)(
      Txt.div(cls := CT.BorderTextCell.c)(loc.queries),
      Txt.div(cls := IT.Up.c)(),
      Txt.div(cls := IT.Down.c)(),
    ),
    Txt.div(cls := Inner(CT.Queries))(
      Txt.li(cls := CT.QuerySubRow.c)(
        Txt.button(cls := WT.RunQueryBtn.c)(Txt text loc.runQuery),
        Txt.div(cls := IT.Add.c)(),
        Txt.div(cls := IT.Parens.c)(),
        Txt.div(cls := CT.QueryFill.c)(),
        Txt.div(cls := CT.QuerySelectContainer.c)(
          Txt.select(cls := WT.LoadQuerySel.c)(Txt options sels),
        ),
        Txt.input(Text, cls := WT.QueryName.c),
        Txt.button(cls := WT.SaveQueryBtn.c)(Txt text loc.saveQuery),
        Txt.button(cls := WT.DeleteQueryBtn.c)(Txt text loc.deleteQuery)
      ),
      Txt.ul(cls := CT.QueryList.c)(qs: _*)
    ),
  )

  val storedQueriesExmpl: List[SelectEntry] = List(
    SelectEntry(some("1"), true,  "New Compounds"),
    SelectEntry(some("2"), false, "Modified Containers"),
  )

  def queryRow(
    comp:     Option[QComp],
    neg:      Boolean,
    field:    String,
    query:    String,
  ): String = Txt.li(cls := CT.QueryRow.c)(
    Txt.div(cls := CT.QCompContainer.c)(
      comp.fold("")(comparator(_, WT.Query))
    ),
    negator(neg, WT.Query),
    field,
    Txt.div(cls := CT.QueryDetails.c)(query),
    Txt.div(cls := IT.DeleteQuery.c)(),
  )

  def parens(comp: Option[QComp])(qs: String*): String =
    Txt.li(cls := CT.ParensRow.c)(
      Txt.div(cls := CT.QCompContainer.c)(
        comp.fold("")(comparator(_, WT.Query))
      ),
      Txt.span(cls := CT.QueryParenTxt.c)(Txt text "(")
    ) ++
    Txt.ul(cls := CT.QueryParen.c)(qs: _*) ++
    Txt.li(cls := CT.ParensRow.c)(
      Txt.span(cls := CT.QueryParenTxt.c)(Txt text ")")
    )

  def txtQ[A](rp: RP[A], s: String): String =
    Txt.input(Text, cls := WT.Query(WT.Text).c, value := rp(s).as(s).get)

  def stringQ(pre: String, s: String): String =
    stringQueryPrefix(pre) ++
    Txt.input(Text, cls := WT.Query(WT.Text).c, value := s)

  def dateQ(pre: String, d: String): String =
    queryPrefix(pre) ++
    Txt.input(InputType.Date, cls := WT.Query(WT.Date).c, value := d)
}
