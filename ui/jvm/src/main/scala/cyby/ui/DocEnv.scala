/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui

import cats.Eq, cats.implicits._
import cyby.dat.{Add ⇒ _, _}
import cyby.query.{Comp ⇒ QComp, StringQPrefixes, QPrefixes}
import cyby.dat.format.{Color, Gradient}
import cyby.export.{Format ⇒ EFormat, Sdf}

import cyby.ui.{IconType ⇒ IT, DocType ⇒ DT, WidgetType ⇒ WT, CompType ⇒ CT,
                TitleType ⇒ TT}
import msf.js.{SelectEntry, InputType ⇒ IPT}

trait DocEnv extends TextEnv {
  import tags._

  //----------------------------------------------------------------------
  //                            Constants
  //----------------------------------------------------------------------
 
  val cyby: String = s"CyBy<sup>2</sup>"

  val enter: String = s"Enter"

  val hint: String = s"Hint"


  //----------------------------------------------------------------------
  //                            Links
  //----------------------------------------------------------------------
  
  def link(id: UIdP, name: String): String =
    Txt.a(cls := DT.Link.c, href := s"#${id}")(name)
  
  def extLink(target: String, name: String): String =
    Txt.a(cls := DT.ExtLink.c, href := target)(name)

  def tocRef1(id: UIdP, n: String): String = 
    Txt.h1(cls := DT.TocH1.c)(link(id, n))

  def tocRef2(id: UIdP, n: String): String = 
    Txt.h2(cls := DT.TocH2.c)(link(id, n))

  def tocRef3(id: UIdP, n: String): String =
    Txt.h3(cls := DT.TocH3.c)(link(id, n))

  lazy val explorerRef = link(UId.ExplorerDoc, "explorer")

  lazy val navigatorRef = link(UId.NavigatorDoc, "navigator")

  lazy val quickSearchRef = link(UId.QuickSearchDoc, "quick search")

  lazy val navSearchRef = link(UId.NavSearchDoc, "navigator shortcuts")

  lazy val combSearchRef = link(UId.CombSearchDoc, "combined queries")

  def exportRef(s: String) = link(UId.ExportDoc, s)

  //----------------------------------------------------------------------
  //                            Resuable Elements
  //----------------------------------------------------------------------

  def checkBox(wt: WT, comp: CT, b: Boolean): String =
    Txt.div(cls := comp.c)(Txt.input(IPT.CheckBox, cls := wt.c, checked := b))

  def regexp: String =
    extLink("https://en.wikipedia.org/wiki/Regular_expression", "regular expression")

  def icon(i: IT): String = Txt.div(cls := i.cd)()

  def example(s: String): String = Txt.div(cls := DT.Example.c)(s)

  def select[A:Eq](vs: List[A], wt: WT, sel: A)(disp: A ⇒ String): String =
    selectE(vs map Right.apply, wt, sel)(disp)

  def links(vs: List[String], sel: String, f: WT ⇒ WT): String =
    select(vs, f(WT.DataLinkSel), sel)(identity)

  def linksA[A:Eq](vs: List[A], sel: A, f: WT ⇒ WT)(name: A ⇒ String): String =
    select(vs, f(WT.DataLinkSel), sel)(name)

  def selectE[A:Eq](vs: List[Either[String,A]], wt: WT, sel: A)(
    disp: A ⇒ String
  ): String = selectP[A](
    vs.map(_ map (a ⇒ a -> disp(a))),
    a1 ⇒ a2 ⇒ a1 === a2,
    wt,
    sel
  )

  def selectP[A](
    vs:  List[Either[String,(A,String)]],
    eqv: A ⇒ A ⇒ Boolean,
    wt:  WT,
    sel: A
  ): String = {
    val es = vs.zipWithIndex map {
      case (Left(str),i)       ⇒ SelectEntry(None,false,str)
      case (Right((a,str)),i)  ⇒
        SelectEntry(Some(i.toString), eqv(a)(sel), str)
    }

    Txt.select(cls := wt.c)(Txt options es)
  }

  def selectEnum[A:Eq:Enum](wt: WT, disp: A ⇒ String, sel: A): String =
    selectEnumP[A](_ ⇒ true)(wt, disp, sel)

  def selectEnumP[A:Eq:Enum](p: A ⇒ Boolean)(wt: WT, disp: A ⇒ String, sel: A)
    : String = select[A](Enum[A].values.toList filter p, wt, sel)(disp)

  def boolSel(sel: Boolean, f: WT ⇒ WT): String =
    select(List(false, true), f(WT.BoolSel), sel)(loc.bool)

  def userLevelSel(sel: UserLevel, f: WT ⇒ WT): String =
    select(UserLevel.values, f(WT.UserLevelSel), sel)(loc.dispUserLevel)
    
  def exportFormat(sel: EFormat, f: WT ⇒ WT): String =
    selectEnum(f(WT.ExportFormatSel), loc.exportFormat, sel)

  def comparator(sel: QComp, f: WT ⇒ WT): String =
    selectEnum(f(WT.ComparatorSel), loc.queryComp, sel)

  def negator(sel: Boolean, f: WT ⇒ WT): String =
    select(List(false, true), f(WT.NegatorSel), sel)(loc.negators)

  def statsType(sel: StatsType, f: WT ⇒ WT): String =
    selectEnum[StatsType](f(WT.StatsSel), _ locName loc, sel)

  def stringQueryPrefix(s: String): String =
    select(StringQPrefixes, WT.StringQPrefixSel, s)(loc.stringQueryPrefix)

  def queryPrefix(s: String): String =
    select(QPrefixes, WT.QPrefixSel, s)(identity)

  // -----------------------------------------------------------------
  // -----             Formats                                   -----
  // -----------------------------------------------------------------
 
  def color(v: Color, f: WT ⇒ WT): String =
    Txt.input(IPT.Color, cls := f(WT.Color).c, value := s"$v")
 
  def int(v: Int, f: WT ⇒ WT): String =
    Txt.input(IPT.Text, cls := f(WT.Number).c, value := s"$v")
 
  def double(v: Double, f: WT ⇒ WT): String =
    Txt.input(IPT.Text, cls := f(WT.Number).c, value := s"$v")

  def gradientRow(v: GradientEntry): String =
    Txt.li(cls := CT.DynamicEditRow.c)(
      double(v._1, WT.Format),
      color(v._2, WT.Format),
      Txt.div(cls := IT.Delete.c)(),
    )

  lazy val confirmRow: String = Txt.li(cls := CT.NavRow.c)(
    Txt.div(cls := IT.Confirm.c)(),
    Txt.div(cls := IT.Cancel.c)(),
  )


  def gradient(st: St, c: Column, v: Gradient[Double]): String = {
    val titleRow: String = Txt.li(cls := CT.NavRow.c)(
      Txt.h1(cls := TT.NavSection.c)(loc.formatHeader(c,st)),
    )

    val nodRow: String = Txt.li(cls := CT.NavEditRowStr("nod").c)(
      lbl("nod", LabelType.Nod),
      int(v.nod, WT.Format)
    )

    val gradRow = Txt.li(cls := CT.NavEditRowStr("gradient").c)(
      lbl("gradient", LabelType.Gradient),
      Txt.div(cls := CT.ListEditContainer.c)(
        Txt.ul(cls := CT.DynamicEditList.c)(
          Txt.li(cls := CT.CombosAdderRow.c)(
            Txt.div(cls := IT.Add.c)() ++
            v.pairs.map(gradientRow).mkString("")
          )
        )
      )
    )
    
    titleRow ++ nodRow ++ gradRow ++ confirmRow
  }
    
  /**
    * Label to an input element
    */
  def lbl(n: String, tpe: LabelType): String =
    Txt.label(cls := tpe.c)(Txt.text(loc name n))

  /**
    * Label to an input element
    */
  def lbl(s: Symbol): String =
    Txt.label(cls := LabelType.SymbolLabel(s).c)(Txt.text(loc name s))

  // -----------------------------------------------------------------
  // -----             Explorer                                  -----
  // -----------------------------------------------------------------

  def explorerDoc(mode: EditMode)(
    export:  String,
    columns: String,
    header:  String,
    subs:    String,
  ): String = Txt.section(cls := CT.Explorer.c)(
    Txt.explorer(mode)(export, columns, header, subs)
  )

  // -----------------------------------------------------------------
  // -----             Export                                    -----
  // -----------------------------------------------------------------
  
  def expRow(s: String): String =
    Txt.li(cls := CT.ExportFieldRow.c)(s, Txt.div(cls := IT.Delete.c)())

  def export(s: USettings, col: Field ⇒ String): String =
    Txt.exportTitle ++
    Txt.li(cls := CT.NavEditRow(UserSettings.exportFormat).c)(
      lbl(UserSettings.exportFormat),
      exportFormat(s.exportFormat getOrElse Sdf, WT.Export),
    ) ++
    Txt.li(cls := CT.NavEditRow(UserSettings.exportSelection).c)(
      lbl(UserSettings.exportSelection),
      checkBox(WT.CheckBox, CT.CheckBoxContainer, s.exportSelection)
    ) ++
    Txt.li(cls := CT.NavEditRow(UserSettings.exportFields).c)(
      lbl(UserSettings.exportFields),
      Txt.div(cls := CT.ListEditContainer.c)(
        Txt.ul(cls := CT.DynamicEditList.c)(
          Txt.li(cls := CT.CombosAdderRow.c)(Txt.div(cls := IT.Add.c)()),
          s.exportFields.map(c ⇒ expRow(col(c))).mkString(""),
        )
      )
    ) ++
    confirmRow
}
