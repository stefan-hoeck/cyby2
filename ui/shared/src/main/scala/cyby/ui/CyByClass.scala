/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui

/**
  * Typesafe CSS classes
  */
sealed abstract class CyByClass(override val toString: String)

case class Comp(tpe: CompType) extends CyByClass(tpe.toString)

case class Icon(tpe: IconType) extends CyByClass(s"icon ${tpe.name}")

case class Inner(tpe: MainCompType) extends CyByClass(s"inner ${tpe.name}")

case class Label(tpe: LabelType) extends CyByClass(s"label ${tpe.name}")

case class Title(tpe: TitleType) extends CyByClass(s"title ${tpe.name}")

case class Widget(tpe: WidgetType) extends CyByClass(s"widget ${tpe.name}")

case class DocElem(tpe: DocType) extends CyByClass(s"doc ${tpe.name}")

case class DocIcon(tpe: IconType) extends CyByClass(s"doc icon ${tpe.name}")

case object Logo extends CyByClass("cybylogo")

case object Copyright extends CyByClass("copyright")

case object StructureIconCol extends CyByClass("structureicons")

case class StoQuery(outer: Boolean) extends
  CyByClass(if (outer) "stoquery outer" else "stoquery inner")

case class StoEdit(outer: Boolean) extends
  CyByClass(if (outer) "stoedit outer" else "stoedit inner")

sealed abstract class CompType(override val toString: String) {
  def c: CyByClass = Comp(this)
}

sealed abstract class MainCompType(val name: String)
  extends CompType(s"comp main ${name}")

sealed abstract class ListType(val name: String)
  extends CompType(s"list ${name}")

sealed abstract class CellType(val name: String)
  extends CompType(s"cell ${name}")

sealed abstract class RowType(val name: String)
  extends CompType(s"row ${name}")

object CompType {
  case object Explorer extends MainCompType("explorer")
  case object Log extends MainCompType("log")
  case object LoginStatus extends MainCompType("loginlog")
  case object Nav extends MainCompType("nav")
  case object Queries extends MainCompType("queries")

  case object LoginView extends CompType("comp login")
  case object LoginTxt extends CompType("comp logintxt")
  case object Main extends CompType("comp maincontent")
  case object SideView extends CompType("comp sideview")
  case object SideViewUpper extends CompType("comp sideview-upper")

  case object CheckBoxContainer extends CompType("checkboxcont")

  case object DocLinkContainer extends CompType("doclinkcont")

  case object ExportContainer extends CompType("exportcontainer")
  case object QueryDetails extends CompType("querydetails")

  case object ConDetails extends ListType("container-details-block")
  case object ContainerDets extends ListType("container-details")
  case object Dynamic extends ListType("dynamic")
  case object DynamicEditList extends ListType("edit-dynamic")
  case object ExplorerList extends ListType("explorer")
  case object FormatList extends ListType("format")
  case object InfoList extends ListType("info")
  case object LoginList extends ListType("login")
  case object NavCreateContainer extends ListType("nav-create-container")
  case object NavDetails extends ListType("nav-details")
  case object NavList extends ListType("nav")
  case object Parens extends ListType("parens")
  case object PlotEditList extends ListType("plotedit")
  case object QueryList extends ListType("query")
  case object QueryParen extends ListType("queryparen")
  case object SubTable extends ListType("subtable")
  
  case class LogLvlCell(lvl: LogLevel) extends CellType(s"log ${lvl.toString map (_.toLower)}")
  case class SubCell(s: String) extends CellType(s"sub ${s}")
  case class SubHeaderCell(s: String) extends CellType(s"sub-header ${s}")
  case object BorderTextCell extends CellType("bordertext")
  case object ConDetailCell extends CellType("con-details")
  case object ConIconCell extends CellType("container-icon")
  case object ConRestCell extends CellType("container-rest")
  case object ExplorerTitelFill extends CellType(s"explorer-titel-fill")
  case object QueryFill extends CellType(s"query-fill")
  case object FilDetailCell extends CellType("fil-details")
  case object LogMsgCell extends CellType("logmsg")
  case object MethodStatsCell extends CellType(s"sub method-stats")
  case object MethodStatsHeaderCell extends CellType(s"sub-header method-stats")
  case object NavCell extends CellType("nav")
  case object NavDetailCell extends CellType("nav-details")
  case object SubEntry extends CellType("subentry")
  case object SubGridCell extends CellType(s"sub-grid")


  case object EditColumns extends CompType("columnedit")
  case object EditExport extends CompType("exportedit")
  case object InfoDate extends CompType("infodate")
  case object InfoEntry extends CompType("infoentry")
  case object ListEditContainer extends CompType("listeditcontainer")
  case object NelEditContainer extends CompType("neleditcontainer")
  case object PlotSvgCont extends CompType("plotsvgcont")
  case object PlotSubCont extends CompType("plotsubcont")
  case object QuerySelectContainer extends CompType("queryselectcontainer")
  case object QCompContainer extends CompType("querycompcontainer")
  case object QueryParenTxt extends CompType("query-paren-txt")
  case object StructureEdit extends CompType("structure-edit")

  case class NavDetailRow(s: Symbol) extends RowType(s"nav-details ${s.name}")
  case class NavEditRow(s: Symbol) extends RowType(s"navedit ${s.name}")
  case class NavEditRowStr(s: String) extends RowType(s"navedit ${s}")
  case class ConDetailRow(s: Symbol) extends RowType(s"con-details ${s.name}")
  case object ColumnIconRow extends RowType("columnicons")
  case object ColumnTitleRow extends RowType("columntitle")
  case object CombosAdderRow extends RowType("combos-adder")
  case object DynamicEditRow extends RowType("edit-dynamic")
  case object ExplorerConRow extends RowType("explorer-con")
  case object ExplorerConRowHeader extends RowType("explorer-con-header")
  case object ExportDetailRow extends RowType("export-detail")
  case object ExportFieldRow extends RowType("export-field")
  case object ExplorerHeaderRow extends RowType("explorer-header")
  case object ExplorerSubRow extends RowType("explorer-sub")
  case object FormatAddRow extends RowType("format-add")
  case object FormatRow extends RowType("format")
  case object GridHeaderRow extends RowType("grid-header")
  case object InfoRow extends RowType("info")
  case object LogRow extends RowType("log")
  case object LoginRow extends RowType("login")
  case object NavRow extends RowType("nav")
  case object PlotHeaderRow extends RowType("plotheader")
  case object PlotContentRow extends RowType("plotcontentrow")
  case object ParensRow extends RowType("parens")
  case object QueryRow extends RowType("query")
  case object QueryDetailRow extends RowType("query-detail")
  case object QuerySubRow extends RowType("query-sub")
  case object SubGridRow extends RowType("sub-grid")
  case object SubstanceRow extends RowType("substance")
}


sealed abstract class IconType(val name: String) {
  def c: CyByClass = Icon(this)
  def cd: CyByClass = DocIcon(this)
}

object IconType {
  case object Add extends IconType("add")
  case object AddColumn extends IconType("addcolumn")
  case object AddHidden extends IconType("addhidden")
  case object AddSub extends IconType("addsub")
  case object Cancel extends IconType("cancel")
  case object Clone extends IconType("clone")
  case object Collapsed extends IconType("collapsed")
  case object Confirm extends IconType("confirm")
  case object Delete extends IconType("delete")
  case object DeleteColumn extends IconType("delcolumn")
  case object DeleteHidden extends IconType("deletehidden")
  case object DeleteQuery extends IconType("delete query")
  case object Down extends IconType("down")
  case object Edit extends IconType("edit")
  case object EditFormat extends IconType("editformat")
  case object EditingDisabled extends IconType("editingdisabled")
  case object EditingEnabled extends IconType("editingenabled")
  case object Expanded extends IconType("expanded")
  case object False extends IconType("false")
  case object Flask extends IconType("flask")
  case object FlaskEmpty extends IconType("flask empty")
  case object FlaskLent extends IconType("flask lent")
  case object Info extends IconType("info")
  case object Left extends IconType("left")
  case object Minus extends IconType("minus")
  case object QuickCaseSensitive extends IconType("casesensitive")
  case object QuickCaseInsensitive extends IconType("caseinsensitive")
  case object QuickExcludeEmpty extends IconType("excludeempty")
  case object QuickIncludeEmpty extends IconType("includeempty")
  case object Parens extends IconType("parens")
  case object Plus extends IconType("plus")
  case object Reload extends IconType("reload")
  case object Right extends IconType("right")
  case object SortDec extends IconType("sortdec")
  case object SortInc extends IconType("sortinc")
  case object SortNone extends IconType("sortnone")
  case object True extends IconType("true")
  case object Up extends IconType("up")
  case object Undo extends IconType("undo")
  case object Redo extends IconType("redo")

  case class NavSearch(m: NavSearchMode) extends IconType(s"search_$m")
}

sealed abstract class LabelType(val name: String) {
  def c: CyByClass = Label(this)
}

object LabelType {
  case object Format extends LabelType("format")
  case object GridColumns extends LabelType("gridcolumns")
  case object Nod extends LabelType("nod")
  case object Gradient extends LabelType("gradient")
  case object Login extends LabelType("login")
  case object PlotLabel extends LabelType("plot")
  case class SymbolLabel(s: Symbol) extends LabelType(s"symbol ${s.name}")
}


sealed abstract class TitleType(val name: String) {
  def c: CyByClass = Title(this)
}

object TitleType {
  case object BorderTxt extends TitleType("border txt")
  case object BorderBtnTitle extends TitleType("border btn")

  case object ConDetail extends TitleType("con-detail")
  case object ConRow extends TitleType("con-row")

  case object NavItem extends TitleType("nav-item")
  case object NavSection extends TitleType("nav-section")
  case object NavDetail extends TitleType("nav-detail")
}

sealed abstract class WidgetType(val name: String) {
  def c: CyByClass = Widget(this)
}

object WidgetType {
  case class Edit(t: WidgetType) extends WidgetType(s"${t.name} edit")
  case class Export(t: WidgetType) extends WidgetType(s"${t.name} export")
  case class Format(t: WidgetType) extends WidgetType(s"${t.name} format")
  case class Query(t: WidgetType) extends WidgetType(s"${t.name} query")

  case object BorderBtn extends WidgetType("btn border")
  case object EditPasswordBtn extends WidgetType("btn edit-password")
  case object LoginBtn extends WidgetType("btn login")
  case object LogoutBtn extends WidgetType("btn logout")
  case object RunQueryBtn extends WidgetType("btn runquery")
  case object DeleteQueryBtn extends WidgetType("btn deletequery")
  case object SaveQueryBtn extends WidgetType("btn savequery")

  case object LoginTxt extends WidgetType("input login")
  case object QuickTxt extends WidgetType("input quicksearch")

  case object Text extends WidgetType("input text")
  case object Number extends WidgetType("input number")
  case object Color extends WidgetType("input color")
  case object Date extends WidgetType("input date")
  case object CheckBox extends WidgetType("input checkbox")
  case object QueryName extends WidgetType("input queryname")

  case object BoolSel extends WidgetType("select bool")
  case object ColumnSel extends WidgetType("select column")
  case object ComparatorSel extends WidgetType("select comparator")
  case object DataLinkSel extends WidgetType("select link")
  case object ExportFormatSel extends WidgetType("select export-format")
  case object FieldSel extends WidgetType("select field")
  case object Field2Sel extends WidgetType("select field2")
  case object Field3Sel extends WidgetType("select field3")
  case object GradientSel extends WidgetType("select gradient")
  case object GridColumnsSel extends WidgetType("select grid-columns")
  case object NegatorSel extends WidgetType("select negator")
  case object QPrefixSel extends WidgetType("select queryprefix")
  case object QueryChoiceSel extends WidgetType("select querychoice")
  case object QuerySel extends WidgetType("select query")
  case object LoadQuerySel extends WidgetType("select loadquery")
  case object StatsSel extends WidgetType("select stats")
  case object StorageSel extends WidgetType("select storage")
  case object StringQPrefixSel extends WidgetType("select stringprefix")
  case object UserLevelSel extends WidgetType("select level")
}

sealed abstract class DocType(val name: String) {
  def c: CyByClass = DocElem(this)
}

object DocType {
  case object Inner extends DocType("inner")
  case object Outer extends DocType("outer")
  case object Header extends DocType("header")
  case object Main extends DocType("main")
  case object Toc extends DocType("toc content")
  case object Article extends DocType("article")
  case object Section extends DocType("section")
  case object SectionIntro extends DocType("section intro")
  case object SectionContent extends DocType("sectioncontent")

  case object TocTitle extends DocType("toc title")
  case object TocH1 extends DocType("toc h1")
  case object TocH2 extends DocType("toc h2")
  case object TocH3 extends DocType("toc h3")
  case object TocH4 extends DocType("toc h4")
  case object TocLink extends DocType("toc link")
  
  case object Title extends DocType("title")
  case object H1 extends DocType("h1")
  case object H2 extends DocType("h2")
  case object H3 extends DocType("h3")
  case object H4 extends DocType("h4")
  case object Link extends DocType("link")
  case object ExtLink extends DocType("link ext")
  case object Example extends DocType("example")

  case object Paragraph extends DocType("paragraph")

  case object IconList extends DocType("iconlist")
  case object IconRow extends DocType("iconrow")
  case object IconCell extends DocType("iconcell")
  case object IconDesc extends DocType("icondesc")

  case object DescList extends DocType("desclist")
  case object DescRow extends DocType("descrow")
  case object DescName extends DocType("descname")
  case object DescContent extends DocType("desccontent")
}

// vim: set ts=2 sw=2 et:
