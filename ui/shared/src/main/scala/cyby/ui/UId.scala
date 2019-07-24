/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui

import cats.implicits._
import cyby.ui.{NavSearchMode ⇒ NSM}

/**
  * Typesafe IDs for HTML elements.
  *
  * We abstract over two types: types of data objects encoded
  * in IDs and paths leading to data objects encoded in IDs.
  * (see also type UiDp in trait cyby.ui.UIEnv).
  *
  * Note: In a future version of CyBy it might make more sense
  * to use a custom attribute representing commands instead of
  * (ab)using element IDs for this.
  *
  * Note 2: IDs are delimited by hyphens (-). Therefore, do not use
  * hyphens in the string representation of objects of type D or P, as
  * those will be split up when parsing an element ID.
  *
  * @tparam D: data types encoded as part of element IDs
  * @tparam P: data paths encoded as part of element IDs
  */
sealed abstract class UId[+D,+P](val id: String){
  def i: UId[D,P] = this
  override def toString = id
}

sealed abstract class UIdN(id: String) extends UId[Nothing,Nothing](id)

sealed abstract class UIdP[P](id: String) extends UId[Nothing,P](id)

sealed abstract class UIdD[D](id: String) extends UId[D,Nothing](id)


object UId {
  case class AddColumnId(index: Int) extends UIdN(s"addcolumn-$index")
  case class ClickEdit[P](fld: String, p: P) extends UIdP[P](s"clickedit-$fld-$p")
  case class Clone[P](p: P) extends UIdP[P](s"clone-$p")
  case class CreateCont[D,P](dt: D, p: P) extends UId[D,P](s"createcont-$dt-$p")
  case class Create[D,P](dt: D, p: P) extends UId[D,P](s"create-$dt-$p")
  case class Dat[P](p: P) extends UIdP[P](s"data-$p")
  case class DataList[D,P](dt: D, p: P) extends UId[D,P](s"datalist-$dt-$p")
  case class DelColumnId(index: Int) extends UIdN(s"delcolumn-$index")
  case class DeleteId[P](p: P) extends UIdP[P](s"delete-$p")
  case class EditCont[P](fld: String, p: P) extends UId(s"editcont-$fld-$p}")
  case class EditFormatId(s: String) extends UIdN(s"editformat-$s")
  case class Edit[P](fld: String, p: P) extends UIdP[P](s"edit-$fld-$p")
  case class EditableColumn(index: Int) extends UIdN(s"editablecolumn-$index")
  case class Expand[D,P](comp: UId[D,P]) extends UId[D,P](s"expand-${comp.id}")
  case class Item[D,P](comp: UId[D,P]) extends UId[D,P](s"item-${comp.id}")
  case class Lbl[D,P](comp: UId[D,P]) extends UId[D,P](s"lbl-${comp.id}")
  case class Load[D](dt: D) extends UIdD[D](s"load-$dt")
  case class NavSearch[P](p: P, m: NSM) extends UIdP[P](s"navsearch-$m-$p")
  case class NavSectionId[D](dt: D) extends UIdD[D](s"navsection-$dt")
  case class Plot[P](p: P) extends UIdN(s"plot-$p")
  case class Select(sid: String) extends UIdN(s"select-${sid}")
  case class SortSub(s: String) extends UIdN(s"sortsub-$s")
  case class StrId(s: String) extends UIdN(s)
  case class Unique(l: Long) extends UIdN(s"unique-${l}")

  /**
    * Reads an element ID from a list of strings.
    */
  def fromSplit[D:Read,P:Read](ss: List[String]): Option[UId[D,P]] = ss match {
    case s::Nil if s.nonEmpty     ⇒ some(StrId(s))
    case "unique"::l::Nil         ⇒ read[Long](l) map Unique.apply
    case "addcolumn"::s::Nil      ⇒ read[Int](s) map  AddColumnId.apply
    case "delcolumn"::s::Nil      ⇒ read[Int](s) map DelColumnId.apply
    case "editformat"::s::Nil     ⇒ some(EditFormatId(s))
    case "editablecolumn"::s::Nil ⇒ read[Int](s) map EditableColumn.apply
    case "select"::s::Nil         ⇒ some(Select(s))
    case "sortsub"::s::Nil        ⇒ some(SortSub(s))
    case "lbl"::t                 ⇒ fromSplit[D,P](t) map Lbl.apply
    case "expand"::t              ⇒ fromSplit[D,P](t) map Expand.apply
    case "create"::d::p::Nil      ⇒ (read[D](d),read[P](p)).mapN(Create.apply)
    case "plot"::p::Nil           ⇒ read[P](p).map(Plot.apply)
    case "createcont"::d::p::Nil  ⇒ (read[D](d),read[P](p)).mapN(CreateCont.apply)
    case "delete"::p::Nil         ⇒ read[P](p) map DeleteId.apply
    case "data"::p::Nil           ⇒ read[P](p) map Dat.apply
    case "datalist"::d::p::Nil    ⇒ (read[D](d),read[P](p)).mapN(DataList.apply)
    case "edit"::f::p::Nil        ⇒ read[P](p) map (Edit(f, _))
    case "clickedit"::f::p::Nil   ⇒ read[P](p) map (ClickEdit(f, _))
    case "editcont"::f::p::Nil    ⇒ read[P](p) map (EditCont(f, _))
    case "item"::t                ⇒ fromSplit[D,P](t) map Item.apply
    case "navsection"::d::Nil     ⇒ read[D](d) map NavSectionId.apply
    case "load"::d::Nil           ⇒ read[D](d) map Load.apply
    case "navsearch"::NSM(m)::p::Nil ⇒ read[P](p) map (NavSearch(_,m))
    case "clone"::p::Nil          ⇒ read[P](p) map Clone.apply
    case _                        ⇒ None
  }

  def strId(s: String): UIdN = StrId(s)
  
  final val Columns            = strId("columns")
  final val Content            = strId("content")
  final val DeleteQuery        = strId("deletequery")
  final val EditExportBtn      = strId("editexportbtn")
  final val EditPlotBtn        = strId("editplotbtn")
  final val ExplorerId         = strId("explorer")
  final val ExplorerStyle      = strId("explorerstyle")
  final val ExplorerZoomIn     = strId("explorerzoomin")
  final val ExplorerZoomOut    = strId("explorerzoomout")
  final val ExportId           = strId("export")
  final val GridColumns        = strId("gridcolumns")
  final val Header             = strId("header")
  final val InfoBtn            = strId("infobtn")
  final val InfoView           = strId("info")
  final val LogId              = strId("cybylog")
  final val Login              = strId("login")
  final val LoginBtnId         = strId("dologin")
  final val LoginLog           = strId("loginlog")
  final val LoginName          = strId("loginname")
  final val LoginPw            = strId("loginpw")
  final val LoginStatusId      = strId("loginstatus")
  final val LogoutBtnId        = strId("dologout")
  final val MainId             = strId("main")
  final val NavBtn             = strId("navbtn")
  final val NavId              = strId("nav")
  final val NavLeft            = strId("navleft")
  final val NavRight           = strId("navright")
  final val NavView            = strId("navview")
  final val PlotEditList       = strId("ploteditlist")
  final val PlotSvgCont        = strId("plotsvgcont")
  final val PlotSubCont        = strId("plotsubcont")
  final val QueriesId          = strId("queries")
  final val QueryDown          = strId("querydown")
  final val QueryName          = strId("queryname")
  final val QueryRedo          = strId("queryredo")
  final val QuerySelect        = strId("queryselect")
  final val QuerySelectCont    = strId("queryselectcont")
  final val QueryUndo          = strId("queryundo")
  final val QueryUp            = strId("queryup")
  final val QueryView          = strId("queryview")
  final val QuickSearchCase    = strId("quicksearchcase")
  final val QuickSearchEmpty   = strId("quicksearchempty")
  final val QuickSearchTxt     = strId("quicksearchtxt")
  final val RunQuery           = strId("runquery")
  final val SaveQuery          = strId("savequery")
  final val SideViewId         = strId("sideview")
  final val SubGridBtn         = strId("subgridbtn")
  final val SubStatsBtn        = strId("substatsbtn")
  final val SubTableBtn        = strId("subtablebtn")
  final val SubTableCont       = strId("subtablecont")
  final val SubTableHeader     = strId("subtableheader")
  final val SubTableId         = strId("subtable")
  final val ToggleEditing      = strId("toggleediting")

  final val ColumnsDoc         = strId("columnsdoc")
  final val CombSearchDoc      = strId("combsearchdoc")
  final val CombiningQueriesDoc = strId("combiningqueries")
  final val CreateDoc          = strId("createdoc")
  final val DeleteDoc          = strId("deletedoc")
  final val Doc                = strId("doc")
  final val EditingDoc         = strId("editingdoc")
  final val ExplorerDoc        = strId("explorerdoc")
  final val ExportDoc          = strId("exportdoc")
  final val ExportSetDoc       = strId("exportsetdoc")
  final val ExportSettingsDoc  = strId("exportsettingsdoc")
  final val FileFormatsDoc     = strId("fileformatsdoc")
  final val FormattingDoc      = strId("formattingdoc")
  final val GridViewDoc        = strId("gridviewdoc")
  final val NavSearchDoc       = strId("navsearchdoc")
  final val NavigatorDoc       = strId("navigatordoc")
  final val NumQueryDoc        = strId("numquerydoc")
  final val ProjectsDoc        = strId("projectsdoc")
  final val QueryDoc           = strId("querydoc")
  final val QueryManagementDoc = strId("querymanagementdoc")
  final val QuickSearchDoc     = strId("quicksearchdoc")
  final val SingleQueriesDoc   = strId("singlerowqueries")
  final val StatisticsDoc      = strId("statisticsdoc")
  final val StringQueryDoc     = strId("stringquerydoc")
  final val TableViewDoc       = strId("tableviewdoc")
  final val TagSupDoc          = strId("tagsupdoc")
  final val TextQueryDoc       = strId("textquerydoc")
  final val UpdateDoc          = strId("updatedoc")
  final val UsersDoc           = strId("usersdoc")
  final val ValidateDoc        = strId("validatedoc")
  final val ViewsDoc           = strId("mainviewsdoc")
}

// vim: set ts=2 sw=2 et:
