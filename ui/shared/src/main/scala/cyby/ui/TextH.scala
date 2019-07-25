/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui

import cats.implicits._

import cyby.dat.format._
import cyby.dat._
import explorer._

import UId._
import cyby.ui.{WidgetType ⇒ WT, TitleType ⇒ TT, IconType ⇒ IT, CompType ⇒ CT,
                LabelType ⇒ LT}

import msf.js.{InputType, SelectEntry, Node, raw, nodes, Attribute}
import InputType.{Text, Password}

/**
  * Wrapper trait defining many elements and widgets of the UI as
  * pure HTML strings. For moste interactive elements in the UI, their
  * visualization is completely disconnected from their behavior:
  * They are defined in this trait as HTML strings (visualization).
  * All events are then caught at the toplevel and sent to the
  * UI's main signal function (together with the event source's ID). From
  * this ID the effect of a given event is derived in the different
  * modules. Eventually, it is our goal to design the whole UI like this,
  * leading to a highly responsive and fast user interface.
  * However, for some signal functions like the combined queries, this is a
  * non-trivial task, and we are not yet there.
  */
trait TextEnv extends LocEnv {
  object tags extends msf.js.Attributes[UIdP,CyByClass]{
    def idToString(i: UIdP): String = i.id

    def clsToString(c: CyByClass): String = c.toString

    /**
      * custom data type to mark certain elements as being hidden.
      */
    val dhidden: Mod[Boolean] = dataBool("hidden")

    /**
      * attribute to hide an element
      */
    val dhide: Attribute = dhidden := true

    /**
      * attribute to show (= unhide) an element
      */
    val dshow: Attribute = dhidden := false

    /**
      * custom data type to mark certain elements as being selected.
      */
    val dselected: Mod[Boolean] = dataBool("selected")

    def dexp(b: Boolean) = if (b) expanded else collapsed
    val collapsed = cls := Icon(IconType.Collapsed)
    val expanded = cls := Icon(IconType.Expanded)
  }

  import tags.{dhide, dshow, dexp, dhidden}

  /**
    * symbol for fields containing modification info (of type cyby.dat.EditInfo)
    */
  def modifiedSym: Symbol

  /**
    * symbol for fields containing creation time stamps (of type cyby.dat.TimeStamp)
    */
  def createdSym: Symbol


  def Txt: TextH

  /**
    * Helper class for creating Html Strings
    */
  trait TextH extends msf.js.TextHelper[UIdP,CyByClass] {

    override def clsToString(c: CyByClass) = c.toString

    override def idToString(uid: UIdP) = uid.toString

    /**
      * Info link leading to a part of the documentation as given
      * by ID t.
      */
    def info(t: UIdP): Node = a(
      cls := IT.Info.c,
      href := s"doc_${version}.html#${t}",
      target := "_blank"
    )()

    // -----------------------------------------------------------------
    // -----                Main Views                             -----
    // -----------------------------------------------------------------
   
    /**
      * Constructs CyBy's main view depending on the actual
      * EditMode
      */
    def mainView(mode: EditMode): Node =
      main(id := MainId, cls := CT.Main.c)(
        section(id := ExplorerId, cls := CT.Explorer.c)(
          explorer(mode)(nodes(),nodes(),nodes(),nodes()),
        ),
        section(id := QueriesId, cls := CT.Queries.c)(
          h1(cls := TT.BorderBtnTitle.c)(
            div(cls := CT.BorderTextCell.c)(text(loc.queries)),
            div(id := QueryUp, cls := IT.Up.c)(),
            div(id := QueryDown, cls := IT.Down.c)(),
            info(QueryDoc),
          ),
          div(id := QueryView, cls := Inner(CT.Queries))(),
        )
      )

    /**
      * Constructs the explorer window.
      *
      * @param export: content of the export view, where users can make
      *                choices about what fields to export
      * @param columns: content of the columns view, where users can make
      *                 choices about what columns are displayed and how they
      *                 are formatted
      * @param header: content of the explorer table header
      * @param subs:   content of the explorer table
      */
    def explorer(mode: EditMode)(
      export:  Node,
      columns: Node,
      header:  Node,
      subs:    Node,
    ): Node =
      nodes(
        h1(cls := TT.BorderBtnTitle.c)(
          text(loc.explorer),
          div(id := ExplorerZoomOut, cls := IT.Minus.c)(),
          div(id := ExplorerZoomIn, cls := IT.Plus.c)(),
          button(id := SubGridBtn, cls := WT.BorderBtn.c)(text(loc.grid)),
          button(id := SubTableBtn, cls := WT.BorderBtn.c)(text(loc.table)),
          button(id := SubStatsBtn, cls := WT.BorderBtn.c)(text(loc.stats)),
          button(id := EditExportBtn, cls := WT.BorderBtn.c)(text(loc.export)),
          // button(id := EditPlotBtn, cls := WT.BorderBtn.c)(loc.plots),
          input(Text, id := QuickSearchTxt, cls := WT.QuickTxt.c,
            placeholder := loc.quickSearch),
          div(
            id := QuickSearchEmpty,
            cls := IT.QuickExcludeEmpty.c,
            title := loc.includeEmpty,
          )(),
          div(
            id := QuickSearchCase,
            cls := IT.QuickCaseInsensitive.c,
            title := loc.caseSensitive,
          )(),
          div(cls := CT.ExplorerTitelFill.c)(),
          mode ifEditing div(id := Create(subType,rootPath), cls := IT.AddSub.c)(),
          div(
            id := ToggleEditing,
            cls := (if (mode.isEditing) IT.EditingEnabled.c
                   else IT.EditingDisabled.c),
            title := loc.toggleEditing
          )(),
          info(ExplorerDoc),
        ),
        ul(id := ExportId, cls := CT.NavCreateContainer.c)(export),
        ul(id := Columns, cls := CT.NavCreateContainer.c)(columns),
        ul(id := SubTableHeader, cls := CT.SubTable.c)(header),
        div(id := SubTableId, cls := Inner(CT.Explorer))(subs)
      )

    /**
      * Creates the title of the export view
      */
    def exportTitle: Node =
      li(cls := CT.NavRow.c)(
        h1(cls := TT.NavSection.c)(text(loc.exportSettings)),
        div(cls := CT.ExplorerTitelFill.c)(),
        info(ExportDoc),
      )

    /**
      * Creates the title of the column editing view
      */
    def columnsTitle: Node =
      li(cls := CT.NavRow.c)(
        h1(cls := TT.NavSection.c)(text(loc.addColumn)),
        div(cls := CT.ExplorerTitelFill.c)(),
        info(ColumnsDoc)
      )

    /**
      * Creates the side view on the left of the main screen
      *
      * @param c: credentials of the actually logged in user
      * @param changes: list of changes for the changelog
      * @param navSections: sections in the navigator
      */
    def leftView(
      c:           Creds,
      changes:     List[(String,String)],
      navSections: Node*
    ): Node =
      aside(id := SideViewId, cls := CT.SideView.c)(
        div(cls := CT.SideViewUpper.c)(
          section(cls := CT.LoginStatus.c)(
            h2(cls := TT.BorderBtnTitle.c)(
              div(cls := CT.BorderTextCell.c)(text(loc.loginStatus)),
              div(id := NavLeft, cls := IT.Left.c)(),
              div(id := NavRight, cls := IT.Right.c)(),
            ),
            div(id := LoginStatusId, cls := Inner(CT.LoginStatus))(
              p()(text(loc.loggedIn(alias(c.user), level(c.user)))),
              button(id := LogoutBtnId, cls := WT.LogoutBtn.c)(text(loc.logout))
            )
          ),
          navigator(c, changes, navSections: _*),
        ),
        section(cls := CT.Log.c)(
          h2(cls := TT.BorderTxt.c)(text(loc.log)),
          ul(id := LogId, cls := Inner(CT.Log))()
        ),
      )

    /**
      * Creates the navigator on the left of the main screen
      *
      * @param c: credentials of the actually logged in user
      * @param changes: list of changes for the changelog
      * @param navSections: sections in the navigator
      */
    def navigator(
      c:           Creds,
      changes:     List[(String,String)],
      navSections: Node*
    ): Node = section(id := NavId, cls := CT.Nav.c)(
       h1(cls := TT.BorderBtnTitle.c)(
         button(id := NavBtn, cls := WT.BorderBtn.c)(text(loc.navigator)),
         button(id := InfoBtn, cls := WT.BorderBtn.c)(text(loc.info)),
         div(cls := CT.ExplorerTitelFill.c)(),
         info(NavigatorDoc),
       ),
       div(cls := Inner(CT.Nav))(
         div(id := NavView)(navSections:_*),
         div(id := InfoView, dhidden := true)(
           div(cls := CT.InfoList.c)(
             changes.map{ case (d,msg) ⇒ 
               li(cls := CT.InfoRow.c)(
                 div(cls := CT.InfoDate.c)(text(d)),
                 div(cls := CT.InfoEntry.c)(text(msg))
               )
             }:_*
           )
         )
       )
     )


    // -----------------------------------------------------------------
    // -----                Navigator                              -----
    // -----------------------------------------------------------------

    /**
      * Section of data entries in the navigator
      *
      * @param de:   environment used to adjust how the navigator is displayed
      * @param dt:   the type of data being displayed in this part of
      *              the navigator. this is mainly used to create the appropriate
      *              element IDs.
      * @param path: the path to the data being displayed in this part of
      *              the navigator. this is mainly used to create the appropriate
      *              element IDs.
      * @param name: Localized name used in this navigator section's title
      * @param lvl:  User level required to create new entries.
      * @param rows: List of entries in this part of the navigator. This
      *              list will be hidden by default and can be expanded
      *              by clicking the corresponding icon.
      */
    def navSection(
      de     : DispEnv,
      dt     : DataType,
      path   : Path,
      name   : String,
      lvl    : UserLevel,
      rows   : Node*,
    ): Node = {
      val i = DataList(dt, path).i

      section(id := NavSectionId(dt))(
        div(cls := CT.NavRow.c)(
          div(id := Expand(i), dexp(de exp i))(),
          h2(cls := TT.NavSection.c)(text(name)),
          de.ifEditingAs(lvl)(
            div(id := Create(dt, path), cls := IT.AddHidden.c)()
          ),
          div(id := UId.Load(dt), cls := IT.Reload.c)()
        ),
        ul(id := CreateCont(dt, path), cls := CT.NavCreateContainer.c)(),
        ul(id := i, cls := CT.NavList.c, hide(de.exp, i))(rows: _*)
      )
    }

    /**
      * Row in a list of details about a data object in the navigator.
      *
      * @param path: the path to the data being displayed in this part of
      *              the navigator. this is mainly used to create the appropriate
      *              element IDs.
      * @param f:    Symbol representing the field being displayed in this row.
      *              This is both used to get a localized name for the information
      *              being displayed as well as a proper element ID if the
      *              information being displayed is editable.
      * @param v:    HTML string containing the actual information being displayed
      *              here.
      * @param de:   environment used to adjust how the navigator is displayed
      * @param editLvl: User level required to edit this piece of information
      *                 This is set to None, if the entry is not editable.
      */
    def navDetRow(
      p       : Path,
      f       : Symbol,
      v       : Node,
      de      : DispEnv,
      editLvl : Option[UserLevel],
    ): Node = {
      li(id := UId.Edit(f.name, p), cls := CT.NavDetailRow(f).c)(
        h4(cls := TT.NavDetail.c)(text(s"${loc name f}:")),
        if (editLvl exists de.isEditingAs)
          div(id := EditCont(f.name, p), cls := CT.NavDetailCell.c)(v)
        else div(cls := CT.NavDetailCell.c)(v)
      )
    }

    /**
      * Row in a list of details containing information about when
      * a piece of data was modified for the last time.
      */
    def editRow(inf: EditInfo): Node =
      li(cls := CT.NavDetailRow(modifiedSym).c)(
        h4(cls := TT.NavDetail.c)(text(s"${loc name modifiedSym}:")),
        div(cls := CT.NavDetailCell.c)(text(editStr(inf)))
      )

    /**
      * Row in a list of details containing information about when
      * a piece of data was created.
      */
    def createRow(ts: TimeStamp): Node =
      li(cls := CT.NavDetailRow(createdSym).c)(
        h4(cls := TT.NavDetail.c)(text(s"${loc name createdSym}:")),
        div(cls := CT.NavDetailCell.c)(text(timeStampStr(ts)))
      )

    /**
      * Properly formatted string of a timestamp.
      */
    def timeStampStr(ts: TimeStamp): String = {
      val d = mkDate(ts.v)
      val date = localeDateString(d)
      val time = localeTimeString(d)
      s"${date} ${time}"
    }

    def timeStampNode(ts: TimeStamp): Node = text(timeStampStr(ts))

    /**
      * Properly formatted string of date represented as milliseconds
      * since 1.1.1970.
      */
    def dateStr(v: Long): String = localeDateString(mkDate(v))

    def dateNode(v: Long): Node = text(dateStr(v))

    def editNode(inf: EditInfo): Node = text(editStr(inf))

    /**
      * Properly formatted string of editing information
      */
    def editStr(inf: EditInfo): String = {
      val user = inf.name.fold(loc.unknown)(_.v)

      s"${timeStampStr(inf.timestamp)} (${user})"
    }

    /**
      * Displays a value of the given type as an entry in the navigator,
      * creating a proper title and functionality for editing
      * and deleting.
      *
      * @param dets: creates detailed information about the item being displayed.
      * @param n   : name of the value being displayed. this is used in the
      *              item's title.
      * @param path: the path to the data being displayed in this part of
      *              the navigator. this is mainly used to create the appropriate
      *              element IDs.
      * @param s:    symbol representing the field to be edited when
      *              double clicking the item's title in the navigator
      *              (this is not always "name", for instance with users it
      *              might be "alias")
      * @param deleteLvl: User level required to delete this piece of information
      *                 Set this to None, if the entry cannot be deleted.
      * @param editLvl: User level required to edit this piece of information
      *                 Set this to None, if the entry is not editable.
      * @param search: Search modes available for this item. This results in
      *                list of icons being visible when hovering over the
      *                entry in question.
      */
    def dispNav[A](dets: (A,DispEnv) ⇒ Node)(
      n           : A ⇒ Name,
      path        : A ⇒ Path,
      s           : Symbol,
      deleteLevel : Option[UserLevel],
      editLevel   : Option[UserLevel],
      search      : List[NavSearchMode],
    ): DispEnv ⇒ A ⇒ Node = de ⇒ a ⇒ {
      val pth = path(a)
      val i = Dat(pth).i
      val se  = nodes(search map navSearchBtn(pth): _*)
      val del = if (deleteLevel exists de.isEditingAs)
                  div(id := DeleteId(pth), cls := IT.DeleteHidden.c)()
                else nodes()

      navItem(UId Item i)(
        div(cls := CT.NavRow.c)(
          expBtn(de.exp, i),
          navTitle(pth, n(a).v, de, s, editLevel),
          se,
          del
        ),
        dets(a, de)
      )
    }

    /**
      * creates an attribute for hiding or showing an element of
      * the given UIdP, depending on a set of ids of visible elements.
      */
    def hide(ids: Set[UIdP], i: UIdP) = if (ids(i)) dshow else dhide

    /**
      * Creates a list of detail entries.
      *
      * @param de:   environment used find out whether the list is actually
      *              expanded (aka visible).
      * @param path: the path to the data being displayed in this part of
      *              the navigator. this is mainly used to create the appropriate
      *              element IDs.
      */
    def navDets(de: DispEnv, p: Path)(ss: Node*): Node =
      ul(id := Dat(p), cls := CT.NavDetails.c, hide(de.exp, Dat(p)))(ss:_*)

    private def navItem(i: UIdP)(ss: Node*): Node =
      li(id := i, cls := CT.NavCell.c)(ss:_*)
   
    /**
      * Displays details about an entry in the navigator with
      * attached child entries
      *
      * @param path: the path to the data being displayed in this part of
      *              the navigator. this is mainly used to create the appropriate
      *              element IDs.
      * @param dt:   DataType representing the child elements
      * @param name: name of the data object being displayed
      * @param as:   list of child objects
      * @param de:   environment used to adjust how the navigator is displayed
      * @param titleSymbol:   symbol used for editing the appropriate field
      *                       when double clicking the title
      * @param deleteLevel:   required level to delete the data entry
      *                       (set to None if data cannot be deleted)
      * @param editLevel:     required level to edit the data entry's title
      *                       (set to None if title cannot be edited)
      * @param search: Search modes available for this item. This results in
      *                list of icons being visible when hovering over the
      *                entry in question.
      * @param mkChild: creates a HTML description for a given child
      * @param dets: details about the parent object.
      */
    def navChildrenDets[A](
      path                 : Path,
      dt                   : DataType,
      name                 : String,
      as                   : List[A],
      de                   : DispEnv,
      titleSymbol          : Symbol,
      deleteLevel          : Option[UserLevel],
      editLevel            : Option[UserLevel],
      search               : List[NavSearchMode],
    )(mkChild: DispEnv ⇒ A ⇒ Node)(dets: Node*): Node = {
      val i = DataList(dt,path).i

      navItem(UId Item i)(
        div(cls := CT.NavRow.c)(
          expBtn(de.exp, i),
          navTitle(path, name, de, titleSymbol, editLevel),
          nodes(search map navSearchBtn(path): _*),
          if (editLevel exists de.isEditingAs) {
            val del = if (deleteLevel exists de.isEditingAs)
                        div(id := DeleteId(path), cls := IT.DeleteHidden.c)()
                      else nodes()
            nodes(del, div(id := Create(dt,path), cls := IT.AddHidden.c)())
          } else nodes(),
        ),
        if (editLevel exists de.isEditingAs) 
          ul(id := CreateCont(dt,path), cls := CT.NavCreateContainer.c)()
        else nodes(),
        ul(id := i, cls := CT.NavDetails.c, hide(de.exp, i))(
          dets.toList ::: as.map(mkChild(de)): _*
        ),
      )
    }
   
    /**
      * Displays details about an entry in the navigator with
      * attached child entries
      *
      * @param path: the path to the data being displayed in this part of
      *              the navigator. this is mainly used to create the appropriate
      *              element IDs.
      * @param dt:   DataType representing the child elements
      * @param name: name of the data object being displayed
      * @param as:   list of child objects
      * @param de:   environment used to adjust how the navigator is displayed
      * @param titleSymbol:   symbol used for editing the appropriate field
      *                       when double clicking the title
      * @param deleteLevel:   required level to delete the data entry
      *                       (set to None if data cannot be deleted)
      * @param editLevel:     required level to edit the data entry's title
      *                       (set to None if title cannot be edited)
      * @param mkChild: creates a HTML description for a given child
      */
    def navChildren[A](
      path                 : Path,
      dt                   : DataType,
      name                 : String,
      as                   : List[A],
      de                   : DispEnv,
      titleSymbol          : Symbol,
      deleteLevel          : Option[UserLevel],
      editLevel            : Option[UserLevel],
    )(mkChild: DispEnv ⇒ A ⇒ Node): Node =
      navChildrenDets(path,dt,name,as,de,titleSymbol,deleteLevel,editLevel,Nil)(mkChild)()

    /**
      * Creates an editable title for an entry in the navigator
      *
      * @param p:    used in the element's ID to find out which item to
      *              edit if the item's name is double-clicked.
      * @param n:    name displayed in the title
      * @param s:    symbol representing the field to be edited when
      *              double clicking the title in the navigator
      *              (this is not always "name", for instance with users it
      *              might be "alias")
      * @param de:   environment used to adjust how the navigator is displayed
      * @param editLvl: user level required to edit the 
      */
    def navTitle(
      p         : Path,
      n         : String,
      de        : DispEnv,
      s         : Symbol,
      editLevel : Option[UserLevel],
    ) = if (editLevel exists de.isEditingAs)
          h3(id := UId.Edit(s.name, p), cls := TT.NavItem.c)(
            span(id := EditCont(s.name, p))(text(n))
          )
        else h3(cls := TT.NavItem.c)(span()(text(n)))

    /**
      * Editable detail entry for an editable link to another
      * data item.
      *
      * @param p: Path to the parent object
      * @param f: Symbol representing the field being displayed
      * @param l: The actual link being displayeed
      * @param de: environment used to adjust how the navigator is displayed
      * @param editLevel:     required level to edit the data entry's title
      *                       (set to None if title cannot be edited)
      */
    def linkRow[A](
      p         : Path,
      f         : Symbol,
      l         : (Id[A],Name),
      de        : DispEnv,
      editLevel : Option[UserLevel],
    ): Node = navDetRow(p, f, text(l._2.v), de, editLevel)

    /**
      * Editable list of links to other data items
      *
      * @param p: Path to the parent object
      * @param f: Symbol representing the field being displayed
      * @param l: The actual links being displayeed
      * @param de: environment used to adjust how the navigator is displayed
      * @param editLevel:     required level to edit the data entry's title
      *                       (set to None if title cannot be edited)
      */
    def linksRow[A](
      p         : Path,
      f         : Symbol,
      l         : List[(Id[A],Name)],
      de        : DispEnv,
      editLevel : Option[UserLevel],
    ): Node = navDetRow(p, f, text(l.map(_._2.v).mkString(", ")), de, editLevel)

    /**
      * Creates an icon for expanding an element in the UI
      *
      * @param ids : Set of expanded element IDs
      * @param i   : ID to be expanded or collapsed
      */
    def expBtn(ids: Set[UIdP], i: UIdP) = div(id := Expand(i), dexp(ids(i)))()

    private def navSearchBtn(p: Path)(m: NavSearchMode): Node = div(
      id    := NavSearch(p, m),
      cls   := IT.NavSearch(m).c,
      title := loc.navSearchTitle(m),
    )()

    // -----------------------------------------------------------------
    // -----                Explorer                               -----
    // -----------------------------------------------------------------
      
    /**
      * Creates the table header of the substance table from
      * the actual explorer environment. The result depends on the
      * mode the explorer is actually in: A grid view gets a different
      * header than the statistics table view for instance.
      */
    def mkHeadRow(expEnv: ExpEnv): Node = {
      import implicits.colEqI

      val expSt = expEnv.expSt
      val st = expEnv.st

      def columns = List(2,3,4,5,6,10,12,15,20).map(i ⇒ 
        SelectEntry(some(i.toString), i === expSt.gridColumns, i.toString)
      )

      def mkHeader(p: (Column, Int)) = {
        val (c, index) = p
        val pth = columnPath(c)

        def sortIcon = (c === expSt.query.sort, expSt.query.reverse) match {
          case (true,true)  ⇒ IT.SortDec.c
          case (true,false) ⇒ IT.SortInc.c
          case (false,_)    ⇒ IT.SortNone.c
        }

        val name = columnLoc(st)(c)

        val sortImg =
          if (canSortBy(c)) div(id := SortSub(pth), cls := sortIcon)()
          else nodes()

        lazy val formatI = div(
          id    := EditFormatId(pth),
          cls   := IT.EditFormat.c,
          title := loc.editFormatting,
        )()

        val formatImg = if (columnDesc(c).formattable) formatI else nodes()

        val addImg = div(
          id    := AddColumnId(index),
          cls   := IT.AddColumn.c,
          title := loc.addColumnTitle,
        )()

        val delImg = if (c === structCol) nodes() else div(
          id    := DelColumnId(index),
          cls   := IT.DeleteColumn.c,
          title := loc.deleteColumn,
        )()

        val textRow =
          if (c === structCol)
            li(cls := CT.ColumnTitleRow.c)(sortImg, text(name))
          else
            li(id := EditableColumn(index), cls := CT.ColumnTitleRow.c)(
              sortImg,
              text(name)
            )

        val colCls = columnDesc(c) match {
          case NoDesc         ⇒ CT.MethodStatsHeaderCell.c
          case StatsDesc      ⇒ CT.MethodStatsHeaderCell.c
          case StrDesc(n,_,_) ⇒ CT.SubHeaderCell(n).c
          case StructDesc     ⇒ CT.SubHeaderCell("structure").c
        }

        ul(cls := colCls)(
          textRow,
          li(cls := CT.ColumnIconRow.c)(formatImg, addImg, delImg),
        )
      }

      expSt.mode match {
        case SubstanceGrid ⇒ 
          li(cls := CT.GridHeaderRow.c)(
            lbl(GridColumns, LT.GridColumns, loc.numberOfColumns),
            select(id := GridColumns, cls := WT.GridColumnsSel.c)(options(columns)),
          )
        case PlotsView     ⇒ li(cls := CT.GridHeaderRow.c)(
          ul(id := UId.PlotEditList, cls := CT.PlotEditList.c)()
        )
        case _             ⇒ 
          nodes(
            div(
              id := CreateCont(subType,rootPath),
              cls := CT.NavCreateContainer.c
            )(),
            li(cls := CT.ExplorerHeaderRow.c)(
              expSt.columns.zipWithIndex map mkHeader: _*
            )
          )
      }
    }

    private def gridRow[S](mkId: S ⇒ UIdP)(ss: List[S])(implicit S: ToMol[S]): Node = {
      def cell(s: S) = div(id := mkId(s), cls := CT.SubGridCell.c)(
        raw(S svg s),
        div()(text(S id s toString))
      )

      div(cls := CT.SubGridRow.c)(ss map cell:_*)
    }

    /**
      * Creates the list of substance entries
      *
      * @param e: explorer environment affecting the columns displayed,
      *           editability of items, which nodes are expanded amonst other
      *           things.
      * @param inner: creates actual HTML row for a substance
      * @param mkId: creates an element id from a substance
      * @param subs: list of substances
      */
    def subItems[S:ToMol](
      e: ExpEnv,
      inner: ExpEnv ⇒ S ⇒ Node,
      mkId: S ⇒ UIdP,
    )(subs: List[S]): List[Node] = {

      def item(s: S) = div(id := mkId(s), cls := CT.SubEntry.c)(inner(e)(s))

      e.expMode match {
        case SubstanceGrid ⇒ subs.grouped(e.expSt.gridColumns).toList map gridRow(mkId)
        case _             ⇒ subs map item
      }
    }

    /**
      * CSS style declaration calculated from actual explorer settings
      * and the width of the substance table view. This is inserted
      * at the beginning of the HTML document whenever one of these
      * settings change.
      */
    def expStyle(env: ExpEnv, width: Int): Node = {
      def st = env.expSt
      val gridW = (width - st.gridColumns * 10) / st.gridColumns
      val expH = st.verticalSize max MinSize min MaxSize
      val expW = st.horizontalSize max MinSize min MaxSize
      val sideW = TotSize - expW
      val queryH = TotSize - expH

      raw(
        s"""
          .cell.sub.structure svg {
            width: ${st.rowH}px;
            height: ${st.rowH}px;
          }
          .cell.sub-header.structure { width: ${st.rowH + 10}px; }
          .cell.sub.structure { width: ${st.rowH + 10}px; height: ${st.rowH + 10}px }
          .row.explorer-sub { height: ${st.rowH + 10}px; }
          .cell.sub-grid svg {
            width: ${gridW}px;
            height: ${st.rowH}px;
          }
          .cell.sub-grid {
            height: ${st.rowH + 25}px;
            width: ${gridW + 10}px;
          }
          .comp.main.explorer { flex: ${expH}; }
          .comp.main.queries { flex: ${queryH}; }
          .comp.maincontent { flex: ${expW}; }
          .comp.sideview { flex: ${sideW}; }
        """
      )
    }

    private def colName(c: Column) = columnDesc(c) match {
      case NoDesc         ⇒ ""
      case StatsDesc      ⇒ ""
      case StrDesc(n,_,_) ⇒ n
      case StructDesc     ⇒ "structure"
    }

    private def subCellCls(c: Column) = columnDesc(c) match {
      case NoDesc      ⇒ CT.MethodStatsCell.c
      case StatsDesc   ⇒ CT.MethodStatsCell.c
      case _           ⇒ CT.SubCell(colName(c)).c
    }

    /**
      * Creates an editable cell in the substance table
      *
      * @param col : column of the cell
      * @param t   : text value to be displayed. this will be escaped, so
      *              do not try to render some HTML here. use rawEditableSubCell
      *              for this.
      * @param editLevel : user level needed for editing the value
      *                    (None if value is not editable)
      * @param env : explorer envirionment affecting look and behavior of
      *              the cell
      */
    def editableSubCell[S:ToMol](
      col       : Column,
      t         : S ⇒ String,
      editLevel : Option[UserLevel],
      env       : ExpEnv,
    ): S ⇒ Node =
      rawEditableSubCell[S](col, sub ⇒ text(t(sub)), editLevel, env)

    /**
      * Creates an editable cell in the substance table
      *
      * @param col : column of the cell
      * @param t   : text value to be displayed. this will not be escaped,
      *              so it can contain additional markup.
      * @param editLevel : user level needed for editing the value
      *                    (None if value is not editable)
      * @param env : explorer envirionment affecting look and behavior of
      *              the cell
      */
    def rawEditableSubCell[S:ToMol](
      col       : Column,
      t         : S ⇒ Node,
      editLevel : Option[UserLevel],
      env       : ExpEnv,
    ): S ⇒ Node = sub ⇒ {
      val pth = subPath(ToMol[S] id sub)
      val cn  = colName(col)

      if (editLevel exists env.isEditingAs)
        div(cls := subCellCls(col), id := UId.Edit(cn,pth))(
          div(id := EditCont(cn,pth))(t(sub))
        )
      else
        div(cls := subCellCls(col))(div()(t(sub)))
    }

    /**
      * Creates an editable cell for a boolean value in the substance table
      *
      * @param col : column of the cell
      * @param t   : value to be displayed
      * @param editLevel : user level needed for editing the value
      *                    (None if value is not editable)
      * @param env : explorer envirionment affecting look and behavior of
      *              the cell
      */
    def editableSubCellBool[S:ToMol](
      col       : Column,
      t         : S ⇒ Boolean,
      editLevel : Option[UserLevel],
      env       : ExpEnv,
    ): S ⇒ Node = sub ⇒ {
      val pth = subPath(ToMol[S] id sub)
      val cn  = colName(col)
      val c = if (t(sub)) IT.True.c else IT.False.c

      if (editLevel exists env.isEditingAs)
        div(cls := subCellCls(col), id := UId.Edit(cn,pth))(
          div(id := EditCont(cn,pth), cls := c)()
        )
      else
        div(cls := subCellCls(col))(div(cls := c)())
    }

    /**
      * Creates an non-editable cell for a boolean value in the substance table
      *
      * @param col : column of the cell
      * @param t   : value to be displayed
      */
    def subCellBool(col: Column, t: Boolean): Node = {
      val c = if (t) IT.True.c else IT.False.c
      div(cls := subCellCls(col))(div(cls := c)())
    }
    
    /**
      * Creates an non-editable cell for a value in the substance table
      *
      * @param col : column of the cell
      * @param t   : text value to be displayed. this will be escaped, so
      *              do not try to render some HTML here. use rawEditableSubCell
      *              for this.
      */
    def subCell(col: Column, t: String): Node = rawSubCell(col, text(t))

    /**
      * Creates an non-editable cell for a value in the substance table
      *
      * @param col : column of the cell
      * @param t   : text value to be displayed. this will not be escaped,
      *              so it can contain additional markup.
      */
    def rawSubCell(col: Column, t: Node): Node =
      div(cls := subCellCls(col))(t)

    /**
      * Creates an cell displaying the structure of a compound.
      * This includes icons for editing and deleting the compound, if
      * the UI is in editing mode.
      *
      * @param col : column of the cell
      * @param s   : compound whose structure is to be displayed
      * @param deleteLevel : user level required to delete the compound
      *                      (None if compound cannot be deleted)
      * @param editLevel : user level required to edit the compound's structure
      *                    (None if structure cannot be edited)
      * @param env : explorer envirionment affecting look and behavior of
      *              the cell
      */
    def structCell[S](
      col         : Column,
      s           : S,
      deleteLevel : Option[UserLevel],
      editLevel   : Option[UserLevel],
      env         : ExpEnv,
    )(implicit S: ToMol[S]): Node =
      if (editLevel exists env.isEditingAs) {
        val pth = subPath(S id s)
        val cn  = colName(col)
        val icons = div(cls := StructureIconCol)(
          div(id := ClickEdit(cn, pth), cls := IT.Edit.c)(),
          if (deleteLevel exists env.isEditingAs)
            div(id := DeleteId(pth), cls := IT.Delete.c)()
          else nodes()
        )
        div(id := UId.Edit(cn, pth), cls := subCellCls(col))(raw(S svg s), icons)
      } else div(cls := subCellCls(col))(raw(S svg s))

    /**
      * Displays a floating point value with correct background coloring
      * if some conditional formatting rules for the column are defined.
      *
      * @param c: column where the value is displayed
      * @param ov: optional floating point value to be displayed
      * @param expSt: explorer state containing all defined formatting rules
      */
    def gradientCell(c: Column, ov: Option[Double], expSt: ExpSt): Node =
      gradientCell(c, subCellCls(c), ov, expSt)

    /**
      * Displays a floating point value for a given statistic with correct background
      * coloring
      * if some conditional formatting rules for the column are defined.
      *
      * @param col: column where the value is displayed
      * @param tpe: type of stastic to be displayed
      * @param ov: optional list of values from which the statistic should be calculated
      * @param expSt: explorer state containing all defined formatting rules
      */
    def statsCell(
      col: Column,
      tpe: StatsType,
      stats: Option[Stats],
      expSt: ExpSt
    ): Node = gradientCell(
      col,
      CT.MethodStatsCell.c,
      stats map tpe.get,
      expSt,
      stats.map(_.values.toList mkString "\n")
    )

    /**
      * Displays a floating point value for with correct background
      * coloring if some conditional formatting rules for the column are defined.
      *
      * @param col: column where the value is displayed
      * @param c: CSS class of the cell
      * @param ov: optional floating point value
      * @param expSt: explorer state containing all defined formatting rules
      * @param tooltip: optional title attribute appearing when
      *                 hovering the mouse over the cell
      */
    def gradientCell(
      col:     Column,
      c:       CyByClass,
      ov:      Option[Double],
      expSt:   ExpSt,
      tooltip: Option[String] = None,
    ): Node = {
      lazy val form: Format[Double] =
        expSt.settings.doubleFormats.get(col) getOrElse Gradient[Double](Nil,0)

      lazy val color = for {
        v        <- ov
        c        <- Format.applyNum(v)(form)
      } yield style := s"background-color : $c"

      val atts = (cls := c) :: (title := tooltip.getOrElse("")) :: color.toList

      div(atts:_*)(text(ov.fold("")(Format.formatNum(_)(form))))
    }

    /**
      * Displayes one of the fields of a molecule in a cell in the
      * substance table.
      *
      * @param f: Field to be displayed
      * @param o: optional molecular structure (optional, since molecular
      *           structures might not be defined for all compounds
      * @param est: explorer state containing all kinds of information about
      *             the explorer and how to format and display values
      */
    def mol(f: Mol.Field, o: Option[Mol], est: ExpSt): Node = f match {
      case Mol.Mass ⇒ gradientCell(molFieldToColumn(f), o.map(_.mass), est)
      case Mol.ExactMass ⇒ gradientCell(molFieldToColumn(f), o.map(_.exactMass), est)
      case Mol.LogP ⇒ gradientCell(molFieldToColumn(f), o.flatMap(_.logP), est)
      case Mol.Tpsa ⇒ gradientCell(molFieldToColumn(f), o.flatMap(_.tpsa), est)
      case Mol.Lipinski ⇒ subCellBool(molFieldToColumn(f), o.flatMap(_.lipinski) getOrElse false)
      case Mol.Formula  ⇒ rawSubCell(molFieldToColumn(f), raw(o.fold("")(_.formulaHtml)))
      case Mol.Structure ⇒ nodes()
      case Mol.SubStructure ⇒ nodes()
      case Mol.ExactStructure ⇒ nodes()
      case Mol.NoStructure ⇒ nodes()
      case Mol.Svg ⇒ nodes()
      case Mol.Inchi ⇒ nodes()
      case Mol.Smiles ⇒ nodes()
    }

    /**
      * Creates a cell containing modification info
      *
      * @param c : column of the cell
      * @param e : modification info to be displayed
      * @param f : field to be displayed
      */
    def editInfo(c: Column, e: EditInfo, f: EditInfo.Field): Node = f match {
      case EditInfo.Summary   ⇒ subCell(c, s"""${timeStampStr(e.timestamp)} (${e.name.fold("")(_.v)})""")
      case EditInfo.UserId    ⇒ subCell(c, e.id.toString)
      case EditInfo.UserName  ⇒ subCell(c, e.name.fold("")(_.v))
      case EditInfo.Timestamp ⇒ subCell(c, timeStampStr(e.timestamp))
    }

    /**
      * Creates a cell containing info about a compound's time of creation
      *
      * @param c : column of the cell
      * @param ts : time of creation
      */
    def created(c: Column, ts: TimeStamp): Node = subCell(c, timeStampStr(ts))

    /**
      * Creates a link to a file stored at the server
      *
      * @param pth : path to the file (sans file name)
      * @param p : name of the file
      * @param c : user credentials (the caller is actually autheticated
      *            at the server and access rights are verified)
      */
    def fileLink(pth: Path, p: FileName, n: Name, c: Creds): Node = a(
      href   := s"""cyby-serv/$filType/${pth}/${p.v}${c.credsQ}""",
      target := "_blank"
    )(text(n.v))

    /**
      * Row of details about a data field in an expanded
      * view in the substance table
      *
      * @param p : path to parent type containing the displayed field
      * @param f : symbol representing the field being displayed
      * @param v : actual value being displayed
      * @param editLevel : user level needed to edit the field
      *                    (None if field is not editable)
      * @param env: explorer environment affecting editability of items
      *             and how values are displayed
      *
      */
    def conDetRow(
      p         : Path,
      f         : Symbol,
      v         : Node,
      editLevel : Option[UserLevel],
      env       : ExpEnv,
    ): Node = {
      div(id := UId.Edit(f.name, p), cls := CT.ConDetailRow(f).c)(
        h4(cls := TT.ConDetail.c)(text(s"${loc name f}:")),
        if (editLevel exists env.isEditingAs)
          div(id := EditCont(f.name, p), cls := CT.ConDetailCell.c)(v)
        else div(cls := CT.ConDetailCell.c)(v)
      )
    }

    /**
      * Row of details about a linked data object in an expanded
      * view in the substance table
      *
      * @param p : path to parent type containing the displayed field
      * @param f : symbol representing the field being displayed
      * @param l : value of the link being displayed.
      * @param editLevel : user level needed to edit the field
      *                    (None if field is not editable)
      * @param env: explorer environment affecting editability of items
      *             and how values are displayed
      *
      */
    def conLinkRow(
      p         : Path,
      f         : Symbol,
      l         : (Id[_],Name),
      editLevel : Option[UserLevel],
      env       : ExpEnv,
    ): Node = conDetRow(p, f, text(l._2.v), editLevel, env)

    /**
      * Row of details about a list of linked data objects in an expanded
      * view in the substance table
      *
      * @param p : path to parent type containing the displayed field
      * @param f : symbol representing the field being displayed
      * @param l : list of links being displayed.
      * @param editLevel : user level needed to edit the field
      *                    (None if field is not editable)
      * @param env: explorer environment affecting editability of items
      *             and how values are displayed
      *
      */
    def conLinksRow(
      p         : Path,
      f         : Symbol,
      l         : List[(Id[_],Name)],
      editLevel : Option[UserLevel],
      env       : ExpEnv,
    ): Node = conDetRow(p, f, text(l.map(_._2.v).mkString(", ")), editLevel, env)

    // -----------------------------------------------------------------
    // -----                Login                                  -----
    // -----------------------------------------------------------------
    
    /**
      * HTML formatted string of the login screen
      */
    lazy val login: Node =
      div(id := Login, cls := CT.LoginView.c, dshow)(
        img(cls := Logo, src := "css/images/cyby2.png")(),
        div(cls := CT.LoginTxt.c)(
          div(cls := CT.LoginRow.c)(
            lbl(LoginName, LT.Login, loc.userName),
            input(Text, id := LoginName, cls := WT.LoginTxt.c)
          ),
          div(cls := CT.LoginRow.c)(
            lbl(LoginPw, LT.Login, loc name "password"),
            input(Password, id := LoginPw, cls := WT.LoginTxt.c)
          ),
          button(id := LoginBtnId, cls := WT.LoginBtn.c)(text(loc.login)),
          ul(id := LoginLog, cls := CT.LoginList.c)(),
          div(cls := CT.DocLinkContainer.c)(
            a(href := s"doc_${version}.html", target := "_blank")(text(loc.documentation))
          ),
          div(cls := Copyright)(raw("&#169 Stefan Höck, Rainer Riedl (ZHAW)")),
        )
      )


    // -----------------------------------------------------------------
    // -----                Displaying Logs                        -----
    // -----------------------------------------------------------------

    /**
      * Displays a log message in the log view.
      */
    def log(l: Log): Node = li(cls := CT.LogRow.c)(
      div(cls := CT.LogLvlCell(l.lvl).c)(),
      div(cls := CT.LogMsgCell.c)(text(l.message))
    )

    /**
      * Displays a collection of log messages together in the log view.
      */
    def logs(ls: Vector[Log]): Node = nodes(ls map log: _*)

    /**
      * Displays a log message in the login screen
      */
    def loginLog(l: Log): Node = if (l.lvl < Warning) nodes() else log(l)


    // -----------------------------------------------------------------
    // -----                Util                                   -----
    // -----------------------------------------------------------------

    /**
      * Creates a label for a probably eitable field
      *
      * @param uid: ID used in "for" attribute
      * @param tpe: used to provide the CSS class
      * @param txt: actual textual content
      */
    def lbl(uid: UIdP, tpe: LabelType, txt: String): Node =
      label(id := Lbl(uid), forId := uid, cls := tpe.c)(text(txt))
  }
}

// vim: set ts=2 sw=2 et:
