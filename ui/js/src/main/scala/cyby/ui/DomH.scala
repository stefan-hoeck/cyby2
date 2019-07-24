/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui

import cyby.dat.{Add ⇒ _, _}

import cyby.query.{ReadPred ⇒ RP, Comp ⇒ QComp, _}
import cyby.dat.format.{Color, Format, Gradient}
import cyby.ui.chemdoodle.{Molecule, SketcherCanvas}

import cats.data.RWST
import cats.{Eq, Show}, cats.implicits.{none ⇒ _, _}

import cyby.ui.{WidgetType ⇒ WT, IconType ⇒ IT, LabelType ⇒ LT, CompType ⇒ CT}

import scala.util.matching.Regex

import msf.EF
import msf.js.{InputType ⇒ IPT, SelectEntry}
import org.scalajs.dom.html.Canvas

/**
  * Environment providing utility functions and widgets
  * often used when working with the DOM
  */
trait DomEnv extends CoreEnv {

  trait DomH[E,S,I]
    extends CyByH[E,S,I]
    with msf.js.JSDom[RWST[IO,E,Logs,S,?],UIdP,CyByClass,I] {
    import Src._

    /**
      * A validated query string
      */
    type QStr = Option[String]

    /**
      * Wraps a function, which creates a signal and
      * HTML element from an optional input value. This is, how
      * we usually describe interactive widgets in the UI:
      * Given an initial value
      * we create a HTML element (view of the value) and behavior
      * (signal).
      *
      * @tparam A : input type
      * @tparam B : output type (returned as a signal of options)
      * @tparam El : type representing the HTML element returned
      */
    case class WidgetDesc[El,A,B](
      pairO: Option[A] ⇒ Html[(El,Signal[Option[B]])]
    ){
      /**
        * Returns a new WidgetDesc, wrapping the HTML element
        * of the original one in another one.
        */
      def withinEl[El2<:Elem](el: Html[El2]): WidgetDesc[El2,A,B] =
        WidgetDesc(ini ⇒ for {
          e <- el
          s <- within(e)(signalO(ini))
        } yield e -> s)


      def pair(ini: A): Html[(El,Signal[Option[B]])] = pairO(some(ini))

      lazy val pair: Html[(El,Signal[Option[B]])] = pairO(None)

      lazy val swapped: Html[(Signal[Option[B]],El)] = pair map (_.swap)

      def signalO(ini: Option[A]): Html[Signal[Option[B]]] =
        pairO(ini) map (_._2)

      def signal(ini: A): Html[Signal[Option[B]]] = signalO(some(ini))

      lazy val signal: Html[Signal[Option[B]]] = signalO(None)

      def map[C](f: B ⇒ C): WidgetDesc[El,A,C] = mapO(b ⇒ some(f(b)))

      def mapEl[El2](f: El ⇒ El2): WidgetDesc[El2,A,B] =
        WidgetDesc(pairO(_) map { case (el,sig) ⇒ f(el) -> sig })

      def mapO[C](f: B ⇒ Option[C]): WidgetDesc[El,A,C] = WidgetDesc(
        ini ⇒ pairO(ini).map{ case (e,s) ⇒ e -> s.map(_ flatMap f) }
      )

      def mapP[C](pf: PartialFunction[B,C]): WidgetDesc[El,A,C] = mapO(pf.lift)

      def cmap[C](f: C ⇒ A): WidgetDesc[El,C,B] = cmapO(c ⇒ some(f(c)))

      def cmapO[C](f: C ⇒ Option[A]): WidgetDesc[El,C,B] =
        WidgetDesc(i ⇒ pairO(i flatMap f))

      def cmapP[C](pf: PartialFunction[C,A]): WidgetDesc[El,C,B] = cmapO(pf.lift)

      def opt: WidgetDesc[El,Option[A],B] = cmapO(identity)

      /**
        * Dynamic list of widgets
        *
        * @param ini : list of initial values
        * @param rowType : CSS class representing rows in the list
        * @param addContainer : optional element where the "Add" button
        *                       for creating new entries will be placed
        */
      def dynList(
        ini:          List[A],
        rowType:      RowType,
        addContainer: Option[Elem] = None
      ): Html[Signal[Option[List[B]]]] =
        dynamicListOf(signalO, rowType, addContainer)(ini)

      /**
        * Dynamic non-empty list of widgets
        *
        * @param ini : list of initial values
        * @param rowType : CSS class representing rows in the list
        */
      def dynNel(ini: Option[Nel[A]], rowType: RowType)
        : Html[Signal[Option[Nel[B]]]] =
        dynamicNelOf(signalO, rowType)(ini)

      /**
        * Uses dynList to create a new WidgetDesc for lists of
        * items
        *
        * @param ct : CSS class of the HTML element (a DIV) containing
        *             the dynamic list
        * @param rt : CSS class for the rows in the list
        */
      def list(ct: CompType, rt: RowType): WidgetDesc[Elem,List[A],List[B]] =
        WidgetDesc(as ⇒ for {
          d   <- div(cls := ct.c)
          s   <- within(d)(Ul(cls := CompType.DynamicEditList.c)(
                   dynList(as getOrElse Nil, rt)
                 ))
        } yield d -> s)

      /**
        * Uses dynNel to create a new WidgetDesc for non-empty lists of
        * items
        *
        * @param ct : CSS class of the HTML element (a DIV) containing
        *             the dynamic list
        * @param rt : CSS class for the rows in the list
        */
      def nel(ct: CompType, rt: RowType): WidgetDesc[Elem,Nel[A],Nel[B]] =
        WidgetDesc(as ⇒ for {
          d   <- div(cls := ct.c)
          s   <- within(d)(Ul(cls := CompType.DynamicEditList.c)(
                   dynNel(as, rt)
                 ))
        } yield d -> s)

      /**
        * Dynamic event switching: Whenever the signal of
        * this widget changes, a new signal of a given type
        * is created depending on the actual value of the
        * first widget.
        *
        * Consider the following use case: A selection box (dropdown
        * menu) holding a list of fields to query. Each field comes
        * with its own set of widgets necessary to define the query.
        * The initial signal comes from the selection box (signal
        * of selected field) and function "switch" is used
        * with a description of how to create a query widget
        * and its signal from a field value.
        *
        * @param that : function to create a new signal function plus
        *               its widget (that's why it is wrapped in Html)
        * @param aToC : function to get an optional helper value
        *               of type C (this is not strictly necessary, but
        *               it simplified code making use of this function)
        * @param wrapperClass : CSS class for the DIV wrapper, where the
        *                       additional HTML elements will be placed
        */
      def switch[C,D](
        that:         B ⇒ Option[C] ⇒ Html[Signal[Option[D]]],
        aToC:         A ⇒ Option[C],
        wrapperClass: CyByClass,
      )(implicit B: Eq[B]): WidgetDesc[El,A,D] = WidgetDesc(a ⇒
        for {
          p         <- pairO(a)
          (el,sigB) = p
          oc        = a flatMap aToC
          d         <- div(cls := wrapperClass)
        } yield el ->
          ef.switch(
            EF(sigB).distinct.mapF(
              b ⇒ at(d)(clear) *> at(d)(that(b)(oc)).map(EF.apply)
            )
          ).sf
      )
    }

    /**
      * Helper trait with some utility functions
      */
    trait WithWidgetType[A]{
      def widgetMod(f: WidgetType ⇒ WidgetType): A = widget(f(widget))

      def widget(w: WidgetType): A

      def widget: WidgetType

      def query: A = widgetMod(WT.Query)

      def edit: A = widgetMod(WT.Edit)

      def export: A = widgetMod(WT.Export)

      def format: A = widgetMod(WT.Format)
    }

    // -----------------------------------------------------------------
    // -----                   Labels                              -----
    // -----------------------------------------------------------------
    
    /**
      * Label accompanying an input element
      */
    def lbl(n: String, tpe: LabelType): Html[Elem] =
      label(cls := tpe.c, text(loc name n))

    /**
      * Label accompanying an input element
      */
    def lbl(s: Symbol): Html[Elem] =
      label(cls := LT.SymbolLabel(s).c, text(loc name s))



    // -----------------------------------------------------------------
    // -----                   Actions                             -----
    // -----------------------------------------------------------------
    
    /**
      * A clickable icon
      */
    def icon(i: IconType): Html[Action] = div(cls := i.c) map click[Elem]

    /**
      * A clickable icon for adding stuff
      */
    lazy val iconAdd: Html[Action] = icon(IT.Add)

    /**
      * A clickable icon for deleting stuff
      */
    lazy val iconDelete: Html[Action] = icon(IT.Delete)

    /**
      * A clickable icon for adding a set of parens to a combined query
      */
    lazy val iconParens: Html[Action] = icon(IT.Parens)

    /**
      * A clickable icon linking to a part of the documentation
      */
    def iconInfo(id: UIdP): Html[Elem] = a(
      cls := IT.Info.c,
      href := s"doc_${version}.html#${id}",
      target := "_blank"
    )

    /**
      * Event stream that fires only when either
      * an OK or CANCEL action fires. The return type is
      * wrapped in an Option depending on whether OK or CANCEL fired.
      */
    def confirmed[A](s: Signal[Option[A]])
      (ok: Action, cancel: Action): Src[Option[A]] =
      s.zip(ok.sf).collect{ case (Some(a),Some(_)) ⇒ some(a) } <|>
      cancel.as(none[A])

    /**
      * Takes a (validated) input signal and displays
      * two buttons in a row in order to a) confirm user input or
      * b) cancel user input
      */
    def withConfirmButtons[A](sig: Signal[Option[A]]): Html[Src[Option[A]]] =
      Li(cls := CT.NavRow.c)((icon(IT.Confirm), icon(IT.Cancel))
        .mapN(confirmed(sig)))
   
    /**
      * Displays an OK and a CANCEL button (icon) in a row
      * and returns an event stream signalling the
      * button that was clicked.
      */
    def mkConfirmButtons[A]: Html[Src[Option[Unit]]] =
      withConfirmButtons(const(some(unit)))



    // -----------------------------------------------------------------
    // -----                   Input                               -----
    // -----------------------------------------------------------------

    /**
      * WidgetDesc for an input field representing a value of type A.
      */
    case class InputDesc[A](
      /** function to parse a value of the desired type */
      read:    String ⇒ Option[A],

      /** function to display initial values */
      show:    A ⇒ String,

      /** message to be displayed in case of a parsing error */
      msg:     String ⇒ String,

      /** CSS class of the input element */
      widget:  WT,

      /** type of the input element */
      tpe:     IPT,

      /** optional default initial value */
      default: Option[A]
    ) extends WithWidgetType[InputDesc[A]] {
      def desc: WidgetDesc[Input,A,A] = WidgetDesc(v ⇒
        pairIni(v.orElse(default).fold("")(show))
      )

      def pairIni(ini: String): Html[(Input,Signal[Option[A]])] =
        input(tpe, value := ini, cls := widget.c)
          .map{ e ⇒ e -> valueValidated(read, msg)(e) }

      def signalIni(ini: String): Html[Signal[Option[A]]] =
        pairIni(ini) map (_._2)

      def widget(wt: WT): InputDesc[A] =
        InputDesc(read, show, msg, wt, tpe, default)

      def tpe(it: IPT): InputDesc[A] =
        InputDesc(read, show, msg, widget, it, default)

      def bimap[B](to: A ⇒ B, from: B ⇒ A): InputDesc[B] =
        InputDesc[B](read(_) map to, b ⇒ show(from(b)), msg, widget,
          tpe, default map to)

      def optional: InputDesc[Option[A]] =
        InputDesc[Option[A]](
          s ⇒ if (s.isEmpty) some(none[A]) else read(s) map some,
          _.fold("")(show),
          msg, widget, tpe, some(none[A])
        )

      def revalidate(f: A ⇒ Option[A], msg: String ⇒ String): InputDesc[A] =
        InputDesc[A](read(_) >>= f, show, msg, widget, tpe, default >>= f)
    }

    /**
      * Creates a text input field
      */
    def txtRead[A:Read:Show](msg: String ⇒ String): InputDesc[A] =
      InputDesc(Read[A].read, Show[A].show, msg, WT.Text, IPT.Text, None)

    /**
      * Creates a text input field with a default initial value
      */
    def txtReadDef[A:Read:Show](msg: String ⇒ String, d: A): InputDesc[A] =
      InputDesc(Read[A].read, Show[A].show, msg, WT.Text, IPT.Text, some(d))

    /**
      * Text input for entering user aliases
      */
    lazy val alias: InputDesc[Alias] = txtRead[Alias](loc.aliasMsg)

    /**
      * Text input for entering amounts of compounds
      */
    lazy val amount: InputDesc[Amount] = txtReadDef(loc.amountMsg, Amount.default)

    /**
      * Text input for entering cas numbers
      */
    lazy val casNr: InputDesc[CasNr] = txtRead[CasNr](loc.casNrMsg)

    /**
      * Color input
      */
    lazy val color: InputDesc[Color] = InputDesc(Read[Color].read,
      _.toString, loc.colorMsg, WT.Color, IPT.Color, None)

    /**
      * Text input for entering concentration values
      */
    lazy val concentration: InputDesc[Concentration] =
      txtReadDef(loc.concentrationMsg, Concentration.default)

    private def readDate(s: String): Option[Date] =
      tryO(Date mk (new JSDate(s)).getTime.toLong)

    /**
      * Text input for date values
      */
    lazy val date: InputDesc[Date] = {
      def show(v: Date) = {
        val d = new JSDate(v.v)
        f"${d.getFullYear}-${d.getMonth + 1}%02d-${d.getDate}%02d"
      }

      InputDesc(readDate, show, loc msg "date", WT.Date, IPT.Date, None)
    }

    /**
      * Text input for entering density values
      */
    lazy val density: InputDesc[Density] =
      txtReadDef(loc.densityMsg, Density.default).widget(WT.Number)

    /**
      * Text input for entering floating point values
      */
    lazy val double: InputDesc[Double] =
      txtReadDef(loc.doubleMsg, 0D).widget(WT.Number)

    /**
      * Text input for entering filenames
      */
    lazy val fileName: InputDesc[FileName] = txtRead[FileName](loc.fileNameMsg)

    /**
      * Text input for ints
      */
    lazy val int: InputDesc[Int] = txtReadDef(loc.intMsg, 0).widget(WT.Number)

    /**
      * Text input for longs
      */
    lazy val long: InputDesc[Long] = txtReadDef(loc.longMsg, 0L).widget(WT.Number)

    /**
      * Text input for Names
      */
    lazy val nameIn: InputDesc[Name] = txtRead[Name](loc.nameMsg)

    /**
      * Password input
      */
    lazy val password: InputDesc[Password] =
      txtRead[Password](loc.passwordMsg).tpe(IPT.Password)

    /**
      * Text input for percent values
      */
    lazy val percent: InputDesc[Percent] =
      txtReadDef(loc.percentMsg, Percent.default).widget(WT.Number)

    /**
      * Text input for plain strings (no control characters)
      */
    lazy val plain: InputDesc[Plain] = txtReadDef(loc.plainMsg, Plain.default)

    /**
      * Text input for strings
      */
    lazy val string: InputDesc[String] = txtReadDef(_ ⇒ "", "")
    
    /**
      * Creates a text input field returning a signal function
      * representing number of digits in conditional formatting
      */
    def numberOfDigits(n: Int): Html[Signal[Option[Int]]] =
      lbl("nod", LT.Nod) *> int.format.desc.signal(n)

    /**
      * Creates a text input for defining queries
      */
    def txtQ[A](rp: RP[A]): WidgetDesc[Unit,String,String] =
      string.revalidate(s ⇒ rp(s) as s, loc.predicateMsg)
            .query.desc.mapEl(_ ⇒ unit)
      


    // -----------------------------------------------------------------
    // -----                   CheckBoxes                          -----
    // -----------------------------------------------------------------
    
    /**
      * WidgetDesc for checkboxes. For easier styling, all checkboxes
      * are wrapped in a DIV.
      */
    case class CheckBoxDesc(
      /**
        * CSS class of the checkbox itself
        */
      widget: WT, 

      /**
        * CSS class of the wrapper DIV element around the
        * checkbox
        */
      comp:   CompType
    ) extends WithWidgetType[CheckBoxDesc] {
      def widget(wt: WT) = CheckBoxDesc(wt, comp)

      def desc: WidgetDesc[Input,Boolean,Boolean] = WidgetDesc(v ⇒ 
        Div(cls := comp.c)(
          checkbox(cls := widget.c, checked := v.getOrElse(false))
            .map{ e ⇒ e -> Src.checked(e).map(some) }
          )
      )
    }

    /**
      * Creates a description of a checkbox with the given
      * CSS class for the outer DIV.
      */
    def checkBox(comp: CompType): CheckBoxDesc =
      CheckBoxDesc(WT.CheckBox, comp)
    

    // -----------------------------------------------------------------
    // -----                   Selects                             -----
    // -----------------------------------------------------------------
    
    /**
      * WidgetDesc for select elements (comboboxe or dropdown menues).
      */
    case class SelectDesc[A](
      /**
        * List of possible values. Use a String wrapped in a
        * Left for headings and a pair of a value and a
        * localized String for selectable values.
        */
      values: List[Either[String,(A,String)]],

      /**
        * Function used to compare two values for equivalence
        */
      eqv:    A ⇒ A ⇒ Boolean,

      /**
        * CSS class used for the select box widget
        */
      widget: WT,

      /**
        * optional default value selected if no initial value
        * is given
        */
      default: Option[A] = None,
    ) extends WithWidgetType[SelectDesc[A]] {
      self ⇒

      private lazy val pairs = values.zipWithIndex

      private def get(s: String) =
        Read[Int] read s flatMap (values(_).toOption.map(_._1))

      def desc: WidgetDesc[Select,A,A] = WidgetDesc(pairO)

      def pairO(iniO: Option[A]): Html[(Select,Signal[Option[A]])] = {
        val ini = iniO orElse default
        val es = pairs map {
          case (Left(str),i)       ⇒ SelectEntry(None,false,str)
          case (Right((a,str)),i)  ⇒
            SelectEntry(Some(i.toString), ini exists eqv(a), str)
        }

        combo(es, widget.c) map { e ⇒ e -> Src.select(e).map(get) }
      }

      def widget(wt: WT): SelectDesc[A] = SelectDesc(values, eqv, wt)

      /**
        * Event stream firing whenever the selected element has
        * changed
        */
      def onChange(ini: Option[A]): Html[Src[Option[A]]] =
        pairO(ini) map { case (e,s) ⇒ s on Src.change(e) }

      /**
        * Use several select boxes to choose
        * a non-empty path in a homogeneous tree.
        * The first select box is at the root of the tree.
        * Changing the selected value adds another selection
        * box with 
        *
        */
      def tree(
        /**
          * Child values depending on the selected parent value.
          */
        vals        : A ⇒ List[Either[String,(A,String)]],

        /**
          * CSS class for the outer DIV element wrapping the
          * list of select elements.
          */
        outerClass  : CyByClass,

        /**
          * CSS class used for inner wrapper elements created
          * when the selection in the existing selection boxes changes.
          */
        innerClass  : CyByClass,
      )(implicit A: Eq[A]): WidgetDesc[Elem,Nel[A],Nel[A]] = {
        def odesc(vs: List[Either[String,(Option[A],String)]]) =
          SelectDesc[Option[A]](
            Right(none[A] -> "") :: vs, oa ⇒ ob ⇒ oa === ob, widget
          )

        def wrapper = div(cls := innerClass)

        def tailSig(oa: Option[A], ini: Option[List[A]]):
          Html[Signal[Option[List[A]]]] = oa match {
            case None    ⇒ Src.sigHtml(some(nil[A]))
            case Some(a) ⇒ vals(a).map2{ case (a,s) ⇒ some(a) -> s} match {
              case Nil ⇒ Html pure const(some(nil[A]))
              case vs  ⇒ for {
                sig <- odesc(vs).desc.signal(ini flatMap (_.headOption))
                d   <- wrapper
              } yield EF.switch(
                sig.collectO(identity).distinct.mapF{ oa2 ⇒ 
                  at(d)(
                    clear *> tailSig(oa2, ini flatMap tailOption)
                               .map3(as ⇒ oa2.fold(as)(_::as))
                               .map(EF.apply)
                  )
                }
              ).sf
            }
          }

        def pairO(ini: Option[Nel[A]]): Html[(Elem,Signal[Option[Nel[A]]])] = for {
          w     <- div(cls := outerClass)
          sig   <- within(w)(desc.signalO(ini map (_.head)))
          inner <- within(w)(wrapper)
        } yield w -> EF.switch(
          sig.collectO(identity).distinct.mapF{ a ⇒ 
            at(inner)(
              clear *> tailSig(some(a), ini map (_.tail))
                         .map3(as ⇒ Nel(a,as))
                         .map(EF.apply)
            )
          }
        ).sf

        lazy val res: WidgetDesc[Elem,Nel[A],Nel[A]] = WidgetDesc(pairO)

        res
      }
    }

    /**
      * Factory method for SelectDesc
      *
      * @param vs : list of possible values
      * @param wt : CSS class of the select box
      * @param disp : returns a localized string representation for
      *               selectable values
      */
    def selDesc[A:Eq](vs: List[A], wt: WT)(disp: A ⇒ String): SelectDesc[A] =
      selDescE(vs map Right.apply, wt)(disp)

    /**
      * Factory method for SelectDesc (CSS class is "select link")
      *
      * @param vs : list of possible values
      * @param disp : returns a localized string representation for
      *               selectable values
      */
    def link[A:Eq](vs: List[A])(disp: A ⇒ String): SelectDesc[A] =
      selDesc(vs, WT.DataLinkSel)(disp)

    /**
      * Factory method for SelectDesc (CSS class is "select link")
      *
      * @param vs : list of possible values or headings
      * @param disp : returns a localized string representation for
      *               selectable values
      */
    def linkE[A:Eq](vs: List[Either[String,A]])(disp: A ⇒ String): SelectDesc[A] =
      selDescE(vs, WT.DataLinkSel)(disp)

    /**
      * Factory method for SelectDesc
      *
      * @param vs : list of possible values or headings
      * @param wt : CSS class of the select box
      * @param disp : returns a localized string representation for
      *               selectable values
      */
    def selDescE[A:Eq](vs: List[Either[String,A]], wt: WT)(disp: A ⇒ String): SelectDesc[A] =
      SelectDesc[A](vs.map2(a ⇒ a -> disp(a)), a1 ⇒ a2 ⇒ a1 === a2, wt)

    /**
      * Creates a SelectDesc for enumerations
      *
      * @param wt : CSS class of the select box
      * @param disp : returns a localized string representation for
      *               selectable values
      */
    def selDescEnum[A:Eq:Enum](wt: WT, disp: A ⇒ String): SelectDesc[A] =
      selDescEnumP[A](_ ⇒ true)(wt, disp)

    /**
      * Creates a SelectDesc for a subset of the possible values
      * of an enumeration
      *
      * @param p : Predicate used to filter values. Only values where p returns
      *       true will be listed in the select box.
      * @param wt : CSS class of the select box
      * @param disp : returns a localized string representation for
      *               selectable values
      */
    def selDescEnumP[A:Eq:Enum](p: A ⇒ Boolean)(wt: WT, disp: A ⇒ String)
      : SelectDesc[A] = selDesc[A](Enum[A].values.toList filter p, wt)(disp)


    private lazy val bools: List[Boolean] = List(true, false)

    /**
      * SelectDesc for booleans
      */
    lazy val bool: SelectDesc[Boolean] = 
      selDesc(List(false, true), WT.BoolSel)(loc.bool)

    /**
      * SelectDesc for user levels
      */
    lazy val userLevel: SelectDesc[UserLevel] =
      selDesc(UserLevel.values, WT.UserLevelSel)(loc.dispUserLevel)
      
    /**
      * SelectDesc for export formats
      */
    lazy val exportFormat: SelectDesc[export.Format] =
      selDescEnum(WT.ExportFormatSel, loc.exportFormat)

    /**
      * SelectDesc for query comparators
      */
    lazy val comparator: SelectDesc[QComp] =
      selDescEnum(WT.ComparatorSel, loc.queryComp)

    /**
      * SelectDesc for query negators
      */
    lazy val negator: SelectDesc[Boolean] =
      selDesc(bools.reverse, WT.NegatorSel)(loc.negators)

    /**
      * SelectDesc for types of statistics
      */
    lazy val statsType: SelectDesc[StatsType] =
      selDescEnum[StatsType](WT.StatsSel, _ locName loc)


    // -----------------------------------------------------------------
    // -----             Editing Lists                             -----
    // -----------------------------------------------------------------
    
    /**
      * Dynamic row in an editable list of elements.
      *
      * Takes a Signal "factory" and returns
      * a factory for the same signal together with a "delete button"
      * that can be used to remove the value edited in the original
      * signal.
      */
    def dynamicRow[A](rowType: RowType)(makeSignal: Html[Signal[A]])
      : Html[(Signal[A],Action)] = for {
        l      <- li(cls := rowType.c)
        signal <- within(l)(makeSignal)
        cancel <- within(l)(iconDelete)
      } yield (signal, cancel asF at(l)(remove))
    
    /**
      * Dynamic list of editable rows
      *
      * There is a source of new signal functions which appends
      * a "row" in the editable list upon a user action,
      * and there is a factory of signal function used
      * to create the signals of initial values.
      *
      * @param mk : Creates a an element (for instance an "Add" button)
      *             producing new signal functions.
      * @param sf : creates a new signal from an optional input value
      * @param rowType : CSS class of row entries
      * @param ini : initial list of values
      */
    def dynamicList[A,B](
      mk:      Html[Src[Html[Signal[B]]]],
      sf:      A ⇒ Html[Signal[B]],
      rowType: RowType
    )(ini: List[A]): Html[Signal[List[B]]] = dynlistF(
      mk map2 dynamicRow(rowType),
      ini map sf traverse dynamicRow(rowType)
    )

    /**
      * Dynamic list of editable rows
      *
      * An "Add" button is created, which - when clicked -
      * creates new signals. This "Add" button is
      * appended to an optional Html element.
      *
      * Every row in the list also comes with a "Delete" button
      * for removing the row and thus adjusting the number of
      * elements in the resulting signal.
      *
      * @param signal : creates a new signal from an optional input value
      * @param rowType : CSS class of row entries
      * @param addContainer : optional wrapper element where the "Add"
      *                       button will be placed.
      * @param ini : initial list of values
      */
    def dynamicListOf[A,B](
      signal:       Option[A] ⇒ Html[Signal[Option[B]]],
      rowType:      RowType,
      addContainer: Option[Elem] = None
    )(ini: List[A]): Html[Signal[Option[List[B]]]] = {
      val btn = iconAdd.map2(_ ⇒ signal(None))

      val src =
        addContainer.fold(Li(cls := CT.CombosAdderRow.c)(btn))(within(_)(btn))

      dynamicRowsO(src, (a: A) ⇒ signal(some(a)), rowType)(ini)
    }
    
    /**
      * Dynamic non-empty list of editable rows
      *
      * An "Add" button is created, which - when clicked -
      * creates new signals.
      *
      * @param signal : creates a new signal from an optional input value
      * @param rowType : CSS class of row entries
      * @param ini : initial list of values
      */
    def dynamicNelOf[A,B](
      signal:  Option[A] ⇒ Html[Signal[Option[B]]],
      rowType: RowType,
    )(ini: Option[Nel[A]]): Html[Signal[Option[Nel[B]]]] = for {
      l <- li(cls := rowType.c)
      h <- within(l)(signal(ini map (_.head)))
      t <- dynamicListOf(signal, rowType, some(l))(ini.fold(nil[A])(_.tail))
    } yield (h,t).mapN2(Nel(_,_))

    /**
      * Dynamic list of items from an event stream of signal 
      * functions
      *
      * @param mk : Creates a an element (for instance an "Add" button)
      *             producing new signal functions.
      * @param sf : creates a new signal from an optional input value
      * @param rowType : CSS class of row entries
      * @param ini : initial list of values
      */
    def dynamicRowsO[A,B](
      mk: Html[Src[Html[Signal[Option[B]]]]],
      sf: A ⇒ Html[Signal[Option[B]]],
      rowType: RowType
    )(ini: List[A]): Html[Signal[Option[List[B]]]] =
      dynamicList(mk, sf, rowType)(ini) map2 (_.sequence)


    // -----------------------------------------------------------------
    // -----             Molecules                                 -----
    // -----------------------------------------------------------------

    /**
      * WidgetDesc for editable molecules
      * (since a window for drawing moleculs can hold several
      * compounds, the value type ist List[Molecule])
      */
    case class MolDesc(w: Int, h: Int, comp: CompType) {
      def desc: WidgetDesc[Elem,Mol,List[Molecule]] = WidgetDesc(pairO)

      def pairO(ini: Option[Mol]): Html[(Elem,Signal[Option[List[Molecule]]])] = for {
        d         <- div(cls := comp.c)
        drawI     <- unique
        c         <- within(d)(molCanvas(drawI, w, h))
        sketch    <- Html liftE sketcher(drawI, ini map (_.structure), w, h)
      } yield d -> constS(sketch traverse getMols map (_.flatten))
    }

    def getMols(c: SketcherCanvas): Eff[Option[List[Molecule]]] =
      Eff delayedTryO c.getMolecules map2 (_.toList)

    def editMol(w: Int, h: Int, ini: Option[MolFile]): Html[Signal[Option[MolFile]]] = for {
      drawContI <- unique
      dispContI <- unique
      drawI     <- unique
      dispI     <- unique
      conf      <- Ul(id := drawContI, molCanvas(drawI, w, h))(mkConfirmButtons)
      _         <- div(id := dispContI, molCanvas(dispI, w, h))
      sketch    <- Html liftE sketcher(drawI, ini, w, h, false, true, false, false)
      hideId    =  if (ini.isEmpty) dispContI else drawContI
      _         <- within(hideId)(set attribute tags.dhide)

      mol    = Eff.delayed(sketch map (_.getMolecule))
      setmol = mol >>= (om ⇒ om.fold(Eff.unit)(dispMol(dispI)) as om)
      query  = (om: Option[Molecule]) ⇒ Eff.pure(om flatMap writeMol)

      _         <- ini traverse_ (s ⇒ Html liftE dispMolS(dispI)(s))

      hideSF = dblclick(dispI) >>- (hide(dispContI) |+| show(drawContI))

      confEF = conf.collectF{
                 case None    ⇒ mol    >>= query
                 case Some(_) ⇒ setmol >>= query
               } >>* (hide(drawContI) |+| show(dispContI))

    } yield (idS[Ev] >>* hideSF >-> confEF).holdF(mol >>= query)

    private def molCanvas(i: UIdP, w: Int, h: Int): Html[Canvas] =
      canvas(id := i, cwidth := w, cheight := h)

    // -----------------------------------------------------------------
    // -----             Formats                                   -----
    // -----------------------------------------------------------------

    private def gradientRow(v: Option[GradientEntry])
      : Html[Signal[Option[GradientEntry]]] = (
        double.format.desc.signalO(v.map(_._1)),
        color.format.desc.signalO(v.map(_._2)),
      ).mapN2((_,_))

    /**
      * List of values and colors describing a gradient
      * used in the conditional formatting
      *
      * @param st : the actual state of values loaded in the UI
      * @param c  : column to which the formatting rules should
      *             be applied
      * @param v  : initial gradient
      */
    def gradient(st: St, c: Column, v: Gradient[Double])
      : Html[Signal[Option[Format[Double]]]] = for {
        nod <- Li(cls := CT.NavEditRowStr("nod").c)(numberOfDigits(v.nod))
        g   <- Li(
                 cls := CT.NavEditRowStr("gradient").c,
                 lbl("gradient", LabelType.Gradient),
               )(
                 Div(cls := CT.ListEditContainer.c)(
                   Ul(cls := CT.DynamicEditList.c)(
                     for {
                       l <- li(cls := CT.CombosAdderRow.c)
                       g <- dynamicListOf(
                              gradientRow,
                              CT.DynamicEditRow,
                            some(l))(v.pairs)
                     } yield g
                   )
                 )
               )
      } yield (g,nod).mapN2(Gradient[Double](_,_) : Format[Double])

    // -----------------------------------------------------------------
    // -----             Query                                     -----
    // -----------------------------------------------------------------

    /**
      * dummy representing a non-existing query component
      */
    lazy val noQ: WidgetDesc[Unit,String,String] =
      WidgetDesc(_ ⇒ Html.pure(unit -> const(none[String])))

    /**
      * dummy returning always the empty string without a visible widget
      */
    lazy val emptyQ: WidgetDesc[Unit,String,String] =
      WidgetDesc(_ ⇒ Html.pure(unit -> const(some(""))))

    /**
      * Select of string query prefixes (==, !=, ..., and "contains")
      */
    lazy val stringQueryPrefix: SelectDesc[String] =
      selDesc(StringQPrefixes, WT.StringQPrefixSel)(loc.stringQueryPrefix)

    /**
      * Select of string query prefixes (==, !=, ...)
      */
    lazy val queryPrefix: SelectDesc[String] =
      selDesc(QPrefixes, WT.QPrefixSel)(identity)

    private def getPrefixO(os: Option[String]): (Option[String],Option[String]) =
      os.fold(none[String] -> none[String])(getPrefix)

    private def getPrefix(s: String): (Option[String],Option[String]) =
      s.split(" ", 2).toList match {
        case List(a,b) ⇒ Some(a) -> Some(b)
        case _         ⇒ None -> None
      }

    /**
      * Widgets for editing string-based queries
      */
    lazy val stringQ: WidgetDesc[Unit,String,String] = WidgetDesc{os ⇒
      val (op,oq) = getPrefixO(os)

      for {
        pre <- stringQueryPrefix.query.desc.signalO(op)
        p   <- string.query.desc.pairO(oq)
      } yield unit -> (pre, p._2).mapN(readSQ(p._1)).mapF(identity)
    }

    /**
      * Widgets for editing string-based queries for optional
      * values
      */
    lazy val stringQO: WidgetDesc[Unit,String,String] = WidgetDesc{os ⇒
      val (op,oq) = getPrefixO(os)

      for {
        pre <- stringQueryPrefix.query.desc.signalO(op)
        p   <- string.query.desc.pairO(oq)
      } yield unit -> (pre, p._2).mapN(readSQO(p._1)).mapF(identity)
    }

    private def readSQO(e: Elem)(oq: QStr, os: QStr): Eff[QStr] =
      (oq,os getOrElse "") match {
        case (Some("=="), "") ⇒ Eff pure some(s"${Undefined}")
        case (oq,s)           ⇒ readSQ(e)(oq, os)
      }

    private def readSQ(e: Elem)(pre: QStr, que: QStr): Eff[QStr] = {
      val s = que getOrElse ""
      pre match {
        case Some(Contains) ⇒ Read[Regex].read(s) match {
          case None ⇒ at(e)(set customValidity loc.msg("regex") as none[String])
          case _ ⇒ at(e)(set customValidity (_ ⇒ "") as some(s"${Contains} ${s}"))
        }

        case oq ⇒ Eff pure oq.map(q ⇒ s"${q} ${s}")
      }
    }

    /**
      * Widgets for editing boolean queries
      */
    lazy val boolQ: WidgetDesc[Unit,String,String] =
      bool.query.desc.map(_.toString)
          .cmapO(Read[Boolean].read)
          .mapEl(_ ⇒ unit)

    /**
      * Widgets for editing queries for date values
      */
    lazy val dateQ: WidgetDesc[Unit,String,String] =
      WidgetDesc{os ⇒ 
        val (op,oq) = getPrefixO(os)

        for {
          pre <- queryPrefix.query.desc.signalO(op)
          p   <- date.query.desc.signalO(oq flatMap Read[Date].read)
        } yield unit -> (pre,p).mapN2((p,d) ⇒ s"${p} ${d}")
      }

    /**
      * Widgets for editing queries about editing information
      * based on a given EditInfo.Field.
      *
      * @param f : describing the field to be queried
      * @param userQ : widgets to define queries about users
      *                (typically a select box of registered user aliases)
      */
    def editQ(f: EditInfo.Field, userQ: WidgetDesc[Unit,String,String])
      : WidgetDesc[Unit,String,String] = f match {
        case EditInfo.UserName  ⇒ userQ
        case EditInfo.UserId    ⇒ userQ
        case EditInfo.Summary   ⇒ noQ
        case EditInfo.Timestamp ⇒ dateQ
      }

    /**
      * Widgets for querying molecules based on a Mol.Field.
      */
    def molQ(f: Mol.Field): WidgetDesc[Unit,String,String] = f match {
      case Mol.Smiles         ⇒ stringQO
      case Mol.Inchi          ⇒ stringQ
      case Mol.Mass           ⇒ txtQ(RP.double_)
      case Mol.ExactMass      ⇒ txtQ(RP.double_)
      case Mol.Formula        ⇒ stringQ
      case Mol.LogP           ⇒ txtQ(RP.doubleO_)
      case Mol.Tpsa           ⇒ txtQ(RP.doubleO_)
      case Mol.Lipinski       ⇒ boolQ
      case Mol.Structure      ⇒ molQ
      case Mol.ExactStructure ⇒ molQ
      case Mol.SubStructure   ⇒ molQ
      case Mol.NoStructure    ⇒ emptyQ
      case Mol.Svg            ⇒ noQ
    }

    private def molQ: WidgetDesc[Unit,String,String] = WidgetDesc(o ⇒
      editMol(300, 200, o.map(s ⇒ MolFile fromString s))
        .map3(_.v)
        .map(unit -> _)
    )

    /**
      * Creates a signal of query comparators (and, or)
      */
    def comparatorQ(ini: Option[QComp]): Html[Signal[Option[QComp]]] = for {
      p     <- Html.ask
      d     <- div(cls := CT.QCompContainer.c)
      fst   <- within(p)(isFirstChild)
      dummy = at(d)(clear) as (const(some(QComp.And.c)): Signal[Option[QComp]])
      vs    <- if (fst) Html.liftE(dummy)
               else  within(d)(comparator.query.desc.signalO(ini))
    } yield vs switch onFirstChild(p).head.asF(dummy)

  }
}

// vim: set ts=2 sw=2 et:
