/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package msf
package js

import cats.data.Kleisli
import cats.{ Applicative, Monad, Monoid, MonoidK }, cats.implicits._

import org.scalajs.dom.{html, raw ⇒ hraw, document, File}

abstract class BasicJS[F[_]](implicit M: Monad[F], L: LiftIO[F])
  extends JSHelper[F,String,String,Either[HttpEvent,UIEvent]] {

  def effAsLiftIO: LiftIO[Eff] = L

  def effAsMonad: Monad[Eff] = M

  def evToUIEvent(ev: Ev) = ev.toOption

  def idToString(s: String) = s

  def unapplyId(s: String) = some(s)

  def uniqueId(l: Long) = s"unique-$l"

  def clsToString(c: String) = c
}

object basic extends BasicJS[IO]

trait JSHelper[F[_],ID,Cls,EV]
  extends Attributes[ID,Cls]
  with SFHelper[F] 
  with AppSyntax { self ⇒

  type Ev = EV

  type Elem = html.Element

  type Input = html.Input

  type Select = html.Select

  type Eff[A] = F[A]

  type Html[A] = Kleisli[Eff,Option[Elem],A]

  type EF[A,B] = ef.EF[A,B]

  type Sink[A] = SF[A,Unit]

  type Signal[A] = SF[Ev,A]

  type Src[A] = EF[Ev,A]

  type Action = Src[Unit]

  val ef: EFHelper[F] = EF.helper[F]

  def effAsLiftIO: LiftIO[Eff]

  def effAsMonad: Monad[Eff]

  def evToUIEvent(e: Ev): Option[UIEvent]

  private[js] implicit def liftIOI: LiftIO[Eff] = effAsLiftIO

  private[js] implicit def monadI: Monad[Eff] = effAsMonad

  private[msf] def setAttr(a: Attribute)(e: html.Element): Unit = {
    if (a._2.isEmpty) e removeAttribute a._1
    else e.setAttribute(a._1,a._2)
  }

  def append[A<:html.Element](c: A): Html[A] =
    withElem_{e: Elem ⇒ e appendChild c; ()}.as(c)

  def appendText(t: hraw.Text): Html[Unit] =
    withElem_{e: Elem ⇒ e appendChild t; ()}

  def appendHtml(h: Node): Html[Unit] =
    withElem_{e: Elem ⇒ e.innerHTML = e.innerHTML ++ h.toString}

  def at[A:ToElem,B](a: A)(b: Html[B]): Eff[B] =
    ToElem[A] apply a flatMap b.run

  def clear: Html[Unit] = set innerHtml nodes()

  def elemAt[A:FromElem](id: ID): Eff[Option[A]] = Eff.delayed(
    document getElementById idToString(id) match {
      case e: html.Element ⇒ FromElem[A] apply e
      case _               ⇒ None
    }
  )

  def local[A,B:ToElem](ha: Html[A])(b: B): Html[A] =
    Html.liftE(ToElem[B] apply b) >>= (o ⇒ ha local (_ ⇒ o))

  def localH[A,B:ToElem](ha: Html[A])(b: Html[B]): Html[A] =
    b >>= local(ha)

  def remove: Html[Unit] =
    withElem_{e: Elem ⇒ e.parentNode removeChild e; ()}

  def remove(c: Elem): Html[Unit] = 
    withElem_{e: Elem ⇒ e removeChild c; ()}

  def unapplyId(s: String): Option[ID]

  def uniqueId(i: Long): ID

  def unique: Html[ID] = uiId[Html] map uniqueId

  def withElem_[A](f: A ⇒ Unit)(implicit A: FromElem[A]): Html[Unit] =
    withElemM[A,Unit](a ⇒ Eff delayedTry f(a))

  def withElem[A,B](f: A ⇒ Eff[B])(implicit A: FromElem[A]): Html[Option[B]] =
    Kleisli(_ flatMap A.apply traverse f)

  def withElemM[A,B](f: A ⇒ Eff[B])(implicit A: FromElem[A], B: Monoid[B])
    : Html[B] = withElem(f).map(_ getOrElse B.empty)

  def withElemK[A,B,G[_]](f: A ⇒ Eff[G[B]])
    (implicit A: FromElem[A], G: MonoidK[G]): Html[G[B]] =
    withElem(f).map(_ getOrElse G.empty[B])

  def within[A,B:ToElem](b: B)(ha: Html[A]): Html[A] =
    local(ha)(b)

  def withinH[A,B:ToElem](b: Html[B])(ha: Html[A]): Html[A] =
    localH(ha)(b)

  object get {
    def id: Html[Option[ID]] = withElemK{e: Elem ⇒ Eff delayed unapplyId(e.id) }

    def ids: Html[List[ID]] = for {
      oi <- id
      op <- parent
      rest <- op.fold(Html pure List[ID]())(within(_)(ids))
    } yield oi.fold(rest)(_ :: rest)

    def innerHtml: Html[String] = withElemM{e: Elem ⇒ Eff delayed e.innerHTML}

    def select: Html[String] = withElemM((i: Select) ⇒ Eff delayed i.value)

    def value: Html[String] = withElemM((i: Input) ⇒ Eff delayed i.value)

    def files: Html[List[File]] = withElemM((i: Input) ⇒ Eff delayed domListToList(i.files))

    def checked: Html[Boolean] =
      withElem((i: Input) ⇒ Eff delayed i.checked) map (_ getOrElse false)

    def parent: Html[Option[Elem]] = withElemK{ e: Elem ⇒
      Eff delayed (e.parentNode match {
        case null             ⇒ none
        case _: html.Document ⇒ none
        case x: Elem          ⇒ some(x)
        case _                ⇒ none
      })
    }

    def scrolledToTop: Html[Boolean] =
      withElem{ e: Elem ⇒ Eff delayed (e.scrollTop == 0) }
        .map(_ getOrElse false)

    def scrolledToBottom: Html[Boolean] =
      withElem{ e: Elem ⇒ Eff delayed (
        (e.scrollTop + e.clientHeight) == e.scrollHeight
      ) }.map(_ getOrElse false)
  }

  object set {
    def attribute(a: Attribute): Html[Unit] = withElem_(setAttr(a))

    def customValidity(msg: String ⇒ String): Html[Unit] =
      withElem_((i: Input) ⇒ i setCustomValidity escape(msg(i.value)))

    def innerHtml(h: Node): Html[Unit] =
      withElem_{e: Elem ⇒ e.innerHTML = h.toString}

    def select(s: String): Html[Unit] =
      withElem_((i: Select) ⇒ i.value = s)

    def value(s: String): Html[Unit] =
      withElem_((i: Input) ⇒ i.value = s)
  }
  
  def dynlistF[A,B](
    addH: Html[EF[A,Html[(SF[A,B],EF[A,Unit])]]],
    ini: Html[List[(SF[A,B],EF[A,Unit])]]
  ): Html[SF[A,List[B]]] = for {
    p   <- Html.ask
    add <- addH
    ls  <- ini
    mk  =  add mapF (_ run p)
  } yield msf.SF.dynlist(mk.sf map (_.toList), ls)

  def dynlistA[G[_]:Applicative, A,B](
    addH: Html[EF[A,Html[(SF[A,G[B]],EF[A,Unit])]]],
    ini: Html[List[(SF[A,G[B]],EF[A,Unit])]]
  ): Html[SF[A,G[List[B]]]] = dynlistF(addH,ini) map2 (_.sequence)

  object Src {
    def ui: Src[UIEvent] = idS[Ev] collectO evToUIEvent

    def never[A]: Src[A] = ef.never

    def neverF[G[_]:Applicative,A]: G[Src[A]] = Applicative[G] pure never[A]

    def neverHtml[A]: Html[Src[A]] = neverF[Html,A]

    def neverEff[A]: Eff[Src[A]] = neverF[Eff,A]

    def src[A](a: A): Src[A] = ef const a

    def srcF[G[_]:Applicative,A](a: A): G[Src[A]] = Applicative[G] pure src(a)

    def srcHtml[A](a: A): Html[Src[A]] = srcF[Html,A](a)

    def srcEff[A](a: A): Eff[Src[A]] = srcF[Eff,A](a)

    def srcNone[A]: Src[Option[A]] = src(none)

    def srcNoneF[G[_]:Applicative,A]: G[Src[Option[A]]] = srcF[G,Option[A]](none)

    def srcNoneHtml[A]: Html[Src[Option[A]]] = srcNoneF[Html,A]

    def srcNoneEff[A]: Eff[Src[Option[A]]] = srcNoneF[Eff,A]

    def sig[A](a: A): Signal[A] = const(a)

    def sigF[G[_]:Applicative,A](a: A): G[Signal[A]] = Applicative[G] pure sig(a)

    def sigHtml[A](a: A): Html[Signal[A]] = sigF[Html,A](a)

    def sigEff[A](a: A): Eff[Signal[A]] = sigF[Eff,A](a)

    def sigNone[A]: Signal[Option[A]] = sig(none)

    def sigNoneF[G[_]:Applicative,A]: G[Signal[Option[A]]] = sigF[G,Option[A]](none)

    def sigNoneHtml[A]: Html[Signal[Option[A]]] = sigNoneF[Html,A]

    def sigNoneEff[A]: Eff[Signal[Option[A]]] = sigNoneF[Eff,A]

    def ev[A,E:HasID](f: ID ⇒ Src[A])(e: E): Src[A] =
      HasID[E].apply(e).fold(ef.never : Src[A])(f)

    def htmlSrc[A](f: Elem ⇒ Src[A]): Html[Src[A]] =
      Html.ask map (_.fold[Src[A]](ef.never)(f))

    def htmlSignal[A:Monoid](f: Elem ⇒ Signal[A]): Html[Signal[A]] =
      Html.ask map (_.fold[Signal[A]](const(Monoid[A].empty))(f))

    lazy val change: Html[Action] = htmlSrc(change[Elem])

    def change[E:HasID](e: E): Action =
      ev(i ⇒ ui.collect{ case UIEvent.Change(IDStr(s)) if s == idToString(i) ⇒ unit })(e)

    lazy val click: Html[Action] = htmlSrc(click[Elem])

    def click[E:HasID](e: E): Action =
      ev(i ⇒ ui.collect{ case UIEvent.Click(IDStr(s)) if s == idToString(i) ⇒ unit })(e)

    lazy val dblclick: Html[Action] = htmlSrc(dblclick[Elem])

    def dblclick[E:HasID](e: E): Action =
      ev(i ⇒ ui.collect{ case UIEvent.DblClick(IDStr(s)) if s == idToString(i) ⇒ unit })(e)

    lazy val enter: Html[Action] = htmlSrc(enter[Elem])

    def enter[E:HasID](e: E): Action = 
      keypress(e).collect{ case x if x.key == "Enter" ⇒ unit }

    lazy val esc: Html[Action] = htmlSrc(esc[Elem])

    def esc[E:HasID](e: E): Action =
      keypress(e).collect{ case x if x.key == "Escape" || x.key == "Esc" ⇒ unit }

    lazy val init: Action = ui.collect{ case UIEvent.Init ⇒ unit}

    lazy val keydown: Html[Src[hraw.KeyboardEvent]] = htmlSrc(keydown[Elem])

    def keydown[E:HasID](e: E): Src[hraw.KeyboardEvent] =
      ev(i ⇒ ui.collect{ case UIEvent.KeyDown(x@IDStr(s)) if s == idToString(i) ⇒ x })(e)

    lazy val keyup: Html[Src[hraw.KeyboardEvent]] = htmlSrc(keyup[Elem])

    def keyup[E:HasID](e: E): Src[hraw.KeyboardEvent] =
      ev(i ⇒ ui.collect{ case UIEvent.KeyUp(x@IDStr(s)) if s == idToString(i) ⇒ x })(e)

    lazy val keypress: Html[Src[hraw.KeyboardEvent]] = htmlSrc(keypress[Elem])

    def keypress[E:HasID](e: E): Src[hraw.KeyboardEvent] =
      ev(i ⇒ ui.collect{ case UIEvent.KeyPress(x@IDStr(s)) if s == idToString(i) ⇒ x })(e)

    lazy val scroll: Html[Action] = htmlSrc(scroll[Elem])

    def scroll[E:HasID](e: E): Action =
      ev(i ⇒ ui.collect{ case UIEvent.Scroll(IDStr(s)) if s == idToString(i) ⇒ unit })(e)

    lazy val input: Html[Action] = htmlSrc(input[Elem])

    def input[E:HasID](e: E): Action =
      ev(i ⇒ ui.collect{ case UIEvent.Input(IDStr(s)) if s == idToString(i) ⇒ unit })(e)

    lazy val value: Html[Signal[String]] = htmlSignal(value[Elem])

    def value[E:ToElem](e: E): Signal[String] =
      constS(at(e)(get.value))

    lazy val files: Html[Signal[List[File]]] = htmlSignal(files[Elem])

    def files[E:ToElem](e: E): Signal[List[File]] =
      constS(at(e)(get.files))

    def valueValidated[A,E:ToElem:HasID](
      f: String ⇒ Option[A],
      msg: String ⇒ String
    )(e: E): Signal[Option[A]] = {
      def check(s: String): Eff[Option[A]] = f(s) match {
        case None    ⇒ at(e)(set.customValidity(msg))    as none[A]
        case Some(a) ⇒ at(e)(set.customValidity(_ ⇒ "")) as some(a)
      }

      val ini = at(e)(get.value) >>= check

      constS(ini) on input(e) holdF ini
    }

    lazy val select: Html[Signal[String]] = htmlSignal(select[Elem])

    def select[E:ToElem:HasID](e: E): Signal[String] =
      (change(e) >-> constS(at(e)(get.select))) holdF at(e)(get.select)

    def checked[E:ToElem:HasID](e: E): Signal[Boolean] =
      (input(e) >-> constS(at(e)(get.checked))) holdF at(e)(get.checked)
  }

  object sink {
    def console: Sink[String] = liftS(Eff.putStrLn)

    def attribute[E:ToElem](e: E): Sink[Attribute] =
      liftS{ a ⇒ at(e)(set attribute a) }
    
    def innerHtml[E:ToElem](e: E): Sink[Node] =
      liftS{ s ⇒ at(e)(set innerHtml s) }
    
    def value[E:ToElem](e: E): Sink[String] =
      liftS{ s ⇒ at(e)(set value s) }
    
    def selectValue[E:ToElem](e: E): Sink[String] =
      liftS{ s ⇒ at(e)(set select s) }

    def validate[E:ToElem](msg: String ⇒ String)(e: E): Sink[Boolean] =
      liftS{
        if (_) at(e)(set.customValidity(_ ⇒ ""))
        else   at(e)(set customValidity msg)
      }
  }

  object Html extends MonadH[Html] {
    def liftE[A](e: Eff[A]): Html[A] = Kleisli liftF e

    def ask: Html[Option[Elem]] = Kleisli.ask
  }

  object Eff extends MonadH[Eff]

  lazy val T = new TextHelper[ID,Cls]{
    def clsToString(c: Cls) = self clsToString c
    def idToString(i: ID) = self idToString i
  }

  trait ToElem[A] { def apply(a: A): Eff[Option[Elem]] }

  object ToElem {
    def apply[A](implicit A: ToElem[A]): ToElem[A] = A

    def inst[A](f: A ⇒ Eff[Option[Elem]]): ToElem[A] =
      new ToElem[A]{ def apply(a: A) = f(a) }

    implicit def elemI[A<:Elem]: ToElem[A] = inst(a ⇒ Eff pure some(a))

    implicit def idI[I<:ID]: ToElem[I] = inst(i ⇒ elemAt[Elem](i))

    implicit def optionI[A](implicit A: ToElem[A]): ToElem[Option[A]] =
      inst(_.fold(Eff pure none[Elem])(A.apply))

    implicit def effI[A](implicit A: ToElem[A]): ToElem[Eff[A]] =
      inst(_ >>= A.apply)

    implicit val targetI: ToElem[hraw.EventTarget] = inst{
      case e: Elem ⇒ Eff pure some(e)
      case _       ⇒ Eff pure none
    }
  }

  trait HasID[A] { def apply(a: A): Option[ID] }

  object HasID extends HasIDLP {
    def apply[A](implicit A: HasID[A]): HasID[A] = A

    implicit def elemI[A<:Elem]: HasID[A] = inst(a ⇒ unapplyId(a.id))

    implicit def idI[I<:ID]: HasID[I] = inst(some)

    implicit def optionI[A](implicit A: HasID[A]): HasID[Option[A]] =
      inst(_ flatMap A.apply)
  }

  trait HasIDLP {
    def inst[A](f: A ⇒ Option[ID]): HasID[A] =
      new HasID[A]{ def apply(a: A) = f(a) }
  }

  object ID {
    def unapply(e: hraw.Event): Option[ID] = e.target match {
      case i: html.Element ⇒ unapplyId(i.id)
      case _               ⇒ None
    }
  }

  object IDStr {
    def unapply(e: hraw.Event): Option[String] = {
      e.target match {
        case i: html.Element ⇒ Some(i.id)
        case _               ⇒ None
      }
    }
  }
}

// vim: set ts=2 sw=2 et:
