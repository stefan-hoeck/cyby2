/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui
package editor

import cyby.dat.{Edit ⇒ DEdit, _}

import chemdoodle.Molecule

import cats.implicits.{none ⇒ _, _}

import msf.js.{UIEvent, InputType}, UIEvent.{DblClick, Click}
import cyby.ui.{CompType ⇒ CT}

import shapeless.{HNil, HList, Witness, Lazy, LabelledGeneric}
import shapeless.labelled.FieldType

import io.circe.{JsonObject, Json, Encoder}
import io.circe.syntax._

import org.scalajs.dom.raw.MouseEvent

/**
  * Environment trait used to write signal functions for
  * editing and updating data objects.
  */
trait EditEnv extends DomEnv {
  /**
    * Deals with creating, editing, and updating data objects.
    */
  trait Editor extends DomH[(St,Creds),IO[Unit],Option[UIEvent]] {
    import UId.{Load ⇒ _, _}, Src.{click, enter, ui, esc}
   
    type Res = Option[JsonObject ⇒ JsonObject]

    override def evToUIEvent(e: Ev) = e

    def run: LogSF[EditorIn,Unit] = unrss(behavior >>- liftS(load))(ioUnit)

    lazy val askSt: Html[St] = Html liftE askE.map(_._1)

    lazy val creds: Eff[Creds] = askE.map(_._2)

    lazy val credsS: Signal[Creds] = env.map(_._2)

    private def behavior: Src[Load] =
      ef.switch(ed collectO identity).effect(cleanup)                          <|>
      click(LogoutBtnId).asF(creds map (Load.Logout(_).l))                     <|>
      ui.collectF{ case Click(ID(UId.Load(p))) ⇒ creds.map(Load.Path(p, _).l)} <|>
      credsS.collect{ case c@Creds(_,_,_,HasLoggedIn) ⇒ Load.All(c).l }

    private def doedit: Src[Option[Src[Option[Load]]]] = {
      def tryEdit(e: MouseEvent)(pf: PartialFunction[UIdP,(String,Path)]) =
        at(e.target)(get.ids).flatMap(
          _.collect(pf).headOption.traverse{
            case (f,p) ⇒ at(EditCont(f,p))(prepare(Edit(f,p)) >>= edit(f,p))
          }
        )

      Src.ui.collectF {
        case Click(ID(Create(dt,p))) ⇒ some(p).traverse{ p ⇒
          at(CreateCont(dt,p))(prepare(Create(dt,p)) >>= add(dt,p))
        }

        case Click(ID(DeleteId(p))) ⇒ some(p).traverse{ p ⇒
          askE map delete(p)
        }

        case Click(ID(Clone(p))) ⇒ clone(p)

        case DblClick(e) ⇒ tryEdit(e){case Edit(f,p) ⇒ (f,p)}

        case Click(e) ⇒ tryEdit(e){case ClickEdit(f,p) ⇒ (f,p)}
      }
    }

    private def ed: Src[Option[Src[Load]]] = doedit.map2(_.mapF{
      case Some(j) ⇒ cleanup as j
      case None    ⇒ cleanup as Load.NoLoad.l
    }.head)

    /**
      * Based on a string representing the field to edit and
      * a Path object, create an event stream that interprets
      * user input and fires, once a confirmation or cancelling button
      * or key has been pressed. 
      */
    def edit(fld: String, p:  Path)(env: Env): Html[Src[Option[Load]]]

    /**
      * Clone an existing piece of data: Given a path create
      * a view for adding a new data object with fields already
      * filled in accordance with the data object to be cloned.
      */
    def clone(p: Path): Eff[Option[Src[Option[Load]]]]

    /**
      * Create an view for editing a new data object of
      * the given DataType and with the given parent Path.
      * The event stream should fire a Some if input is
      * valid and users click a button or presse a key for
      * confirmation, and it should fire a None if
      * user input signals cancelling of the process.
      */
    def add(dt: DataType, p: Path)(env: Env): Html[Src[Option[Load]]]

    /**
      * Signal function fireing a Some if a user confirms
      * deliting the object at the given Path and a None
      * to signal cancelling the process.
      */
    def delete(p: Path)(env: Env): Src[Option[Load]]

    def prepare(actId: UIdP): Html[Env] = for {
      _   <- Html liftE cleanup
      i   <- Html.ask
      ht  <- get.innerHtml
      _   <- set innerHtml ""
      act <- Html liftE elemAt[Elem](actId)
      _   <- act traverse_ (e ⇒ Html delayed (e.id = ""))
      _   <- Html liftE setS(
               msf.js.delayTry{
                 i foreach (_.innerHTML = ht)
                 act foreach (_.id = actId.id)
               }
             )
      e   <- Html liftE askE
    } yield e

    private lazy val cleanup: Eff[Unit] = (getS >>= Eff.liftIO) *> setS(ioUnit)
   
    def stopEditing[A]: Html[Src[A]] = Html liftE (cleanup as ef.never)
   
    def emptyRes: Signal[Res] = Src sig some(identity)

    def edLink[El<:Elem,A,P,ID:Encoder](desc: St ⇒ WidgetDesc[El,A,A])(
      find: St ⇒ P ⇒ Option[A],
      id: A ⇒ ID
    ): St ⇒ WidgetDesc[El,Link[P],Json] = st ⇒
      desc(st).cmapO((l: Link[P]) ⇒ find(st)(l._1)).map(id(_).asJson)

    /**
      * Typeclassic description of editing a single data field,
      * based on an optional initial value.
      *
      * Instances are responsible to append all necessary
      * HTML elements to the parent element available from
      * the Html Monad.
      */
    trait Editable[A]{ self ⇒ 
      /**
        * Edit a single field. This is used when updating
        * a field in an existing piece of data.
        */
      def edit(a: A): Html[Src[Option[Json]]]

      /**
        * Used for creating new pieces of data: Given an optional
        * starting value plus a Symbol describing the field to
        * be edited, create signal function for editing
        * the field in question.
        */
      def create(o: Option[A], s: Symbol): Html[Signal[Res]]

      /**
        * Editable is a contravariant functor
        */
      def contramap[B](f: B ⇒ A): Editable[B] = new Editable[B]{
        def edit(b: B) = self edit f(b)
        def create(o: Option[B], s: Symbol) = self.create(o map f, s)
      }
    }

    /**
      * Implementation of Editable:
      *
      * sig creates a HTML element and signal function from an optional
      * starting value. This is used for creating new values and
      * for editing existing value.
      *
      * src uses the results from sig to create an event stream.
      * This means typically, this is used for editing existing values:
      * The event stream only fires if a confirmation button was clicked
      * or Enter was pressed on a text field or drop down menue.
      */
    case class EdI[A](
      sig: (St,Option[A]) ⇒ Html[(Elem,Signal[Option[Json]])],
      src: (St,Elem,Signal[Option[Json]]) ⇒ Html[Src[Option[Json]]],
    ) extends Editable[A] {
      def edit(a: A) = askSt >>= { st ⇒
        sig(st, some(a)) >>= { case (e,s) ⇒ src(st, e, s) }
      }

      def create(o: Option[A], s: Symbol) = Li(cls := Comp(CT NavEditRow s), lbl(s)){
        askSt >>= { sig(_,o) map (_._2) map3 Createable.adjust(s) }
      }
    }

    object Editable extends LowPriorityEditable {
      def apply[A](implicit U: Editable[A]): Editable[A] = U

      def desc[El<:Elem,A](
        wd: St ⇒ WidgetDesc[El,A,Json],
        confirmActions: Boolean = false
      ): EdI[A] = EdI[A](
        (st,oa) ⇒ wd(st) pairO oa map { case (e,s) ⇒ (e: Elem) -> s },
        (_,e,s) ⇒ if (confirmActions) Html pure confirmed(s)(enter(e), esc(e))
                  else within(e)(withConfirmButtons(s))
      )

      def wrapped[El<:Elem,A](wd: St ⇒ WidgetDesc[El,A,Json]): EdI[A] = desc(
        wd(_).withinEl(li(cls := CT.DynamicEditRow.c))
             .withinEl(ul(cls := CT.ListEditContainer.c))
      )

      def input[A:Encoder](d: InputDesc[A]): EdI[A] =
        desc(_ ⇒ d.edit.desc.map(_.asJson), true)

      def sel[A:Encoder](d: St ⇒ SelectDesc[A]): EdI[A] =
        wrapped(d(_).edit.desc.map(_.asJson))

      def list[El<:Elem,A,B:Encoder](d: St ⇒ WidgetDesc[El,A,B]): EdI[List[A]] =
        desc(d(_).list(CT.ListEditContainer, CT.DynamicEditRow)
                 .map(bs ⇒ Json fromValues bs.map(_.asJson)))

      def nel[El<:Elem,A,B:Encoder](d: St ⇒ WidgetDesc[El,A,B]): EdI[Nel[A]] =
        desc(d(_).nel(CT.ListEditContainer, CT.DynamicEditRow)
                 .map(bs ⇒ Json fromValues bs.toList.map(_.asJson)))

      implicit val molI: Editable[Maybe[Mol]] =
        desc(_ ⇒ MolDesc(600, 400, CT.StructureEdit)
                   .desc.cmapO[Maybe[Mol]](_.o) mapO mergeMols)

      private def mergeMols(ms: List[Molecule]): Option[Json] = ms match {
        case Nil  ⇒ some("".asJson)
        case _    ⇒ tryO{
          val mol = new Molecule()
          ms foreach { m ⇒ {
            mol.atoms = mol.atoms ++ m.atoms
            mol.bonds = mol.bonds ++ m.bonds
          }}
          Json.obj("raw" -> writeMol(mol).asJson)
        }
      }

      def noEdit[A]: Editable[A] = new Editable[A] {
        def edit(a: A) = Src.srcNoneHtml
        def create(o: Option[A], s: Symbol) = Html pure emptyRes
      }

      implicit val undefI: Editable[Undef] = noEdit

      implicit val unitI: Editable[Unit] = noEdit

      implicit val infoI: Editable[EditInfo] = noEdit

      implicit val timestampI: Editable[TimeStamp] = noEdit

      implicit val aliasI: Editable[Alias] = input(alias)

      implicit val nameI: Editable[Name] = input(nameIn)

      implicit val fileNameI: Editable[FileName] = input(fileName)

      implicit val nameOI: Editable[Option[Name]] = input(nameIn.optional)

      implicit val passwordOI: Editable[Option[Password]] = input(password.optional)

      implicit val plainI: Editable[Plain] = input(plain)

      implicit val casNrI: Editable[CasNr] = input(casNr)

      implicit val percentI: Editable[Percent] = input(percent)

      implicit val amountI: Editable[Amount] = input(amount)

      implicit val valueI: Editable[Double] = input(double)

      implicit val concentrationI: Editable[Concentration] = input(concentration)
      
      implicit val densityI: Editable[Density] = input(density)

      implicit val levelI: Editable[UserLevel] = sel(_ ⇒ userLevel)

      implicit val dateI: Editable[Date] = input(date)

      implicit val boolI: Editable[Boolean] = new Editable[Boolean]{
        def edit(b: Boolean) = Html pure ef.const(some((!b).asJson)).head
        def create(o: Option[Boolean], s: Symbol) = sel(_ ⇒ bool).create(o,s)
      }

      implicit def idI[A]: Editable[Id[A]] = noEdit

      implicit def pureI[A](implicit A: Editable[A]): Editable[Pure[A]] =
        A.contramap(_.v)
    }

    trait LowPriorityEditable {
      implicit def listI[A]: Editable[List[A]] = Editable.noEdit
    }

    /**
      * Automatically derived creation of a data object
      * from instances of Editable.
      */
    case class Createable[A](
      sig: Option[A] ⇒ Html[Signal[Res]],
      src: (String, A) ⇒ Option[Html[Src[Res]]]
    ){
      type LoadO = Option[Load]
      type SrcLoad = Html[Src[Option[Load]]]

      def json[B,I](b: B, e: DEdit[Id[I],JsonObject,JsonObject])(toJ: B ⇒ Json ⇒ Json)
        : Json = toJ(b)(e.asJson)

      def load[B,I](c: Creds, dt: DataType, b: B, e: DEdit[Id[I],JsonObject,JsonObject])
        (toJ: B ⇒ Json ⇒ Json): Load = Load.Mutate(dt, toJ(b)(e.asJson), c)

      def addO[B](c: Creds, dt: DataType, b: B, o: Option[A])(toJ: B ⇒ Json ⇒ Json): SrcLoad =
        sig(o).map3(f ⇒ load[B,B](c, dt, b, Add(f(JsonObject())))(toJ))
              .flatMap(withConfirmButtons)

      def add[B](c: Creds, dt: DataType, b: B)(toJ: B ⇒ Json ⇒ Json): SrcLoad =
        addO(c, dt, b, none)(toJ)

      def addFil[B](c: Creds, dt: DataType, b: B)(toJ: B ⇒ Json ⇒ Json): SrcLoad =
        for {
          json <- sig(none).map3(f ⇒ json[B,B](b, Add(f(JsonObject())))(toJ))
          fil  <- input(InputType.File, Nil) map (Src files _)
          sig  =  (json,fil.map(_.headOption)).mapN2(Load.AddFile(dt, _, _, c).l)
          src  <- withConfirmButtons(sig)
        } yield src

      def ed[B,I](
        p:   (St,Creds),
        dt:  DataType,
        b:   B,
        id:  Id[I],
        fld: String,
      )(toJ: B ⇒ Json ⇒ Json,get: St ⇒ Option[A]): SrcLoad = {
        val (st,c) = p

        get(st).flatMap(src(fld,_)) match {
          case None           ⇒ warnH(s"Don't know how to update ${id}, ${fld}") *>
                                Src.srcNoneHtml[Load]
          case Some(sr) ⇒ sr.map3(f ⇒ load(c, dt, b, Mod(id, f(JsonObject())))(toJ))
        }
      }

      def del[B,I](
        c:  Creds,
        dt: DataType,
        b:  B,
        id: Id[I]
      )(toJ: B ⇒ Json ⇒ Json): Src[Option[Load]] =
        ef.const(some(load(c, dt, b, Del(id))(toJ))).head
    }

    object Createable {
      import shapeless.::

      def apply[A](implicit C: Createable[A]): Createable[A] = C

      def adjust(s: Symbol): Json ⇒ JsonObject ⇒ JsonObject =
        jv ⇒ jo ⇒ jo.add(s.name, jv)

      implicit lazy val hnilI: Createable[HNil] =
        Createable(_ ⇒ Html pure emptyRes, (_,_) ⇒ None)
   
      implicit def hlistI[K <: Symbol,H,T <: HList]( implicit
        w: Witness.Aux[K], hI: Lazy[Editable[H]], tI: Lazy[Createable[T]],
      ): Createable[FieldType[K,H] :: T] = {
        val (h, t, sym) = (hI.value, tI.value, w.value)

        val sig = (o: Option[FieldType[K,H] :: T]) ⇒ (
          h.create(o.map(_.head : H), sym),
          t.sig(o.map(_.tail))
        ).mapN2(_ andThen _)

        val src = (s: String, hl: FieldType[K,H] :: T) ⇒ 
          if (s === sym.name) some(h.edit(hl.head).map3(adjust(sym)))
          else t.src(s,hl.tail)
      
        Createable(sig, src)
      }

      implicit def genericI[A,H <: HList](
        implicit g: LabelledGeneric.Aux[A,H],
        i: Lazy[Createable[H]]
      ): Createable[A] = Createable[A](
        (o: Option[A])   ⇒ i.value.sig(o map g.to),
        (s: String,a: A) ⇒ i.value.src(s,g to a)
      )
    }
  }
}

// vim: set ts=2 sw=2 et:
