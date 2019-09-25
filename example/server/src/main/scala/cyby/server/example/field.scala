/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

import cyby.dat.Link, cyby.dat.example._
import cyby.query.{ReadPred ⇒ RP}

/**
  * Fields are used to let users select some content or
  * behavior from a list of choices. For instance: What field
  * of a compound to use in a combined query or what piece
  * of information to include when exporting data.
  *
  * At some place (once in the client code, and once in the server code)
  * we have to pattern match on the list of possible fields to get
  * the desired behavior for each of them. This is this place at
  * the server.
  *
  * Ideally we would make use of some dependent typing to
  * prevent us from making all kinds of type errors in this
  * pattern match. However, coming up with those dependent types
  * is cumbersome in Scala, so we dropped them in preferance
  * of more readable albeit less secure code.
  */
case class Field(coreSettings: CoreSettings) extends CyByZ with cyby.server.fields {
  private def pro[A,B](g: A ⇒ Link[Pro.AccId], h: B ⇒ Pro.AccId): Act[A,B] =
    link(g, h, _.proList)(_.name.v, _.id.to, RP.id_)

  private def loc[A,B](g: A ⇒ Link[Sto.Id], h: B ⇒ Sto.Id): Act[A,B] =
    link(g, h, _.stoList)(_.name.v, _.id, RP.id_)

  private def sup[A,B](g: A ⇒ Link[Sup.Id], h: B ⇒ Sup.Id): Act[A,B] =
    link(g, h, _.supList)(_.name.v, _.id, RP.id_)

  private def met[A,B](g: A ⇒ Link[Met.Id], h: B ⇒ Met.Id): Act[A,B] =
    link(g, h, _.metList)(_.name.v, _.id, RP.id_)

  def sub(f: SubField): Act[Sub.Cli,SubS.Acc] = sub_(f, identity)

  def sub_[A](f: SubField, g: A ⇒ Sub.Cli): Act[A,SubS.Acc] = f match {
    case SubMol(f)         ⇒ mol(f)(g(_).structure.v.o, _.structure.v.o)
    case SubEditInfo(f)    ⇒ edit(f)(g(_).modified, _.modified)
    case SubName           ⇒ string(g(_).name.v.v, _.name.v.v)
    case SubCasNr          ⇒ string(g(_).casNr.v.v, _.casNr.v.v)
    case SubProject        ⇒ pro(g(_).project.v, _.project.v)
    case SubId             ⇒ id(g(_).id, _.id)
    case SubAbs            ⇒ bool(g(_).abs.v, _.abs.v)
    case SubCreated        ⇒ timestamp(g(_).created, _.created)
    case SubFil(_)         ⇒ dummy
    case SubContainers     ⇒ dummy
  }

  def con(f: ConField): Act[Con.Cli, ConS.Acc] = con_(f, identity)

  def con_[A](f: ConField, g: A ⇒ Con.Cli): Act[A, ConS.Acc] = f match {
    case ConLocation       ⇒ loc(g(_).location.v, _.location.v)
    case ConSupplier       ⇒ sup(g(_).supplier.v, _.supplier.v)
    case ConProject        ⇒ pro(g(_).project.v, _.project.v)
    case ConCreated        ⇒ timestamp(g(_).created, _.created)
    case ConEditInfo(f)    ⇒ edit(f)(g(_).modified, _.modified)
    case ConAmount         ⇒ double(g(_).amount.v.v, _.amount.v.v)
    case ConBatch          ⇒ string(g(_).batch.v.v, _.batch.v.v)
    case ConComment        ⇒ string(g(_).comment.v.v, _.comment.v.v)
    case ConConcentration  ⇒ double(g(_).concentration.v.v, _.concentration.v.v)
    case ConDensity        ⇒ double(g(_).density.v.v, _.density.v.v)
    case ConEmpty          ⇒ bool(g(_).empty.v, _.empty.v)
    case ConId             ⇒ id(g(_).id, _.id)
    case ConLentTo         ⇒ string(g(_).lentTo.v.v, _.lentTo.v.v)
    case ConOrderNr        ⇒ string(g(_).orderNr.v.v, _.orderNr.v.v)
    case ConPurity         ⇒ double(g(_).purity.v.v, _.purity.v.v)
    case ConPurityStr      ⇒ string(g(_).purityStr.v.v, _.purityStr.v.v)
    case ConFil(_)         ⇒ dummy
  }

  def bio(f: BioField): Act[BiodataEntry.Cli, BiodataEntryS.Acc] = f match {
    case BioMethod         ⇒ met(_.method.v, _.method.v)
    case BioSupplier       ⇒ sup(_.supplier.v, _.supplier.v)
    case BioDate           ⇒ dateAct(_.date.v, _.date.v)
    case BioProject        ⇒ pro(_.project.v, _.project.v)
    case BioCreated        ⇒ timestamp(_.created, _.created)
    case BioEditInfo(f)    ⇒ edit(f)(_.modified, _.modified)
    case BioComment        ⇒ string(_.comment.v.v, _.comment.v.v)
    case BioId             ⇒ id(_.id, _.id)
    case BioValue          ⇒ double(_.value.v, _.value.v)
    case BioFil(_)         ⇒ dummy
  }

  def fil(f: FilField): Act[Fil.Cli,SubFilS.Acc] = f match {
    case FilId            ⇒ id(_.id, _.id)
    case FilName          ⇒ string(_.name.v.v, _.name.v.v)
    case FilComment       ⇒ string(_.comment.v.v, _.comment.v.v)
    case FilPath          ⇒ string(_.path.v.v, _.path.v.v)
    case FilProject       ⇒ pro(_.project.v, _.project.v)
    case FilCreated       ⇒ timestamp(_.created, _.created)
    case FilEditInfo(f)   ⇒ edit(f)(_.modified, _.modified)
  }
}
