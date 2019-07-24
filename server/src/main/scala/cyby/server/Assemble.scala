/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server

import cats.Traverse
import cats.implicits.{none ⇒ _, _}
import shapeless.{Id ⇒ _, tag ⇒ _, _}

/**
  * Type class to assemble server data before being sent to
  * the client.
  *
  * Typical use case: If a piece of data like a compound
  * is linked to another piece of data (like a project) for instance
  * via its ID or a path field, the data object at the server might
  * only hold this ID or path. This allows for the linked project
  * to be updated without having to update the fields of all linked
  * data objects. However, when sending a compound to the client, the
  * project's name and other information needs to be displayed in the UI.
  * We therefore have to resolve the link before sending the data object
  * to the client.
  *
  * Assembling a piece of data might require additional information,
  * for instance for resolving links to other data. This process
  * might also fail, in which case the result will be an error.
  *
  * Instances of this type class can be derived automatically using
  * shapeless's generic deriving mechanism. See DerivedAssemble.
  *
  * @tparam S : state object needed to assemble objects of type A
  * @tparam E : error type
  * @tparam A : input type
  * @tparam B : output type
  */
trait Assemble[S,E,A,B]{
  def curried(s: S): A ⇒ ValNel[E,B] = run(s,_)

  def run(s: S, a: A): ValNel[E,B]

  def runE(s: S)(a: A): ErrNel[E,B] = run(s,a).toEither

  def map[C](f: B ⇒ C): Assemble[S,E,A,C] = Assemble.inst((s,a) ⇒ run(s,a) map f)
}

object Assemble extends AssembleLowPriority {
  def apply[S,E,A,B](implicit A: Assemble[S,E,A,B]): Assemble[S,E,A,B] = A

  def derived[S,E,A,B](implicit A: DerivedAssemble[S,E,A,B]): Assemble[S,E,A,B] = A

  implicit def trivialI[S,E,A]: Assemble[S,E,A,A] = inst((_,a) ⇒ valid(a))

  implicit def molI[S,E]: Assemble[S,E,Maybe[cyby.chem.Mol],Maybe[cyby.dat.Mol]] =
    Assemble.inst((_,om) ⇒ valid(om map (_.toDatMol)))
}

trait AssembleLowPriority {
  def inst[S,E,A,B](f: (S,A) ⇒ ValNel[E,B]): Assemble[S,E,A,B] =
    new Assemble[S,E,A,B]{def run(s: S, a: A) = f(s,a)}

  implicit def pureI[S,E,A,B](implicit A: Assemble[S,E,A,B]): Assemble[S,E,Pure[A],Pure[B]] =
    inst((s,pa) ⇒ A.run(s,pa.v) map Pure.apply)

  implicit def traverseI[S,E,A,B,F[_]:Traverse](implicit A: Assemble[S,E,A,B])
    : Assemble[S,E,F[A],F[B]] = inst((s,as) ⇒ as traverse (A.run(s, _)))
}

trait DerivedAssemble[S,E,A,B] extends Assemble[S,E,A,B]

object DerivedAssemble {

  def inst[S,E,A,B](f: (S,A) ⇒ ValNel[E,B]): DerivedAssemble[S,E,A,B] =
    new DerivedAssemble[S,E,A,B]{def run(s: S, a: A) = f(s,a)}

  implicit def hnil[S,E]: DerivedAssemble[S,E,HNil,HNil] = inst((_,_) ⇒ valid(HNil))

  implicit def hcons[S,E,H1,H2,T1<:HList,T2<:HList](
    implicit H: Assemble[S,E,H1,H2], T: Lazy[DerivedAssemble[S,E,T1,T2]]
  ): DerivedAssemble[S,E,H1 :: T1, H2 :: T2] = inst((s,hl1) ⇒
    (H.run(s,hl1.head), T.value.run(s,hl1.tail)).mapN(_ :: _)
  )

  implicit def gen[S,E,A1,HL1,A2,HL2](
    implicit G1: Generic.Aux[A1,HL1], G2: Generic.Aux[A2,HL2], A: Lazy[DerivedAssemble[S,E,HL1,HL2]]
  ): DerivedAssemble[S,E,A1,A2] = inst((s,a1) ⇒ A.value.run(s, G1 to a1) map G2.from)
}

// vim: set ts=2 sw=2 et:
