/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server

/**
  * Uses a B to modify an A.
  *
  * Instances of this typeclass are used when
  * updating data objects. They can be derived automatically
  * using shapeless's generic deriving mechanism
  *
  * @tparam A : type of data to be updated
  * @tparam B : type of data used to updated objects of type A
  */
trait Modifier[A,B]{
  def apply(a: A, b: B): A
}

object Modifier extends ModifierLowP {
  implicit def modI[A]: Modifier[Pure[A],Option[A]] =
    inst((pa,oa) ⇒ oa.fold(pa)(Pure.apply))
}

trait ModifierLowP extends ModifierLowestP {
  implicit def voidI[A]: Modifier[A,Undef] = inst((a,_) ⇒ a)
}

trait ModifierLowestP {
  def inst[A,B](f: (A,B) ⇒ A): Modifier[A,B] =
    new Modifier[A,B]{def apply(a: A, b: B) = f(a,b)}

  implicit def trivialI[A]: Modifier[A,A] = inst((_,a) ⇒ a)
}

trait DerivedModifier[A,B]{
  def apply(a: A, b: B): A
}

object DerivedModifier {
  import shapeless.{HNil,::,HList, Generic, Lazy}

  def apply[A,B](implicit D: DerivedModifier[A,B]): DerivedModifier[A,B] = D

  def inst[A,B](f: (A,B) ⇒ A): DerivedModifier[A,B] =
    new DerivedModifier[A,B]{def apply(a: A, b: B) = f(a,b)}

  implicit val hnilI: DerivedModifier[HNil,HNil] = inst((_,_) ⇒ HNil)

  implicit def hlistI[A,B,TA<:HList,TB<:HList](
    implicit HI: Modifier[A,B],
             TI: Lazy[DerivedModifier[TA,TB]]
  ): DerivedModifier[A::TA,B::TB] =
    inst((a,b) ⇒ HI(a.head,b.head) :: TI.value(a.tail,b.tail))

  implicit def generic[A,B,HA,HB](implicit
    GA: Generic.Aux[A,HA],
    GB: Generic.Aux[B,HB],
    D:  Lazy[DerivedModifier[HA,HB]]
  ): DerivedModifier[A,B] =
    inst((a,b) ⇒ GA.from(D.value.apply(GA to a, GB to b)))
}

