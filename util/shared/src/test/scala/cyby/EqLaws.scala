/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby

import cats.implicits._

import org.typelevel.discipline.Laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

object EqLaws {
  def apply[A: cats.Eq: Arbitrary]: EqLaws[A] =
    new EqLaws[A] {
      def Equ = cats.Eq[A]
      def Arb = implicitly[Arbitrary[A]]
    }
}

trait EqLaws[A] extends Laws {

  implicit def Equ: cats.Eq[A]
  implicit def Arb: Arbitrary[A]

  def eqv: DefaultRuleSet = new DefaultRuleSet(
    "eqv",
    None,
    "reflexitivity-eq" -> forAll { (x: A) => x === x },
    "symmetry-eq" -> forAll { (x: A, y: A) => Equ.eqv(x, y) === Equ.eqv(y, x) },
    "transitivity-eq" -> forAll { (x: A, y: A, z: A) =>
      !(Equ.eqv(x, y) && Equ.eqv(y, z)) || Equ.eqv(x, z)
    }
  ){}

}


// vim: set ts=2 sw=2 et:
