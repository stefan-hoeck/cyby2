/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby

import cats.Eq
import cats.instances.all._

import org.typelevel.discipline.Laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

object ReadLaws {
  def apply[A:Eq:Arbitrary:Read]: ReadLaws[A] =
    new ReadLaws[A] {
      def Equ = Eq[Option[A]]
      def Arb = implicitly[Arbitrary[A]]
      def Rea = Read[A]
    }
}

trait ReadLaws[A] extends Laws {
  def Equ: Eq[Option[A]]
  def Rea: Read[A]
  implicit def Arb: Arbitrary[A]

  def fromTo: DefaultRuleSet = new DefaultRuleSet(
    "Read",
    None,
    "to and from" → forAll { a: A ⇒
      val str: String = a.toString
      val res: Option[A] = Rea read str
      val exp: Option[A] = Some(a)

      Equ.eqv(res, exp) :| s"Expected $exp, but was $res"
    }
  ){}
}

// vim: set ts=2 sw=2 et:
