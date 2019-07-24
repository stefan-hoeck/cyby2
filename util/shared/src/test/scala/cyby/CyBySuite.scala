/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby

import cats.Eq
import org.scalatest.compatible.Assertion
import org.typelevel.discipline.Laws

trait CyBySuite
  extends org.scalatest.PropSpec
  with org.scalatest.prop.PropertyChecks
  with org.scalatest.prop.Checkers
  with org.scalatest.Matchers
  with cyby.TestImplicits {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(
      minSize             = 0,
      sizeRange           = 50,
      minSuccessful       = 100,
      maxDiscardedFactor  = 20.0,
      workers             = 4
    )

  def checkLaws(name: String, ruleSet: Laws#RuleSet): Unit = {
    for ((id,prop) ← ruleSet.all.properties)
      property(name + "." + id) { check(prop) }
  }

  implicit class EqSyntax[A](val v: A) {
    def =-= (that: A)(implicit E: Eq[A]): Boolean = E.eqv(v,that)

    def shouldEq (that: A)(implicit E: Eq[A]): Assertion =
      assert(E.eqv(v,that), s"$v was not equal to $that")

    def shouldNotEq (that: A)(implicit E: Eq[A]): Assertion =
      assert(!E.eqv(v,that), s"$v was not different from $that")
  }
}

// vim: set ts=2 sw=2 et:
