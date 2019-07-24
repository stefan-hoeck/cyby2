/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby

import cats.implicits._

import cyby.{Parser ⇒ P}

class ParserTest extends CyBySuite {
  val u: Unit = ()

  property("all consumes whole string") {
    forAll{ s: List[String] ⇒ P.run(P.all)(s) shouldEq some(Nil -> s) }
  }

  property("next on empty string returns none") {
    P.run(P.next)(Nil) shouldEq None
  }

  property("next on non-empty list returns head") {
    forAll{ (s: String, ss: List[String]) ⇒
      P.run(P.next)(s::ss) shouldEq some(ss -> s) }
  }

  property("single on empty string returns none") {
    forAll{ (s: String) ⇒ P.run(P single s)(Nil) shouldEq None }
  }

  property("single on matching head returns head") {
    forAll{ (s: String, ss: List[String]) ⇒
      P.run(P single s)(s :: ss) shouldEq some(ss -> s) }
  }

  property("single on non-matching head returns none") {
    forAll{ (s1: String, s2: String, ss: List[String]) ⇒
      if (s1 =!= s2)
        P.run(P single s1)(s2 :: ss) shouldEq None
    }
  }

  property("pure works properly") {
    forAll { (n: Int, ss: List[String]) ⇒
      P.run(P pure n)(ss) shouldEq Some(ss -> n)
    }
  }

//  property("parens behaves properly") {
//    forAll { (x: Double, s: String) ⇒
//      braces(double).parse(s"($x)") shouldEq Some("" -> x)
//    }
//  }
//
//  property("eoi behaves properly") {
//    forAll { s: String ⇒
//      if (s.isEmpty) eoi.parse(s) shouldEq Some("" -> u)
//      else eoi.parse(s) shouldEq None
//    }
//  }
//
//  property("many behaves properly") {
//    forAll { ds: List[Double] ⇒
//      val p = trimBoth(double)
//      many(p).parse(ds mkString " ") shouldEq Some("" -> ds)
//    }
//  }
//
//  property("some behaves properly") {
//    forAll { (d: Double, ds: List[Double]) ⇒
//      val p = trimBoth(double)
//      Parser.some(p).parse((d::ds) mkString " ") shouldEq
//      Some("" -> Nel(d,ds))
//    }
//  }
}

// vim: set ts=2 sw=2 et:
