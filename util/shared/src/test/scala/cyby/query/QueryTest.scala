/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package query

import cats.implicits._

class QueryTest extends CyBySuite with QueryImplicits {
  def longP(s: String): Pred[Long] = ReadPred.long_(s).get

  property("predicate from \"==\" behaves properly") {
    forAll{ (n1: Long, n2: Long) ⇒ longP(s"== $n1")(n2) shouldEq (n2 == n1) }
  }

  property("predicate from \"!=\" behaves properly") {
    forAll{ (n1: Long, n2: Long) ⇒ longP(s"!= $n1")(n2) shouldEq (n2 != n1) }
  }

  property("predicate from \">=\" behaves properly") {
    forAll{ (n1: Long, n2: Long) ⇒ longP(s">= $n1")(n2) shouldEq (n2 >= n1) }
  }

  property("predicate from \">\" behaves properly") {
    forAll{ (n1: Long, n2: Long) ⇒ longP(s"> $n1")(n2) shouldEq (n2 > n1) }
  }

  property("predicate from \"<=\" behaves properly") {
    forAll{ (n1: Long, n2: Long) ⇒ longP(s"<= $n1")(n2) shouldEq (n2 <= n1) }
  }

  property("predicate from \"<\" behaves properly") {
    forAll{ (n1: Long, n2: Long) ⇒ longP(s"< $n1")(n2) shouldEq (n2 < n1) }
  }

  property("predicate from \"not\" behaves properly") {
    forAll{ (n1: Long, n2: Long) ⇒ longP(s"not(== $n1)")(n2) shouldEq (n2 != n1) }
  }

  property("predicate from \"and\" behaves properly") {
    forAll{ (n1: Long, n2: Long, n3: Long) ⇒
      longP(s"> $n1 and < $n2")(n3) shouldEq (n3 > n1 && n3 < n2) }
  }

  property("predicate from \"or\" behaves properly") {
    forAll{ (n1: Long, n2: Long, n3: Long) ⇒
      longP(s"> $n1 or < $n2")(n3) shouldEq (n3 > n1 || n3 < n2) }
  }
}

// vim: set ts=2 sw=2 et:
