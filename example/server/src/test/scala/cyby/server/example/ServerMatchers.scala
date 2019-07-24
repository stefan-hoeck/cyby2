/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

import cats.Eq, cats.implicits._

import org.scalatest.matchers._

// todo: this should go to core with some other matchers
trait ServerMatchers {
  def haveFailed: Matcher[Either[_,_]] = Matcher{ v ⇒ 
    MatchResult(
      v.isLeft,
      s"Calculation did not fail: $v",
      s"Calculation failed: $v"
    )
  }

  def haveSucceeded: Matcher[Either[_,_]] = Matcher{ v ⇒ 
    MatchResult(
      v.isRight,
      s"Calculation failed: $v",
      s"Calculation did not fail: $v"
    )
  }

  def haveFailedWith[E:Eq](e: E): Matcher[Either[Nel[E],_]] = Matcher{ v ⇒
    MatchResult(
      v.fold(_.toList.exists(e === _) , _ ⇒ false),
      s"Calculation did not fail with $e: $v",
      s"Calculation failed with $e: $v"
    )
  }
}

// vim: set ts=2 sw=2 et:
