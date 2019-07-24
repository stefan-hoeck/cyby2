/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby

import cats.Eq, cats.implicits._
import cyby.syntax._

import org.scalacheck.Arbitrary
import shapeless.{::, HNil}
import org.scalacheck.Shapeless._

class HQTest extends CyBySuite {
  implicit val metI = shapeless.the[Arbitrary[Met]]
  implicit val proI = shapeless.the[Arbitrary[Pro]]
  implicit val roI = shapeless.the[Arbitrary[Root]]

  property("head node") {
    forAll{ p: (Root,Id[Pro]) ⇒ 
      val (r,id) = p
      HQ.root(r).query(lens[Root].pros.at)(id).map(_.node) shouldEq
      r.pros.get(id)
    }
  }

  property("setting") {
    forAll{ p: (Root,Pro) ⇒ 
      val (r,pro) = p
      r.pros.foreach {
        case (id,_) ⇒ {
          val hq = HQ.root(r).query(lens[Root].pros.at)(id).get
          val rn = hq.set(pro).tail.head

          rn.pros shouldEq (r.pros + (id -> pro))
        }
      }
    }
  }

  property("deleting") {
    forAll{ p: (Root,Pro) ⇒ 
      val (r,pro) = p
      r.pros.foreach {
        case (id,_) ⇒ {
          val hq = HQ.root(r).query(lens[Root].pros.at)(id).get
          val rn = hq.delete.head

          rn.pros shouldEq (r.pros - id)
        }
      }
    }
  }

  property("deep setting") {
    forAll{ p: (Root,Met) ⇒ 
      val (r,met) = p
      r.pros.foreach {
        case (pid,p) ⇒ {
          p.mets.foreach {
            case (mid,m) ⇒ {
              val hq = HQ.root(r).query(lens[Root].pros.at)(pid).get
                                 .query(lens[Pro].mets.at)(mid).get
              val (mn::pn::rn::HNil) = hq.set(met)
              val pexp = lens[Pro].mets.modify(p)(_ + (mid -> met))
              val rexp = lens[Root].pros.modify(r)(_ + (pid -> pexp))

              mn shouldEq met
              pn shouldEq pexp
              rn shouldEq rexp
            }
          }
        }
      }
    }
  }

  property("deep deleting") {
    forAll{ p: (Root,Met) ⇒ 
      val (r,met) = p
      r.pros.foreach {
        case (pid,p) ⇒ {
          p.mets.foreach {
            case (mid,m) ⇒ {
              val hq = HQ.root(r).query(lens[Root].pros.at)(pid).get
                                 .query(lens[Pro].mets.at)(mid).get
              val (pn::rn::HNil) = hq.delete
              val pexp = lens[Pro].mets.modify(p)(_ - mid)
              val rexp = lens[Root].pros.modify(r)(_ + (pid -> pexp))

              pn shouldEq pexp
              rn shouldEq rexp
            }
          }
        }
      }
    }
  }
}

case class Root(pros: Map[Id[Pro],Pro])

object Root{
  implicit val eqI: Eq[Root] = Eq.fromUniversalEquals
}

case class Pro(
  id:      Id[Pro],
  name:    String,
  comment: Option[String],
  mets:    Map[Id[Met],Met]
)

object Pro{
  implicit val eqI: Eq[Pro] = Eq.fromUniversalEquals
}

case class Met(
  id:      Id[Met],
  name:    String,
  comment: Option[String],
)

object Met{
  implicit val eqI: Eq[Met] = Eq.fromUniversalEquals
}
