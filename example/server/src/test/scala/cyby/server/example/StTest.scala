/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

class StTest extends EditUtil {
  def dbList[I,A](db: Map[I,A]): List[A] = db.toList map {_._2}

  def pros(st: St) = dbList(st.pros)
  def subs(st: St) = dbList(st.subs)
  def cons(st: St) = subs(st) flatMap {s ⇒ dbList(s.containers) }
  def bios(st: St) = cons(st) flatMap {c ⇒ dbList(c.bio) }
  def subFils(st: St) = subs(st) flatMap {s ⇒ dbList(s.files) }
  def conFils(st: St) = cons(st) flatMap {c ⇒ dbList(c.files) }
  def bioFils(st: St) = bios(st) flatMap {b ⇒ dbList(b.files) }

  property("linked users should contain all project owners"){
    forAll{st: St ⇒ 
      pros(st).foreach{ p ⇒ st.linkedUses should contain(p.owner.v) }
    }
  }

  property("linked users should contain all project users"){
    forAll{st: St ⇒ 
      pros(st).flatMap(_.users).foreach {r ⇒ st.linkedUses should contain(r) }
    }
  }

  property("linked suppliers should contain all container suppliers"){
    forAll{st: St ⇒ 
      cons(st).foreach{c ⇒ st.linkedSups should contain(c.supplier.v) }
    }
  }

  property("linked suppliers should contain all biodata suppliers"){
    forAll{st: St ⇒ 
      bios(st).foreach{b ⇒ st.linkedSups should contain(b.supplier.v) }
    }
  }

  property("linked methods should contain all biodata methods"){
    forAll{st: St ⇒ 
      bios(st).foreach{b ⇒ st.linkedMets should contain(b.method.v) }
    }
  }

  property("linked projects should contain a sub's project"){
    forAll{st: St ⇒ 
      subs(st).map(_.project.v).foreach{p ⇒ st.linkedPros should contain(p) }
    }
  }

  property("linked projects should contain a con's project"){
    forAll{st: St ⇒ 
      cons(st).map(_.project.v).foreach{p ⇒ st.linkedPros should contain(p) }
    }
  }

  property("linked projects should contain a bio's project"){
    forAll{st: St ⇒ 
      bios(st).map(_.project.v).foreach{p ⇒ st.linkedPros should contain(p) }
    }
  }

  property("linked projects should contain a subFil's project"){
    forAll{st: St ⇒ 
      subFils(st).map(_.project.v).foreach{p ⇒ st.linkedPros should contain(p) }
    }
  }

  property("linked projects should contain a conFil's project"){
    forAll{st: St ⇒ 
      conFils(st).map(_.project.v).foreach{p ⇒ st.linkedPros should contain(p) }
    }
  }

  property("linked projects should contain a bioFil's project"){
    forAll{st: St ⇒ 
      bioFils(st).map(_.project.v).foreach{p ⇒ st.linkedPros should contain(p) }
    }
  }
}

