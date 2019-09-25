/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

import tagInstances._
import cats.implicits.{none ⇒ _, _}, cyby.shapelessImplicits._
import cyby.dat.{Mol ⇒ _, _}, example._

trait ConImplicits extends AuthUtil with EditArbs {
  val S = ContainerS
  val imps = new Implicits
}

object ConImplicits extends ConImplicits


class ConTest extends ConImplicits {
  import imps.{idArb ⇒ _, modArb ⇒ _, _}
  implicit val smA: org.scalacheck.Arbitrary[S.Mod] = imps.modArb

  val mod: S.Mod = Container[Option,Undef,Location.Id,Sup.Id,Project.Id,Undef,Undef,Undef,Undef](None,None,None,None,None,None,None,None,None,None,None,None,None,None,None,None,None,None)

  //----------------------------------------------------------------------
  //                         Authorization
  //----------------------------------------------------------------------

  testProAuth(ContainerS, "Con", "containers")(ContainerS.au, _.id)


  //----------------------------------------------------------------------
  //                         Valdation
  //----------------------------------------------------------------------

  property("BR-Con-valid-1: new containers with an existing batch-nr are invalid"){
    forAll{ (s: S.Srv, a: S.Add, i: Sub.Id) ⇒
      val db  = Map(s.id -> s)
      val a2  = a.copy(batch = s.batch)
      val pp  = i :: hnil

      if (s.batch.v.v.nonEmpty) {
        val err = BatchExists(s.batch.v, s.id :: pp)
        S.valid.add(pp -> db,a2) should contain(err)
      }
    }
  }

  property("BR-Con-valid-2: changing a container's name to an existing one is invalid"){
    forAll{ (s: S.Srv, o: S.Srv, b: Plain, i: Sub.Id) ⇒
      if (b.v.nonEmpty) {
        val pp  = i :: hnil
        val s2  = s.copy(batch = Pure(b))
        val db  = Map(s2.id -> s2)
        val m   = lens[S.Mod].batch.set(mod)(some(b))
        val err = BatchExists(b, s.id :: pp)

        if (s.id =!= o.id) S.valid.mod(pp -> db,o,m) should contain(err)
        else S.valid.mod(pp -> db,o,m) shouldNot contain(err)
      }
    }
  }

  //----------------------------------------------------------------------
  //                         Editing
  //----------------------------------------------------------------------

  property("BR-Con-cud-1: new containers are adjusted correctly"){
    forAll{ (is: Set[Container.Id], s: S.Add, ei: EditInfo) ⇒
      val as = S.cud.doAdd(s)(ei -> is)
      is shouldNot contain(as.id)
      as.created shouldEq ei.timestamp
      as.modified shouldEq ei
    }
  }

  property("BR-Con-cud-2: containers are created correctly"){
    forAll{ s: S.SrvAdd ⇒
      S.cud.toSrv(s).bio.v shouldEq Map()
      S.cud.toSrv(s).files.v shouldEq Map()
    }
  }

  property("BR-Con-cud-3: container modifications are adjusted correctly"){
    forAll{ (s: S.Mod, ei: EditInfo) ⇒
      S.cud.adjMod(ei, s).modified shouldEq ei
    }
  }

  property("BR-Con-cud-4: containers are modified correctly"){
    forAll{ (s: S.Srv, m: S.SrvMod) ⇒
      val s2 = S.cud.doMod(s,m)

      s2.id shouldEq s.id
      testMod(s2.location, s.location, m.location)
      testMod(s2.supplier, s.supplier, m.supplier)
      testMod(s2.batch, s.batch, m.batch)
      testMod(s2.orderNr, s.orderNr, m.orderNr)
      testMod(s2.comment, s.comment, m.comment)
      testMod(s2.lentTo, s.lentTo, m.lentTo)
      testMod(s2.purity, s.purity, m.purity)
      testMod(s2.purityStr, s.purityStr, m.purityStr)
      testMod(s2.density, s.density, m.density)
      testMod(s2.concentration, s.concentration, m.concentration)
      testMod(s2.amount, s.amount, m.amount)
      testMod(s2.empty, s.empty, m.empty)
      testMod(s2.project, s.project, m.project)
      s2.bio shouldEq s.bio
      s2.files shouldEq s.files
      s2.created shouldEq s.created
      s2.modified shouldEq m.modified
    }
  }


  //----------------------------------------------------------------------
  //                         Util
  //----------------------------------------------------------------------

  def est(st: St, s: SubS.Srv): S.EdSt = extract(S.edSt(st, s.id :: hnil))

  property("ContainerS edEnv") {
    forAll{ s: SubS.Srv ⇒
      val st = subSt(s)
      val e = PathNotFound(SubP(s.id.inc :: hnil)).e

      est(st,s).nodes shouldEq (s::st::hnil)
      errs(S.edSt(st,s.id.inc::hnil)) shouldEq Nel.of(e)
    }
  }

  property("ContainerS getSrv") {
    forAll{ (s: SubS.Srv, c: S.Srv) ⇒
      val st = conSt(c, s)
      val e = PathNotFound(ConP(c.id.inc :: s.id :: hnil)).e

      extract(S.getSrv(c.id, est(st, s))) shouldEq c
      errs(S.getSrv(c.id.inc, est(st, s))) shouldEq Nel.of(e)
    }
  }

  //----------------------------------------------------------------------
  //                         Json
  //----------------------------------------------------------------------

  checkLaws("json", JsonLaws[S.LoadEd].fromToStripped)
}
