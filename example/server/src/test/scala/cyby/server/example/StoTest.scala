/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

import tagInstances._
import cats.implicits._, cyby.shapelessImplicits._
import cyby.dat._, example._

trait StoImplicits extends AuthUtil with EditArbs {
  val S = LocationS
  val imps = new Implicits
}

class StoTest extends StoImplicits {
  import imps.{idArb ⇒ _, modArb ⇒ _, _}
  implicit val smA: org.scalacheck.Arbitrary[S.Mod] = imps.modArb

  val mod: S.Mod = Location[Option,Undef,Undef,Undef](None,None,None,None,None)

  //----------------------------------------------------------------------
  //                         Authorization
  //----------------------------------------------------------------------

  testCommonAuth(LocationS, "Sto", "storage locations")(LocationS.auth, _.id)

  //----------------------------------------------------------------------
  //                         Valdation
  //----------------------------------------------------------------------

  property("BR-Sto-valid-1: new storage locations with an existing name are invalid"){
    forAll{ (s: S.Srv, a: S.Add) ⇒
      val db  = Map(s.id -> s)
      val a2  = a.copy(name = s.name)
      val err = Exists(s.name.v, StoP(s.id :: hnil))
      S.valid.add(db,a2) should contain(err)
    }
  }

  property("BR-Sto-valid-2: new storage locations with an new name are valid"){
    forAll{ (s: S.Srv, a: S.Add) ⇒
      val db = Map(s.id -> s)
      if (a.name =!= s.name) S.valid.add(db,a) shouldEq Nil
    }
  }

  property("BR-Sto-valid-3: changing a storage locations's name to an existing one is invalid"){
    forAll{ (s: S.Srv, o: S.Srv) ⇒
      val db  = Map(s.id -> s)
      val m   = mod.copy(name = some(s.name.v))
      val err = Exists(s.name.v, StoP(s.id :: hnil))

      if (s.id =!= o.id) S.valid.mod(db,o,m) should contain(err)
      else S.valid.mod(db,o,m) shouldEq Nil
    }
  }

  property("BR-Sto-valid-4: changing a storage locations's name to a non-existing one is valid"){
    forAll{ (s: S.Srv, o: S.Srv, m: S.Mod) ⇒
      val db  = Map(s.id -> s)
      if (!m.name.exists(s.name.v =-= _)) S.valid.mod(db,s,m) shouldEq Nil
    }
  }

  property("BR-Sto-valid-5: deleting a linked storage locations is invalid"){
    forAll{ (is: List[Location.Id], s: S.Srv) ⇒
      val err = StillLinked(StoP(s.id :: hnil))

      if (is.exists(s.id =-= _)) S.valid.del(is,s,s.id) should contain(err)
      else S.valid.del(is,s,s.id) shouldEq Nil
    }
  }


  //----------------------------------------------------------------------
  //                         Editing
  //----------------------------------------------------------------------

  property("BR-Sto-cud-1: new storage locations are adjusted correctly"){
    forAll{ (is: Set[Location.Id], s: S.Add, ei: EditInfo) ⇒
      val as = S.cud.doAdd(s)(ei -> is)
      is shouldNot contain(as.id)
      as.created shouldEq ei.timestamp
      as.modified shouldEq ei
    }
  }

  property("BR-Sto-cud-2: storage locations are created correctly"){
    forAll{ s: S.Srv ⇒ S.cud.toSrv(s) shouldEq s }
  }

  property("BR-Sto-cud-3: storage locations modifications are adjusted correctly"){
    forAll{ (s: S.Mod, ei: EditInfo) ⇒
      S.cud.adjMod(ei, s).modified shouldEq ei
    }
  }

  property("BR-Sto-cud-4: storage locations are modified correctly"){
    forAll{ (s: S.Srv, m: S.SrvMod) ⇒
      val s2 = S.cud.doMod(s,m)

      s2.id shouldEq s.id
      testMod(s2.name, s.name, m.name)
      testMod(s2.comment, s.comment, m.comment)
      s2.created shouldEq s.created
      s2.modified shouldEq m.modified
    }
  }


  //----------------------------------------------------------------------
  //                         Util
  //----------------------------------------------------------------------

  def est(st: St): S.EdSt = extract(S.edSt(st, hnil))

  property("LocationS edEnv") { est(St.empty).nodes shouldEq (St.empty :: hnil) }

  property("LocationS getSrv") {
    forAll{ s: S.Srv ⇒
      val e = PathNotFound(StoP(s.id.inc :: hnil)).e
      val st = St.empty.copy(stos = mkDB(s)(_.id))

      extract(S.getSrv(s.id, est(st))) shouldEq s
      errs(S.getSrv(s.id.inc, est(st))) shouldEq Nel.of(e)
    }
  }

  //----------------------------------------------------------------------
  //                         Json
  //----------------------------------------------------------------------

  checkLaws("json", JsonLaws[S.LoadEd].fromToStripped)

}

