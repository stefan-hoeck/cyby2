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
import cyby.dat._, example._, UserLevel.{Admin,CommonUser,Guest,Superuser}

trait ProImplicits extends AuthUtil with EditArbs {
  val S = ProjectS
  val imps = new Implicits
}

class ProTest extends ProImplicits {
  import imps.{idArb ⇒ _, modArb ⇒ _, _}
  implicit val smA: org.scalacheck.Arbitrary[S.Mod] = imps.modArb

  val mod: S.Mod = Project[Option,Undef,User.Id,Undef,Undef](None,None,None,None,None,None,None)

  //----------------------------------------------------------------------
  //                         Authorization
  //----------------------------------------------------------------------

  property("BR-Pro-auth-1: non superusers are not allowed to create new projects"){
    forAll{ (u: UserLevel, a: S.Add) ⇒ if (u < Superuser)
      S.auth.add(u,a) should contain(Unauthorized)
    }
  }

  property("BR-Pro-auth-2: superusers are allowed to create new projects"){
    forAll{ (u: UserLevel, a: S.Add) ⇒ if (u >= Superuser)
      S.auth.add(u,a) shouldEq Nil
    }
  }

  property("BR-Pro-auth-3: guests are not allowed to change projects"){
    forAll{ (u: U.Srv, o: S.Srv, n: S.Mod) ⇒
      if (u.level.v <= Guest) S.auth.mod(u,o,n) should contain(Unauthorized)
    }
  }

  property("BR-Pro-auth-4: admins are allowed to change projects"){
    forAll{ (u: U.Srv, o: S.Srv, n: S.Mod) ⇒
      if (u.level.v >= Admin) S.auth.mod(u,o,n) shouldEq Nil
    }
  }

  property("BR-Pro-auth-5: owners are allowed to change projects"){
    forAll{ (u: U.Srv, o: S.Srv, n: S.Mod) ⇒
      if (u.level.v >= CommonUser && (o.owner.v =-= u.id))
      S.auth.mod(u,o,n) shouldEq Nil
    }
  }

  property("BR-Pro-auth-6: non-owners are not allowed to change projects"){
    forAll{ (u: U.Srv, o: S.Srv, n: S.Mod) ⇒
      if (u.level.v < Admin && (o.owner.v =!= u.id))
      S.auth.mod(u,o,n) should contain(Unauthorized)
    }
  }

  property("BR-Pro-auth-7: non-admins are not allowed to delete projects"){
    forAll{ (u: UserLevel, o: S.Srv) ⇒
      if (u < Admin) S.auth.del(u,o,o.id) should contain(Unauthorized)
    }
  }

  property("BR-Pro-auth-8: admins are allowed to delete other projects"){
    forAll{ (u: UserLevel, o: S.Srv) ⇒
      if (u >= Admin) S.auth.del(u,o,o.id) shouldEq Nil
    }
  }


  //----------------------------------------------------------------------
  //                         Valdation
  //----------------------------------------------------------------------

  property("BR-Pro-valid-1: new projects with an existing name are invalid"){
    forAll{ (s: S.Srv, a: S.Add) ⇒
      val db  = Map(s.id -> s)
      val a2  = a.copy(name = s.name)
      val err = Exists(s.name.v, ProP(s.id :: hnil))
      S.valid.add(db,a2) should contain(err)
    }
  }

  property("BR-Pro-valid-2: new projects with an new name are valid"){
    forAll{ (s: S.Srv, a: S.Add) ⇒
      val db = Map(s.id -> s)
      if (a.name =!= s.name) S.valid.add(db,a) shouldEq Nil
    }
  }

  property("BR-Pro-valid-3: changing a project's name to an existing one is invalid"){
    forAll{ (s: S.Srv, o: S.Srv) ⇒
      val db  = Map(s.id -> s)
      val m   = mod.copy(name = some(s.name.v))
      val err = Exists(s.name.v, ProP(s.id :: hnil))
      if (s.id =!= o.id) S.valid.mod(db,o,m) should contain(err)
    }
  }

  property("BR-Pro-valid-4: changing a project's name to itself is valid"){
    forAll{ s: S.Srv ⇒
      val db  = Map(s.id -> s)
      val m   = mod.copy(name = some(s.name.v))
      S.valid.mod(db,s,m) shouldEq Nil
    }
  }

  property("BR-Pro-valid-5: changing a project's name to a non-existing one is valid"){
    forAll{ (s: S.Srv, o: S.Srv, m: S.Mod) ⇒
      val db  = Map(s.id -> s)
      if (!m.name.exists(s.name.v =-= _)) S.valid.mod(db,s,m) shouldEq Nil
    }
  }

  property("BR-Pro-valid-6: deleting a linked project is invalid"){
    forAll{ (is: List[Project.Id], s: S.Srv) ⇒
      val err = StillLinked(ProP(s.id :: hnil))
      if (is.exists(s.id =-= _)) S.valid.del(is,s,s.id) should contain(err)
    }
  }

  property("BR-Pro-valid-7: deleting a project no longer linked is valid"){
    forAll{ (is: List[Project.Id], s: S.Srv) ⇒
      if (!is.exists(s.id =-= _)) S.valid.del(is,s,s.id) shouldEq Nil
    }
  }


  //----------------------------------------------------------------------
  //                         Editing
  //----------------------------------------------------------------------

  property("BR-Pro-cud-1: new projects are adjusted correctly"){
    forAll{ (is: Set[Project.Id], s: S.Add, ei: EditInfo) ⇒
      val as = S.cud.doAdd(s)(ei -> is)
      is shouldNot contain(as.id)
      as.created shouldEq ei.timestamp
      as.modified shouldEq ei
    }
  }

  property("BR-Pro-cud-3: project modifications are adjusted correctly"){
    forAll{ (s: S.Mod, ei: EditInfo) ⇒
      S.cud.adjMod(ei, s).modified shouldEq ei
    }
  }

  property("BR-Pro-cud-4: projects are modified correctly"){
    forAll{ (s: S.Srv, m: S.SrvMod) ⇒
      val s2 = S.cud.doMod(s,m)

      s2.id shouldEq s.id
      testMod(s2.name, s.name, m.name)
      testMod(s2.comment, s.comment, m.comment)
      testMod(s2.owner, s.owner, m.owner)
      testMod(s2.users, s.users, m.users)
      s2.created shouldEq s.created
      s2.modified shouldEq m.modified
    }
  }


  //----------------------------------------------------------------------
  //                         Util
  //----------------------------------------------------------------------

  def est(st: St): S.EdSt = extract(S.edSt(st, hnil))

  property("ProjectS edEnv") { est(St.empty).nodes shouldEq (St.empty :: hnil) }

  property("ProjectS getSrv") {
    forAll{ s: S.Srv ⇒
      val e = PathNotFound(ProP(s.id.inc :: hnil)).e
      val st = St.empty.copy(pros = mkDB(s)(_.id))

      extract(S.getSrv(s.id, est(st))) shouldEq s
      errs(S.getSrv(s.id.inc, est(st))) shouldEq Nel.of(e)
    }
  }

  //----------------------------------------------------------------------
  //                         Json
  //----------------------------------------------------------------------

  checkLaws("json", JsonLaws[S.LoadEd].fromToStripped)
}
