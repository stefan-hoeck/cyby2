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
import cyby.dat._, example._, UserLevel.{Admin,CommonUser,Guest}

trait UseImplicits extends AuthUtil with EditArbs {
  val S = UserS
  val imps = new Implicits
}

class UseTest extends UseImplicits {
  import imps.loadArb

  val mod: U.Mod = User[Option,Undef,PWHash,Undef,Undef](None,None,None,None,None,None,None,None,None)

  //----------------------------------------------------------------------
  //                         Authorization
  //----------------------------------------------------------------------

  property("BR-Use-auth-1: non-admins are not allowed to create new users"){
    forAll{ (u: UserLevel, a: U.Add) ⇒
      if (u < Admin) U.auth.add(u,a) shouldEq List(Unauthorized)
    }
  }

  property("BR-Use-auth-2: admins are allowed to create new users"){
    forAll{ (u: UserLevel, a: U.Add) ⇒
      if (u >= Admin) U.auth.add(u,a) shouldEq Nil
    }
  }

  property("BR-Use-auth-3: non-admins are not allowed to change other users"){
    forAll{ (u: U.Srv, o: U.Srv, n: U.Mod) ⇒
      if (u.level.v < Admin && (o.id =!= u.id))
      U.auth.mod(u,o,n) should contain(Unauthorized)
    }
  }

  property("BR-Use-auth-4: admins are allowed to change other users"){
    forAll{ (u: U.Srv, o: U.Srv, n: U.Mod) ⇒
      if (u.level.v >= Admin && (o.id =!= u.id))
      U.auth.mod(u,o,n) shouldEq Nil
    }
  }

  property("BR-Use-auth-5: guests are not allowed to change their own user"){
    forAll{ (u: U.Srv, n: U.Mod) ⇒
      if (u.level.v <= Guest) U.auth.mod(u,u,n) should contain(Unauthorized)
    }
  }

  property("BR-Use-auth-6: common users are allowed to change their own password"){
    forAll{ (u: U.Srv, p: PWHash) ⇒
      if (u.level.v > Guest)
      U.auth.mod(u,u,mod.copy(password = some(p))) shouldEq Nil
    }
  }

  property("BR-Use-auth-7: common users are allowed to change their first name"){
    forAll{ (u: U.Srv, p: Plain) ⇒
      if (u.level.v >= CommonUser)
      U.auth.mod(u,u,mod.copy(firstName = some(p))) shouldEq Nil
    }
  }

  property("BR-Use-auth-8: common users are allowed to change their last name"){
    forAll{ (u: U.Srv, p: Plain) ⇒
      if (u.level.v >= CommonUser)
      U.auth.mod(u,u,mod.copy(lastName = some(p))) shouldEq Nil
    }
  }

  property("BR-Use-auth-9: users are not allowed to change their own level"){
    forAll{ (u: U.Srv, l: UserLevel) ⇒
      if (u.level.v =!= l)
      U.auth.mod(u,u,mod.copy(level = some(l))) should contain(CantChangeLevel)
    }
  }

  property("BR-Use-auth-10: users are not allowed to change their own alias"){
    forAll{ (u: U.Srv, a: Alias) ⇒
      if (u.alias.v =!= a)
      U.auth.mod(u,u,mod.copy(alias = some(a))) should contain(CantChangeAlias)
    }
  }

  property("BR-Use-auth-11: non-admins are not allowed to delete users"){
    forAll{ (u: U.Srv, o: U.Srv) ⇒
      if (u.level.v < Admin)
      U.auth.del(u,o,o.id) should contain(Unauthorized)
    }
  }

  property("BR-Use-auth-12: admins are allowed to delete other users"){
    forAll{ (u: U.Srv, o: U.Srv) ⇒
      if (u.level.v >= Admin && (u.id =!= o.id))
      U.auth.del(u,o,o.id) shouldEq Nil
    }
  }

  property("BR-Use-auth-13: users cannot delete themselves"){
    forAll{ (u: U.Srv) ⇒
      U.auth.del(u,u,u.id) should contain(CantDeleteSelf)
    }
  }


  //----------------------------------------------------------------------
  //                         Valdation
  //----------------------------------------------------------------------

  property("BR-Use-valid-1: new users with an existing alias are invalid"){
    forAll{ (u: U.Srv, a: U.Add) ⇒
      val db  = Map(u.id -> u)
      val a2  = a.copy(alias = u.alias)
      val err = Exists(u.alias.v.name, UseP(u.id :: hnil))
      U.valid.add(db,a2) should contain(err)
    }
  }

  property("BR-Use-valid-2: new users with an new alias are valid"){
    forAll{ (u: U.Srv, a: U.Add) ⇒
      val db = Map(u.id -> u)
      if (u.alias =!= a.alias) U.valid.add(db,a) shouldEq Nil
    }
  }

  property("BR-Use-valid-3: changing a user's alias to an existing one is invalid"){
    forAll{ (u: U.Srv, o: U.Srv) ⇒
      val db  = Map(u.id -> u)
      val m   = mod.copy(alias = some(u.alias.v))
      val err = Exists(u.alias.v.name, UseP(u.id :: hnil))
      if (u.id =!= o.id) U.valid.mod(db,o,m) should contain(err)
    }
  }

  property("BR-Use-valid-4: changing a user's alias to itself is valid"){
    forAll{ (u: U.Srv) ⇒
      val db  = Map(u.id -> u)
      val m   = mod.copy(alias = some(u.alias.v))
      U.valid.mod(db,u,m) shouldEq Nil
    }
  }

  property("BR-Use-valid-5: changing a user's alias to a non-existing one is valid"){
    forAll{ (u: U.Srv, o: U.Srv, m: U.Mod) ⇒
      val db  = Map(u.id -> u)
      if (!m.alias.exists(u.alias.v =-= _)) U.valid.mod(db,u,m) shouldEq Nil
    }
  }

  property("BR-Use-valid-6: deleting a linked user is invalid"){
    forAll{ (is: List[User.Id], u: U.Srv) ⇒
      val err = StillLinked(UseP(u.id :: hnil))
      if (is.exists(u.id =-= _)) U.valid.del(is,u,u.id) should contain(err)
    }
  }

  property("BR-Use-valid-7: deleting a user no longer linked is valid"){
    forAll{ (is: List[User.Id], u: U.Srv) ⇒
      if (!is.exists(u.id =-= _)) U.valid.del(is,u,u.id) shouldEq Nil
    }
  }


  //----------------------------------------------------------------------
  //                         Editing
  //----------------------------------------------------------------------

  property("BR-Use-cud-1: new users are adjusted correctly"){
    forAll{ (is: Set[User.Id], u: U.Add, ei: EditInfo) ⇒
      val as = U.cud.doAdd(u)(ei -> is)
      is shouldNot contain(as.id)
      as.created shouldEq ei.timestamp
      as.modified shouldEq ei
    }
  }

  property("BR-Use-cud-2: users are created correctly"){
    forAll{ u: U.Srv ⇒ U.cud.toSrv(u) shouldEq u }
  }

  property("BR-Use-cud-3: user modifications are adjusted correctly"){
    forAll{ (u: U.Mod, ei: EditInfo) ⇒
      U.cud.adjMod(ei, u).modified shouldEq ei
    }
  }

  property("BR-Use-cud-4: users are modified correctly"){
    forAll{ (u: U.Srv, m: U.SrvMod) ⇒
      val u2 = U.cud.doMod(u,m)

      u2.id shouldEq u.id
      testMod(u2.alias, u.alias, m.alias)
      testMod(u2.firstName, u.firstName, m.firstName)
      testMod(u2.lastName, u.lastName, m.lastName)
      testMod(u2.email, u.email, m.email)
      testMod(u2.password, u.password, m.password)
      testMod(u2.level, u.level, m.level)
      u2.created shouldEq u.created
      u2.modified shouldEq m.modified
    }
  }


  //----------------------------------------------------------------------
  //                         Util
  //----------------------------------------------------------------------

  def est(st: St): U.EdSt = extract(U.edSt(st, hnil))

  property("UserS edEnv") { est(St.empty).nodes shouldEq (St.empty :: hnil) }

  property("UserS getSrv") {
    forAll{ s: U.Srv ⇒
      val e = PathNotFound(UseP(s.id.inc :: hnil)).e
      val st = St.empty.copy(uses = mkDB(s)(_.id))

      extract(U.getSrv(s.id, est(st))) shouldEq s
      errs(U.getSrv(s.id.inc, est(st))) shouldEq Nel.of(e)
    }
  }

  //----------------------------------------------------------------------
  //                         Json
  //----------------------------------------------------------------------

  checkLaws("json", JsonLaws[S.LoadEd].fromToStripped)
}
