/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

import cats.implicits.{none ⇒ _, _}
import org.scalacheck.Arbitrary
import cyby.dat.{Mol ⇒ _, _}, example.Unauthorized, UserLevel.{Admin,CommonUser,Guest,Superuser}

abstract class AuthUtil extends EditUtil {
  def testBasicAuth[AE](e: ExampleEditor, abrv: String, name: String)
    (au: e.AuthorizerImpl[AE,AE,UserLevel], id: e.Srv ⇒ e.Id)
    (lvl: AE ⇒ UserLevel)
    (implicit AA: Arbitrary[e.Add],
              AS: Arbitrary[e.Srv],
              AM: Arbitrary[e.Mod],
              AE: Arbitrary[AE]) = {

    property(s"BR-${abrv}-auth-1: guests are not allowed to create new ${name}"){
      forAll{ (ae: AE, a: e.Add) ⇒ if (lvl(ae) <= Guest)
        au.add(ae,a) should contain(Unauthorized)
      }
    }

    property(s"BR-${abrv}-auth-2: guests are not allowed to change ${name}"){
      forAll{ (ae: AE, o: e.Srv, n: e.Mod) ⇒
        if (lvl(ae) <= Guest) au.mod(ae,o,n) should contain(Unauthorized)
      }
    }

    property(s"BR-${abrv}-auth-3: non-admins are not allowed to delete ${name}"){
      forAll{ (u: UserLevel, o: e.Srv) ⇒
        if (u < Admin) au.del(u,o,id(o)) should contain(Unauthorized)
      }
    }

    property(s"BR-${abrv}-auth-4: admins are allowed to delete other ${name}"){
      forAll{ (u: UserLevel, o: e.Srv) ⇒
        if (u >= Admin) au.del(u,o,id(o)) shouldEq Nil
      }
    }
  }

  def testCommonAuth(e: ExampleEditor, abrv: String, name: String)
    (au: e.AuthorizerImpl[UserLevel,UserLevel,UserLevel], id: e.Srv ⇒ e.Id)
    (implicit AA: Arbitrary[e.Add],
              AS: Arbitrary[e.Srv],
              AM: Arbitrary[e.Mod]) = {
    testBasicAuth(e, abrv, name)(au, id)(identity)

    property(s"BR-${abrv}-auth-5: common users are allowed to create new ${name}"){
      forAll{ (u: UserLevel, a: e.Add) ⇒
        if (u >= CommonUser) au.add(u,a) shouldEq Nil
      }
    }

    property(s"BR-${abrv}-auth-4: common users are allowed to change ${name}"){
      forAll{ (u: UserLevel, o: e.Srv, n: e.Mod) ⇒
        if (u >= CommonUser) au.mod(u,o,n) shouldEq Nil
      }
    }
  }

  def testProAuth(e: ExampleEditor, abrv: String, name: String)(
    au: e.ProAuth, id: e.Srv ⇒ e.Id
  )(implicit AA: Arbitrary[e.Add],
             AS: Arbitrary[e.Srv],
             AM: Arbitrary[e.Mod]) = {

    property("BR-${abrs}-auth-0: auth equals the tested implementation") {
      assert(e.auth == au.auth)
    }

    testBasicAuth(e, abrv, name)(au.auth, id)(_._2.lvl)

    property(s"BR-${abrv}-auth-5: users with access the project are allowed to create new ${name}"){
      forAll{ (ae: AuthEnv, a: e.Add) ⇒
        if (ae.lvl >= CommonUser && ae.canAccess(au.ap.get(a).v))
        au.auth.add(Nil -> ae,a) shouldEq Nil
      }
    }

    property(s"BR-${abrv}-auth-6: users without access to the selected project are not allowed to create new ${name}"){
      forAll{ (ae: AuthEnv, a: e.Add) ⇒
        if (ae.lvl >= CommonUser && !ae.canAccess(au.ap.get(a).v))
        au.auth.add(Nil -> ae,a) should contain(Unauthorized)
      }
    }

    property(s"BR-${abrv}-auth-7: users with access the project are allowed to change ${name} if they don't change the project"){
      forAll{ (ae: AuthEnv, o: e.Srv, n: e.Mod) ⇒
        if (ae.lvl >= CommonUser && ae.canAccess(au.sp.get(o).v))
        au.auth.mod(Nil -> ae,o,au.mp.set(n)(None)) shouldEq Nil
      }
    }

    property(s"BR-${abrv}-auth-8: only super-users are allowed to change ${name} project"){
      forAll{ (ae: AuthEnv, o: e.Srv, n: e.Mod) ⇒
        if (au.mp.get(n).nonEmpty) {
          if (ae.lvl < Superuser)
            au.auth.mod(Nil -> ae,o,n) should contain(Unauthorized)
          else {
            if (ae.canAccess(au.sp.get(o).v) && ae.canAccess(au.mp.get(n).get))
              au.auth.mod(Nil -> ae,o,n) shouldEq Nil
            else
              au.auth.mod(Nil -> ae,o,n) should contain(Unauthorized)
          }
        }
      }
    }
  }
}

