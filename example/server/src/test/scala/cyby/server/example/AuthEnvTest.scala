/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

import cats.implicits._
import cyby.dat.{UserLevel,example}, example.{Project, Unauthorized, User}
import UserLevel.{Superuser,CommonUser}

class AuthEnvTest extends EditUtil {
  property("accAll behaves correctly") {
    forAll{ (ae: AuthEnv, n: List[ProjectS.Id]) ⇒
      ae.accAll(n) shouldEq (n.toList =-= n.filter(ae.canAccess))
    }
  }

  property("authAdd behaves correctly") {
    forAll{ (ae: AuthEnv, ps: List[Project.Id]) ⇒
      val res = ae.authAdd(ps)

      if (!ae.accAll(ps)) res should contain(Unauthorized)
      else if (ae.lvl < CommonUser) res should contain(Unauthorized)
      else res shouldEq Nil
    }
  }

  property("authMod behaves correctly") {
    forAll{ (ae: AuthEnv, ps: List[Project.Id], o: Option[Project.Id]) ⇒
      val res = ae.authMod(ps, o)

      o match {
        case Some(pn) ⇒ 
          if (!ae.accAll(ps)) res should contain(Unauthorized)
          else if (!ae.canAccess(pn)) res should contain(Unauthorized)
          else if (ae.lvl < Superuser) res should contain(Unauthorized)
          else res shouldEq Nil
        case None      ⇒ 
          if (!ae.accAll(ps)) res should contain(Unauthorized)
          else if (ae.lvl < CommonUser) res should contain(Unauthorized)
          else res shouldEq Nil
      }
    }
  }

  property("project behaves correctly") {
    forAll{ (ae: AuthEnv, p: Project.Id) ⇒
      ae.project(p).nonEmpty shouldEq ae.canAccess(p)

      if (ae.project(p).nonEmpty) ae.project(p).get.v.v shouldEq p
    }
  }

  property("user behaves correctly") {
    forAll{ (ae: AuthEnv, i: User.Id) ⇒
      if (i =-= ae.id) assert(ae.user(i).nonEmpty)
      else if (ae.lvl < Superuser) assert(ae.user(i).isEmpty)
      else assert(ae.user(i).nonEmpty)
    }
  }

  property("creation works correctly") {
    forAll{ (st: St, u: UserS.Srv) ⇒
      authEnv(u, st).canAccess shouldEq accessiblePros(u, st.pros)
    }
  }
}
