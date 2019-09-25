/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

import cats.implicits._
import cyby.dat.{Mol ⇒ _, _}, UserLevel.{Admin,CommonUser,Guest,Superuser}

class UtilTest extends EditUtil {
  private implicit def proSrvAI = proSrvA

  property("user categories are correct") {
    forAll{ u: U.Srv ⇒ 
      assert(isAdmin(u.copy(level = Pure(Admin))))
      assert(isSuperUser(u.copy(level = Pure(Admin))))
      assert(isUser(u.copy(level = Pure(Admin))))
      assert(isGuest(u.copy(level = Pure(Admin))))

      assert(!isAdmin(u.copy(level = Pure(Superuser))))
      assert(isSuperUser(u.copy(level = Pure(Superuser))))
      assert(isUser(u.copy(level = Pure(Superuser))))
      assert(isGuest(u.copy(level = Pure(Superuser))))

      assert(!isAdmin(u.copy(level = Pure(CommonUser))))
      assert(!isSuperUser(u.copy(level = Pure(CommonUser))))
      assert(isUser(u.copy(level = Pure(CommonUser))))
      assert(isGuest(u.copy(level = Pure(CommonUser))))

      assert(!isAdmin(u.copy(level = Pure(Guest))))
      assert(!isSuperUser(u.copy(level = Pure(Guest))))
      assert(!isUser(u.copy(level = Pure(Guest))))
      assert(isGuest(u.copy(level = Pure(Guest))))
    }
  }

  property("isOwner works correctly") {
    forAll{ (u: U.Srv, p: ProjectS.Srv) ⇒
      isOwner(u,p) shouldEq (u.id =-= p.owner.v)
    }
  }

  property("owners have access to project") {
    forAll{ (u: U.Srv, p: ProjectS.Srv) ⇒
      if (isOwner(u,p)) assert(hasAccess(u,p))
    }
  }

  property("hasAccess works correctly") {
    forAll{ (u: U.Srv, p: ProjectS.Srv) ⇒
      if (!isOwner(u,p))
      hasAccess(u,p) shouldEq p.users.v.toSet(u.id)
    }
  }

  property("accessiblePros works correctly") {
    forAll{ (u: U.Srv, p: ProjectS.DB) ⇒
      val ps = accessiblePros(u, p)

      if (u.level.v >= Admin) ps shouldEq p.keySet
      else {
        ps.foreach{i ⇒ assert(hasAccess(u, p(i))) }
        p.foreach{ case (i,p2) ⇒ hasAccess(u,p2) shouldEq ps(i) }
      }
    }
  }
}
