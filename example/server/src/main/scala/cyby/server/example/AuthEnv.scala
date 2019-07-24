/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

import cyby.dat.example.{Pro, Unauthorized, Use}

/**
  * Environment used for authentication and authorization
  * of users
  */
case class AuthEnv(
  user:      UseS.Srv,
  canAccess: Set[Pro.Id],
){
  def lvl: cyby.dat.UserLevel = user.level.v

  def accAll(ps: List[Pro.Id]): Boolean = ps forall canAccess

  def authAdd(ps: List[Pro.Id]): List[Err] =
    must(isUser(user) && accAll(ps))(Unauthorized)

  def authMod(
    ids: List[Pro.Id],
    o: Option[Pro.Id]
  ): List[Err] = o match {
    case Some(n) ⇒ must(isSuperUser(user) && canAccess(n) && accAll(ids))(Unauthorized)
    case _       ⇒ must(isUser(user) && accAll(ids))(Unauthorized)
  }

  val accPro: Pure[Pro.Id] ⇒ Option[Pure[Pro.AccId]] =
    p ⇒ if (canAccess(p.v)) Some(Pure(p.v.to)) else None

  def accUse(u: Use.Id): Option[Use.AccId] =
    if (actual(user)(u) || isSuperUser(user)) Some(u.to) else None
}

object AuthEnv{
  def apply(u: UseS.Srv, st: St): AuthEnv = AuthEnv(u, accessiblePros(u, st.pros))
}

// vim: set ts=2 sw=2 et:
