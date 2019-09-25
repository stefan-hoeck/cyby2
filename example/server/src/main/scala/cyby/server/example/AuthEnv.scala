/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

import cyby.dat.example.{Project, Unauthorized, Use}

/**
  * Environment used for authentication and authorization
  * of users
  */
case class AuthEnv(
  user:      UseS.Srv,
  canAccess: Set[Project.Id],
){
  def lvl: cyby.dat.UserLevel = user.level.v

  def accAll(ps: List[Project.Id]): Boolean = ps forall canAccess

  def authAdd(ps: List[Project.Id]): List[Err] =
    must(isUser(user) && accAll(ps))(Unauthorized)

  def authMod(
    ids: List[Project.Id],
    o: Option[Project.Id]
  ): List[Err] = o match {
    case Some(n) ⇒ must(isSuperUser(user) && canAccess(n) && accAll(ids))(Unauthorized)
    case _       ⇒ must(isUser(user) && accAll(ids))(Unauthorized)
  }

  val accPro: Pure[Project.Id] ⇒ Option[Pure[Project.AccId]] =
    p ⇒ if (canAccess(p.v)) Some(Pure(p.v.to)) else None

  def accUse(u: Use.Id): Option[Use.AccId] =
    if (actual(user)(u) || isSuperUser(user)) Some(u.to) else None
}

object AuthEnv{
  def apply(u: UseS.Srv, st: St): AuthEnv = AuthEnv(u, accessiblePros(u, st.pros))
}

// vim: set ts=2 sw=2 et:
