/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

import cats.implicits._
import cyby.server.PWHash.check
import cyby.dat.{Alias, example}, example.{InvalidCreds, Use}

/**
  * Authentication module
  *
  * Implements the abstract members of cyby.server.auth.
  */
case class Auth(coreSettings: CoreSettings) extends auth[LoggedInEnv] with CyByZ {
  val M = CyByMonadIO.env[State]
  val authFailed  = cyby.dat.example.AuthenticationFailed
  val notLoggedIn = cyby.dat.example.NotLoggedIn
  def notFound(r: Request) = cyby.dat.example.NotFound(r.uri.path)

  def mkAuth(e: Env[St], u: UseS.Srv, h: String) = LoggedInEnv(e,u,h)
  def find(st: St, i: Use.Id) = UseS.child(st,i::hnil)

  def doAuth(st: St, name: String, p: String) = optionToErrNel(InvalidCreds)(
    (Alias(name) >>= st.names.get >>= st.uses.get).filter(u ⇒ check(p, u.password))
  )
}

// vim: set ts=2 sw=2 et:
