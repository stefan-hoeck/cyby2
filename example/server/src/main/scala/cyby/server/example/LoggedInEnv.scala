/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

import cyby.dat.{EditInfo, TimeStamp, example}, example.{LoggedIn,Result,USettings}

/**
  * Environment for requests of logged in users
  */
case class LoggedInEnv(env: Env[St], u: UserS.Srv, h: String) {
  lazy val editEnv: EditEnv = EditEnv(env.copy(st = ()), u, h)

  def st: St = env.st

  lazy val settings: USettings = st settingsFor u.id

  def authEnv: AuthEnv = authSt.env

  lazy val authSt:  AuthSt  = AuthSt(st, u)

  lazy val editInfo: EditInfo = EditInfo(TimeStamp mk env.timestamp, u.id.v, none)

  def loginRes: Result =
    LoggedIn(h, u.copy(id = authSt.env.accUse(u.id).get, password = Pure(undef)), settings)

  def ei  = editInfo
  def as  = authSt
  def ae  = authEnv
  def lvl = u.level.v
}
