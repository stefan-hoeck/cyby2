/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

/**
  * Environment for editing requests of logged in users
  */
case class EditEnv(env: Env[Unit], u: UseS.Srv, h: String) {
  def loggedInEnv(st: St): LoggedInEnv = LoggedInEnv(env.copy(st = st), u, h)
}
