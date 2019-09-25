/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

case class AuthSt (
  st:     St,
  user:   UserS.Srv,
){
  lazy val env: AuthEnv = AuthEnv(user, st)
}

// vim: set ts=2 sw=2 et:
