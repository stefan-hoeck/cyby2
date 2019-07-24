/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server

class ModifierTest extends ServerSuite with Generators {
  property("generically derived modifier behaves correctly"){
    forAll{ (s: TestData.Srv, m: TestData.SrvMod) ⇒
      val s2 = TestData.M(s,m)
      s2.id shouldEq s.id
      if (m.alias.nonEmpty) s2.alias.v shouldEq m.alias.get
      if (m.firstName.nonEmpty) s2.firstName.v shouldEq m.firstName.get
      if (m.lastName.nonEmpty) s2.lastName.v shouldEq m.lastName.get
      if (m.email.nonEmpty) s2.email.v shouldEq m.email.get
      if (m.password.nonEmpty) s2.password.v shouldEq m.password.get
      if (m.level.nonEmpty) s2.level.v shouldEq m.level.get
      s2.created shouldEq s.created
      s2.modified shouldEq m.modified
    }
  }
}
