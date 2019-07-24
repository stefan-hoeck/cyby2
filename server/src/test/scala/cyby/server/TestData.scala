/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server

import cyby.dat._

case class TestData[F[_],ID,PW,CR,MO](
  id:         ID,
  alias:      F[Alias],
  firstName:  F[Plain],
  lastName:   F[Plain],
  email:      F[Plain],
  password:   F[PW],
  level:      F[UserLevel],
  created:    CR,
  modified:   MO,
)

object TestData {
  type Id     = cyby.Id[this.type]
  type Srv    = TestData[Pure,Id,PWHash,TimeStamp,EditInfo]
  type SrvMod = TestData[Option,Undef,PWHash,Undef,EditInfo]

  val M = DerivedModifier[Srv,SrvMod]
}
