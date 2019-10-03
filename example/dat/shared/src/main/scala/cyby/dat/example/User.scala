/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat
package example

import cyby.decoder1Instances._
import io.circe.generic.semiauto._
import shapeless.{::, HNil, LabelledGeneric}
import shapeless.ops.record._


/**
  * Data type containing information about users.
  *
  * Look at cyby.dat.example.Sup for a detailed description about
  * the type parameters used. Also, look at the companion object and
  * cyby.server.example.UserS for aliases with often used type parameters.
  *
  * @tparam F:  Effect, in which fields are wrapped. Typically set
  *             to Pure for mandatory fields and Maybe if fields
  *             are optional.
  * @tparam ID: ID of the data object
  * @tparam PW: Type of password field
  * @tparam CR: Information about when the object was created
  * @tparam MO: Information about the last modification
  */
case class User[F[_],ID,PW,CR,MO](
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

object User extends DataCmp {
  type AccId = HasAccess[Id]

  /**
    * Path leading to a given user in the data tree.
    */
  type Path      = Id :: HNil

  /**
    * Users as seen by the client.
    */
  type Cli       = User[Pure,AccId,Undef,TimeStamp,EditInfo]

  val lblG = LabelledGeneric[Cli]
  val lbls@(id::alias::firstName::lastName::email::password::level::created::modified::HNil) = Keys[lblG.Repr].apply

  implicit def eqI[F[_],ID,PW,CR,MO]: cats.Eq[User[F,ID,PW,CR,MO]] = cats.Eq.fromUniversalEquals
  implicit def decI[F[_]:D1,ID:D,PW:D,CR:D,MO:D]: D[User[F,ID,PW,CR,MO]] = deriveDecoder
  implicit def encI[F[_]:E1,ID:E,PW:E,CR:E,MO:E]: E[User[F,ID,PW,CR,MO]] = deriveEncoder
}
