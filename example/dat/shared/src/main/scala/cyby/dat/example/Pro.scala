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
  * Data type containing information about projects.
  *
  * Look at cyby.dat.example.Sup for a detailed description about
  * the type parameters used. Also, look at the companion object and
  * cyby.server.example.ProS for aliases with often used type parameters.
  *
  * @tparam F:  Effect, in which fields are wrapped. Typically set
  *             to Pure for mandatory fields and Maybe if fields
  *             are optional.
  * @tparam ID: ID of the data object
  * @tparam US: Type of users. Affects fields owner and users.
  * @tparam CR: Information about when the object was created
  * @tparam MO: Information about the last modification
  */
case class Pro[F[_],ID,US,CR,MO](
  id:          ID,
  name:        F[Name],
  owner:       F[US],
  users:       F[List[US]],
  comment:     F[Plain],
  created:     CR,
  modified:    MO,
)

object Pro extends DataCmp {
  /**
    * Path leading to a given project in the data tree.
    */
  type Path      = Id::HNil

  /**
    * Projects as seen by the client.
    */
  type Cli       = Pro[Pure,AccId,Link[Use.Id],TimeStamp,EditInfo]

  val lblG = LabelledGeneric[Cli]
  val lbls@(id::name::owner::users::comment::created::modified::HNil) = Keys[lblG.Repr].apply

  implicit def eqI[F[_],ID,US,ME,CR,MO]: cats.Eq[Pro[F,ID,US,CR,MO]] = cats.Eq.fromUniversalEquals
  implicit def decI[F[_]:D1,ID:D,US:D,CR:D,MO:D]: D[Pro[F,ID,US,CR,MO]] = deriveDecoder
  implicit def encI[F[_]:E1,ID:E,US:E,CR:E,MO:E]: E[Pro[F,ID,US,CR,MO]] = deriveEncoder
}
