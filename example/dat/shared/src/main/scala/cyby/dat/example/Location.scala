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
  * Data type containing information about storage locations.
  *
  * Look at cyby.dat.example.Sup for a detailed description about
  * the type parameters used. Also, look at the companion object and
  * cyby.server.example.LocationS for aliases with often used type parameters.
  *
  * @tparam F:  Effect, in which fields are wrapped. Typically set
  *             to Pure for mandatory fields and Maybe if fields
  *             are optional.
  * @tparam ID: ID of the data object
  * @tparam CR: Information about when the object was created
  * @tparam MO: Information about the last modification
  */

case class Location[F[_],ID,CR,MO](
  id:          ID,
  name:        F[Name],
  comment:     F[Plain],
  created:     CR,
  modified:    MO,
)

object Location extends DataCmp {
  /**
    * Path leading to a given storage location in the data tree.
    */
  type Path      = Id::HNil

  /**
    * Storage locations as seen by the client.
    */
  type Cli       = Location[Pure,Id,TimeStamp,EditInfo]

  val lblG = LabelledGeneric[Cli]
  val lbls@(id::name::comment::created::modified::HNil) = Keys[lblG.Repr].apply

  implicit def eqI[F[_],ID,CR,MO]: cats.Eq[Location[F,ID,CR,MO]] = cats.Eq.fromUniversalEquals
  implicit def decI[F[_]:D1,ID:D,CR:D,MO:D]: D[Location[F,ID,CR,MO]] = deriveDecoder
  implicit def encI[F[_]:E1,ID:E,CR:E,MO:E]: E[Location[F,ID,CR,MO]] = deriveEncoder
}

