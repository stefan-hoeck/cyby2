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
  * Data type containing information about linked files.
  *
  * Look at cyby.dat.example.Sup for a detailed description about
  * the type parameters used. Also, look at the companion object and
  * cyby.server.example.SubFilS for aliases with often used type parameters.
  *
  * @tparam F:  Effect, in which fields are wrapped. Typically set
  *             to Pure for mandatory fields and Maybe if fields
  *             are optional.
  * @tparam ID: ID of the data object
  * @tparam P:  Type of project field
  * @tparam CR: Information about when the object was created
  * @tparam MO: Information about the last modification
  */
case class Fil[F[_],ID,P,CR,MO](
  id:            ID,
  name:          F[Name],
  path:          F[FileName],
  comment:       F[Plain],
  project:       F[P],
  created:       CR,
  modified:      MO,
)

object Fil extends DataCmp{
  /**
    * Fil entries as seen by the client.
    */
  type Cli       = Fil[Pure,Id,Link[Project.AccId],TimeStamp,EditInfo]

  val lblG = LabelledGeneric[Cli]
  val lbls@(id::name::path::comment::project::created::modified::HNil) = Keys[lblG.Repr].apply

  implicit def eqI[F[_],ID,P,CR,MO]: cats.Eq[Fil[F,ID,P,CR,MO]] = cats.Eq.fromUniversalEquals
  implicit def decI[F[_]:D1,ID:D,P:D,CR:D,MO:D]: D[Fil[F,ID,P,CR,MO]] = deriveDecoder
  implicit def encI[F[_]:E1,ID:E,P:E,CR:E,MO:E]: E[Fil[F,ID,P,CR,MO]] = deriveEncoder
}

