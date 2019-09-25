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
  * Data type containing information about results from biological tests.
  *
  * Look at cyby.dat.example.Sup for a detailed description about
  * the type parameters used. Also, look at the companion object and
  * cyby.server.example.BiodataEntryS for aliases with often used type parameters.
  *
  * @tparam F:  Effect, in which fields are wrapped. Typically set
  *             to Pure for mandatory fields and Maybe if fields
  *             are optional.
  * @tparam ID: ID of the data object
  * @tparam M:  Type of method field
  * @tparam S:  Type of supplier field
  * @tparam P:  Type of project field
  * @tparam FS: Type of files field
  * @tparam CR: Information about when the object was created
  * @tparam MO: Information about the last modification
  */
case class BiodataEntry[F[_],ID,M,S,P,FS,CR,MO](
  id:           ID,
  value:        F[Double],
  method:       F[M],
  supplier:     F[S],
  date:         F[Date],
  comment:      F[Plain],
  project:      F[P],
  files:        FS,
  created:      CR,
  modified:     MO,
)


object BiodataEntry extends DataCmp {
  /**
    * Path leading to a given bio entry in the data tree.
    */
  type Path      = Id::Container.Path

  /**
    * Path leading to a file linked to a bio entry.
    */
  type FilPath   = Fil.Id::Path

  /**
    * Bio entries as seen by the client.
    */
  type Cli       = BiodataEntry[Pure,Id,Link[Method.Id],Link[Sup.Id],Link[Project.AccId],List[Fil.Cli],TimeStamp,EditInfo]

  val lblG = LabelledGeneric[Cli]
  val lbls@(id::value::method::supplier::date::comment::project::files::created::modified::HNil) = Keys[lblG.Repr].apply

  implicit def eqI[F[_],ID,M,S,P,FS,CR,MO]: cats.Eq[BiodataEntry[F,ID,M,S,P,FS,CR,MO]] = cats.Eq.fromUniversalEquals
  implicit def decI[F[_]:D1,ID:D,M:D,S:D,P:D,FS:D,CR:D,MO:D]: D[BiodataEntry[F,ID,M,S,P,FS,CR,MO]] = deriveDecoder
  implicit def encI[F[_]:E1,ID:E,M:E,S:E,P:E,FS:E,CR:E,MO:E]: E[BiodataEntry[F,ID,M,S,P,FS,CR,MO]] = deriveEncoder
}
