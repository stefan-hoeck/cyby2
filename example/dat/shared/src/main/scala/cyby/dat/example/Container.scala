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
  * Data type containing information about containers.
  *
  * Look at cyby.dat.example.Sup for a detailed description about
  * the type parameters used. Also, look at the companion object and
  * cyby.server.example.ContainerS for aliases with often used type parameters.
  *
  * @tparam F:  Effect, in which fields are wrapped. Typically set
  *             to Pure for mandatory fields and Maybe if fields
  *             are optional.
  * @tparam ID: ID of the data object
  * @tparam LO: Type of location field
  * @tparam SU: Type of supplier field
  * @tparam P:  Type of project field
  * @tparam BS: Type of bio field
  * @tparam FS: Type of files field
  * @tparam CR: Information about when the object was created
  * @tparam MO: Information about the last modification
  */
case class Container[F[_],ID,LO,SU,P,BS,FS,CR,MO](
  id:            ID,
  location:      F[LO],
  supplier:      F[SU],
  batch:         F[Plain],
  orderNr:       F[Plain],
  comment:       F[Plain],
  lentTo:        F[Plain],
  purity:        F[Percent],
  purityStr:     F[Plain],
  density:       F[Density],
  concentration: F[Concentration],
  amount:        F[Amount],
  empty:         F[Boolean],
  project:       F[P],
  bio:           BS,
  files:         FS,
  created:       CR,
  modified:      MO,
)

object Container extends DataCmp {
  /**
    * Path leading to a given container in the data tree.
    */
  type Path      = Id::Compound.Path

  /**
    * Path leading to a file linked to a container entry.
    */
  type FilPath   = File.Id::Path

  /**
    * Container entries as seen by the client.
    */
  type Cli       = Container[Pure,Id,Link[Location.Id],Link[Supplier.Id],Link[Project.AccId],List[BiodataEntry.Cli],List[File.Cli],TimeStamp,EditInfo]

  val lblG = LabelledGeneric[Cli]
  val lbls@(id::location::supplier::batch::orderNr::comment::lentTo::purity::purityStr::density::concentration::amount::empty::project::bio::files::created::modified::HNil) = Keys[lblG.Repr].apply

  def isLent(c: Cli): Boolean = c.lentTo.v.v.trim.nonEmpty

  implicit def eqI[F[_],ID,LO,SU,P,BS,FS,CR,MO]: cats.Eq[Container[F,ID,LO,SU,P,BS,FS,CR,MO]] = cats.Eq.fromUniversalEquals
  implicit def decI[F[_]:D1,ID:D,LO:D,SU:D,P:D,BS:D,FS:D,CR:D,MO:D]: D[Container[F,ID,LO,SU,P,BS,FS,CR,MO]] = deriveDecoder
  implicit def encI[F[_]:E1,ID:E,LO:E,SU:E,P:E,BS:E,FS:E,CR:E,MO:E]: E[Container[F,ID,LO,SU,P,BS,FS,CR,MO]] = deriveEncoder
}
