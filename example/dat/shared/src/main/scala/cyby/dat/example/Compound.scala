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
  * cyby.server.example.CompoundS for aliases with often used type parameters.
  *
  * @tparam F:  Effect, in which fields are wrapped. Typically set
  *             to Pure for mandatory fields and Maybe if fields
  *             are optional.
  * @tparam ID: ID of the data object
  * @tparam SU: Type of structure field
  * @tparam P:  Type of project field
  * @tparam CS: Type of containers field
  * @tparam FS: Type of files field
  * @tparam CR: Information about when the object was created
  * @tparam MO: Information about the last modification
  */
case class Compound[F[_],ID,SU,P,CS,FS,CR,MO](
  id:                     ID,
  name:                   F[Plain],
  structure:              F[Maybe[SU]],
  abs:                    F[Boolean],
  casNr:                  F[CasNr],
  project:                F[P],
  containers:             CS,
  files:                  FS,
  created:                CR,
  modified:               MO,
)

object Compound extends DataCmp {
  /**
    * Path leading to a given substance in the data tree.
    */
  type Path      = Id::HNil

  /**
    * Path leading to a file linked to a substance entry.
    */
  type FilPath   = Fil.Id::Path

  /**
    * Compounds as seen by the client.
    */
  type Cli       = Compound[Pure,Id,Mol,Link[Project.AccId],List[Container.Cli],List[Fil.Cli],TimeStamp,EditInfo]

  /**
    * Collects a list of statistics entries from a
    * list of compounds.
    */
  def toStats(db: List[Cli]): List[BioStats] = for {
    sub <- db
    con <- sub.containers
    if (con.bio.nonEmpty)
  } yield BioStats(
    sub.copy(files = Nil, containers = Nil),
    con.copy(files = Nil)
  )

  val lblG = LabelledGeneric[Cli]
  val lbls@(id::name::structure::abs::casNr::project::containers::files::created::modified::HNil) = Keys[lblG.Repr].apply
  implicit lazy val toMolI: ToMol[Cli] = ToMol(_.id.v, _.structure.v.o)

  implicit def eqI[F[_],ID,SU,P,CS,FS,CR,MO]: cats.Eq[Compound[F,ID,SU,P,CS,FS,CR,MO]] = cats.Eq.fromUniversalEquals
  implicit def decI[F[_]:D1,ID:D,SU:D,P:D,CS:D,FS:D,CR:D,MO:D]: D[Compound[F,ID,SU,P,CS,FS,CR,MO]] = deriveDecoder
  implicit def encI[F[_]:E1,ID:E,SU:E,P:E,CS:E,FS:E,CR:E,MO:E]: E[Compound[F,ID,SU,P,CS,FS,CR,MO]] = deriveEncoder
}
