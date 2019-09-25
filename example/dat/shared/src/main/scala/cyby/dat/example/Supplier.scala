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
  * Data type containing information about registered suppliers.
  *
  * The type parameters allow us to abstract over different types of
  * behavior: The higher order type parameter F is used to decide
  * whether fields are required or optional. This is used for editing
  * data entries: When adding new entries, all fields are mandatory, so
  * F is set to Pure. When modifying existing entries, fields are
  * optional, so F is set to Maybe (see the Maybe type why we don't use
  * Option).
  *
  * First, look at the description of the type parameters below, and
  * then have a look at the specified type aliases in Sup's companion
  * object and at cyby.server.example.SupplierS.
  *
  * @tparam F:  Effect, in which fields are wrapped. Typically set
  *             to Pure for mandatory fields and Maybe if fields
  *             are optional.
  * @tparam ID: ID of the data object
  * @tparam CR: Information about when the object was created
  * @tparam MO: Information about the last modification
  */
case class Supplier[F[_],ID,CR,MO](
  id:           ID,
  name:         F[Name],
  address:      F[Plain],
  created:      CR,
  modified:     MO,
)

/**
  * In the companion object we implement important typeclasses and
  * provide often used type aliases. Aliases {{{Supplier.Id}}} and
  * {{{Supplier.AccId}}} are specified in trait {{{cyby.dat.DataCmp}}}.
  */
object Supplier extends DataCmp {
  /**
    * Path leading to a given instance of Sup in the data tree.
    */
  type Path      = Id::HNil

  /**
    * Suppliers as seen by the client.
    */
  type Cli       = Supplier[Pure,Id,TimeStamp,EditInfo]

  val lblG = LabelledGeneric[Cli]

  val lbls@(id::name::address::created::modified::HNil) = Keys[lblG.Repr].apply

  implicit def eqI[F[_],ID,CR,MO]: cats.Eq[Supplier[F,ID,CR,MO]] = cats.Eq.fromUniversalEquals
  implicit def decI[F[_]:D1,ID:D,CR:D,MO:D]: D[Supplier[F,ID,CR,MO]] = deriveDecoder
  implicit def encI[F[_]:E1,ID:E,CR:E,MO:E]: E[Supplier[F,ID,CR,MO]] = deriveEncoder
}

