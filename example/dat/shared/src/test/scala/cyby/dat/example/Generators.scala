/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat
package example

import org.scalacheck.Shapeless._
import org.scalacheck.{Arbitrary,derive}, derive.{MkArbitrary ⇒ MKA}

trait Generators extends DataImplicits {
  implicit lazy val filArb = MKA[File.Cli].arbitrary
  implicit lazy val metArb = MKA[Method.Cli].arbitrary
  implicit lazy val proArb = MKA[Project.Cli].arbitrary
  implicit lazy val stoArb = MKA[Location.Cli].arbitrary
  implicit lazy val supArb = MKA[Supplier.Cli].arbitrary
  implicit lazy val bioArb = MKA[BiodataEntry.Cli].arbitrary
  implicit lazy val conArb = MKA[Container.Cli].arbitrary
  implicit lazy val subArb = MKA[Compound.Cli].arbitrary
  implicit lazy val useArb = MKA[User.Cli].arbitrary
  implicit lazy val filFieldArb = shapeless.the[Arbitrary[FilField]]
  implicit lazy val subFieldArb = shapeless.the[Arbitrary[CpdField]]
  implicit lazy val conFieldArb = shapeless.the[Arbitrary[ConField]]
  implicit lazy val bioFieldArb = shapeless.the[Arbitrary[BioField]]
  implicit lazy val expFieldArb = shapeless.the[Arbitrary[ExportField]]

  implicit def hasAccessArb[A](implicit A: Arbitrary[A]): Arbitrary[HasAccess[A]] =
    Arbitrary(arb[A] map HasAccess.unsafe)
}

// vim: set ts=2 sw=2 et:
