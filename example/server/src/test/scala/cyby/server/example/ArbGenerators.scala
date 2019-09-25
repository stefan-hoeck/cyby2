/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

import cats.implicits._
import cyby.dat.example.{Use,USettings,Met,Pro}
import org.scalacheck.{Gen,Arbitrary}

import org.scalacheck.Shapeless._
import org.scalacheck.derive.MkArbitrary

trait ArbGenerators extends cyby.server.Generators {
  val U = UseS

  implicit lazy val useSrvA = MkArbitrary[U.Srv].arbitrary
  implicit lazy val useAddA = MkArbitrary[U.Add].arbitrary
  implicit lazy val useModA = MkArbitrary[U.Mod].arbitrary
  implicit lazy val useSrvModA = MkArbitrary[U.SrvMod].arbitrary

  implicit lazy val filSrvA = MkArbitrary[SubFilS.Srv].arbitrary
  implicit lazy val bioSrvA = MkArbitrary[BiodataEntryS.Srv].arbitrary
  implicit lazy val conSrvA = MkArbitrary[ContainerS.Srv].arbitrary
  implicit lazy val subSrvA = MkArbitrary[SubS.Srv].arbitrary
  lazy val proSrvA = MkArbitrary[ProS.Srv].arbitrary

  implicit lazy val uSetA: Arbitrary[Map[Use.Id,USettings]] = Arbitrary(Gen const Map())
  implicit lazy val stA     = MkArbitrary[St].arbitrary
  implicit lazy val authEnvA = MkArbitrary[AuthEnv].arbitrary

  implicit lazy val metPA   = Arbitrary((arb[Met.Id],arb[Pro.Path]).mapN(_ :: _))
  implicit lazy val proPA   = Arbitrary(arb[Pro.Id] map (_ :: hnil))
}

// vim: set ts=2 sw=2 et:
