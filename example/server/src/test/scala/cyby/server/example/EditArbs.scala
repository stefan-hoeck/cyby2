/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

import cats.implicits._

import cyby.dat.{Add,Mod,Del}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.derive.MkArbitrary

trait EditArbs extends ArbGenerators {
  val S: ExampleEditor

  val imps: Implicits

  class Implicits(
    implicit
    AA: MkArbitrary[S.Add],
    MA: MkArbitrary[S.Mod],
    SAA: MkArbitrary[S.SrvAdd],
    SMA: MkArbitrary[S.SrvMod],
    IA:  MkArbitrary[S.Id],
  ) {
    implicit val addArb: Arbitrary[S.Add]       = AA.arbitrary
    implicit val modArb: Arbitrary[S.Mod]       = MA.arbitrary
    implicit val srvAddArb: Arbitrary[S.SrvAdd] = SAA.arbitrary
    implicit val srvModArb: Arbitrary[S.SrvMod] = SMA.arbitrary
    implicit val idArb: Arbitrary[S.Id]         = IA.arbitrary

    implicit val loadArb: Arbitrary[S.LoadEd] = {
      import tagArbInstances.tagArb

      def idG:  Gen[S.Id @@ IsValid] = tagArb(idArb).arbitrary
      def addG: Gen[S.SrvAdd @@ IsValid] = tagArb(srvAddArb).arbitrary
      def modG: Gen[S.SrvMod @@ IsValid] = tagArb(srvModArb).arbitrary

      val editGen: Gen[S.LoadEd] = {
        val del: Gen[S.LoadEd] = idG map Del.apply
        val add: Gen[S.LoadEd] = addG map Add.apply
        val mod: Gen[S.LoadEd] = (idG,modG).mapN(Mod.apply)

        Gen.frequency((4 -> add), (4 -> mod), (2 -> del))
      }

      Arbitrary(editGen)
    }
  }
}
