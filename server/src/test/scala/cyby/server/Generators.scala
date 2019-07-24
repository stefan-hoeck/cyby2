/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server

import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.derive.MkArbitrary

trait Generators extends cyby.dat.DataImplicits with cyby.chem.Generators {
  lazy val pwHashGen: Gen[PWHash] = Gen.oneOf(
    PWHash hash "kj23230",
    PWHash hash "__!!??asdf",
    PWHash hash "AJD_12JJ!$p",
  )

  implicit lazy val pwHashArb: Arbitrary[PWHash] = Arbitrary(pwHashGen)

  implicit lazy val srvArb = MkArbitrary[TestData.Srv].arbitrary
  implicit lazy val modArb = MkArbitrary[TestData.SrvMod].arbitrary
}

// vim: set ts=2 sw=2 et:
