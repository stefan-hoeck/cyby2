/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby

import cats.implicits._
import org.scalacheck.Arbitrary

class MaybeTest extends CyBySuite {
  checkLaws("Maybe eqv", EqLaws[Maybe[Int]].eqv)
  checkLaws("Maybe json", JsonLaws[Maybe[Int]].fromTo)
  checkLaws("Maybe json stripped", JsonLaws[Maybe[Int]].fromToStripped)

  implicit lazy val mtArb: Arbitrary[MTest] =
    Arbitrary((arb[Maybe[List[String]]],arb[Option[Maybe[Long]]]).mapN(MTest.apply))

  checkLaws("Option[Maybe] json", JsonLaws[MTest].fromTo)
  checkLaws("Option[Maybe] json stripped", JsonLaws[MTest].fromToStripped)
}

@io.circe.generic.JsonCodec
case class MTest(may: Maybe[List[String]], omay: Option[Maybe[Long]])

object MTest {
  implicit val eqI: cats.Eq[MTest] = cats.Eq.fromUniversalEquals
}

