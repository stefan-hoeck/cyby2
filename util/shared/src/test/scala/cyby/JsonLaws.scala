/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby

import cats.instances.all._

import io.circe._

import org.typelevel.discipline.Laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

object JsonLaws {
  def apply[A:cats.Eq:Arbitrary:Encoder:Decoder]: JsonLaws[A] =
    new JsonLaws[A] {
      def Equ = cats.Eq[Decoder.Result[A]]
      def Arb = implicitly[Arbitrary[A]]
      def Enc = Encoder[A]
      def Dec = Decoder[A]
    }
}

trait JsonLaws[A] extends Laws {
  implicit def Equ: cats.Eq[Decoder.Result[A]]
  implicit def Arb: Arbitrary[A]
  implicit def Enc: Encoder[A]
  implicit def Dec: Decoder[A]

  def fromTo: DefaultRuleSet = new DefaultRuleSet(
    "Json",
    None,
    "to and from" → forAll { a: A ⇒
      val json: Json = Enc(a)
      val res: Decoder.Result[A] = Dec decodeJson json
      val exp: Decoder.Result[A] = Right(a)

      Equ.eqv(res, exp) :| s"Expected $exp, but was $res"
    }
  ){}

  def fromToStripped: DefaultRuleSet = new DefaultRuleSet(
    "Json stripped",
    None,
    "to and from" → forAll { a: A ⇒
      val json: Json = stripEnc(a)
      val res: Decoder.Result[A] = Dec decodeJson json
      val exp: Decoder.Result[A] = Right(a)

      Equ.eqv(res, exp) :| s"Expected $exp, but was $res"
    }
  ){}
}

// vim: set ts=2 sw=2 et:
