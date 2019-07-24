/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

import cats.instances.all._

import io.circe.{Encoder,Decoder,Json}

/**
  * Wrapper for plaintext passwords. The server modules has a similar
  * wrapper for hashed passwords. No strong password policy is enforced by
  * this type.
  */
final class Password private(val v: String) extends AnyVal {
  override def toString = v
}

object Password extends Refined[String,Password]("Password", _.v){
  val MinLength: Int = 5
  val MaxLength: Int = 1000

  def apply(s: String): Option[Password] =
    Plain(s) flatMap { p ⇒
      if (p.v.length >= MinLength && p.v.length <= MaxLength)
        Some(new Password(p.v)) else None
    }

  override implicit lazy val encodeI: Encoder[Password] =
    enc.mapJson(j ⇒ Json.obj(Unhashed -> j))

  override implicit lazy val decodeI: Decoder[Password] =
    dec.prepare(_ downField Unhashed)
}

// vim: set ts=2 sw=2 et:
