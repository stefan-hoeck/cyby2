/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server

import cats.implicits.{none ⇒ _, _}

import org.mindrot.jbcrypt.BCrypt

import cats.Eq

import io.circe.{Encoder, Decoder, HCursor}

/**
  * Wrapper class for hashed passwords.
  *
  * Passwords are hashed with salt using algorithms
  * provided by org.mindrot.jbcrypt
  */
case class PWHash private (v: String) extends AnyVal {
  override def toString = v
}

object PWHash {
  implicit val eqI: Eq[PWHash] = Eq.fromUniversalEquals

  implicit lazy val readI: Read[PWHash] =
    Read.inst(s ⇒ s"Not a PWHash: $s"){s ⇒ some(PWHash(s)) }

  val Unset: PWHash = PWHash("")

  def hash(pw: String, salt: String): PWHash = PWHash(BCrypt.hashpw(pw, salt))

  def hash(pw: String): PWHash = hash(pw, BCrypt.gensalt())

  def check(pw: String, pwh: PWHash): Boolean =
    tryO(BCrypt.checkpw(pw, pwh.v)).getOrElse(false)

  implicit val encI: Encoder[PWHash] = Encoder[String] contramap (_.v)

  implicit val decI: Decoder[PWHash] = new Decoder[PWHash]{
    def apply(h: HCursor) =
      h.get[String](cyby.dat.Unhashed).map(hash) <+>
      h.as[String].map(PWHash.apply)
  }
}

// vim: set ts=2 sw=2 et:
