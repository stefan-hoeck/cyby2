/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat
package example

import cats.Eq, cats.implicits._
import io.circe.{Encoder,Decoder}

final class HasAccess[A] private (val v: A) extends AnyVal

object HasAccess {
  private[this] def apply[A](a: A): HasAccess[A] = new HasAccess(a)

  /**
    * This method is to be used in client code only as it
    * circumvents access verification.
    */
  def unsafe[A](a: A): HasAccess[A] = apply(a)

  implicit def eqI[A:Eq]: Eq[HasAccess[A]] = Eq.by(_.v)

  implicit def encodeI[A:Encoder]: Encoder[HasAccess[A]] =
    Encoder[A] contramap (_.v)

  implicit def decodeI[A:Decoder]: Decoder[HasAccess[A]] =
    Decoder[A] map apply

  implicit def readI[A:Read]: Read[HasAccess[A]] =
    Read[A] map apply

  /**
    * Environment used for authentication and authorization
    * of users
    */
  case class AuthEnv(
    id:        User.Id,
    lvl:       UserLevel,
    canAccess: Set[Project.Id],
  ){
    def accAll(ps: List[Project.Id]): Boolean = ps forall canAccess

    def authAdd(ps: List[Project.Id]): List[DataErr] =
      must(isUser(lvl) && accAll(ps))(Unauthorized)

    def authMod(
      ids: List[Project.Id],
      o: Option[Project.Id]
    ): List[DataErr] = o match {
      case Some(n) ⇒ must(isSuperUser(lvl) && canAccess(n) && accAll(ids))(Unauthorized)
      case _       ⇒ must(isUser(lvl) && accAll(ids))(Unauthorized)
    }

    val project: Pure[Project.Id] ⇒ Option[Pure[Project.AccId]] =
      p ⇒ if (canAccess(p.v)) Some(Pure(apply(p.v))) else None

    def user(u: User.Id): Option[User.AccId] =
      if (id === u || isSuperUser(lvl)) Some(apply(u)) else None
  }
}
