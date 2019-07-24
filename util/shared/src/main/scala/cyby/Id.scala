/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby

import cats.{Order, Show}, cats.instances.all._

import io.circe.{Encoder,Decoder,KeyEncoder,KeyDecoder}

/**
  * An Identification number for a data object.
  *
  * The type parameter A is a "phantom type": It can be
  * used for reasons of type safety as well as for
  * type class selection.
  */
final class Id[A](val v: Long) extends AnyVal {
  def to[B]: Id[B] = Id(v)

  def inc: Id[A] = Id(v + 1)

  def dec: Id[A] = Id(v - 1)

  override def toString = v.toString
}

object Id {
  def apply[A](id: Long): Id[A] = new Id[A](id)

  def unapply[A](s: String): Option[Id[A]] = readI[A] read s

  implicit def orderI[A]: Order[Id[A]] = Order by (_.v)

  implicit def readI[A]: Read[Id[A]] = Read[Long] map Id.apply

  implicit def showI[A]: Show[Id[A]] = Show.fromToString

  implicit def encodeI[A]: Encoder[Id[A]] = Encoder[Long] contramap (_.v)

  implicit def keyDecodeI[A]: KeyDecoder[Id[A]] = KeyDecoder[Long] map apply

  implicit def keyEncodeI[A]: KeyEncoder[Id[A]] = KeyEncoder[Long] contramap (_.v)

  implicit def decodeI[A]: Decoder[Id[A]] = Decoder[Long] map apply
}

// vim: set ts=2 sw=2 et:
