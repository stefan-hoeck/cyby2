/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat
package format

import cats.Eq, cats.implicits._
import io.circe.{Encoder,Decoder}

/**
  * Primitive RGB colors
  */
case class Color private(r: Int, g: Int, b: Int) {
  import Color.toHex
  override def toString = s"#${toHex(r)}${toHex(g)}${toHex(b)}"
}

object Color {
  def apply(r: Int, g: Int, b: Int): Option[Color] = {
    def isValid(n: Int) = 0 <= n && n <= 255

    if (isValid(r) && isValid(g) && isValid(b)) Some(new Color(r,g,b))
    else None
  }

  private val charMap: Map[Char,Int] = "0123456789abcdef".toList.zipWithIndex.toMap

  private val intMap: Map[Int,Char] = charMap.toList map (_.swap) toMap
  
  private[Color] def toHex(n: Int) = s"${intMap(n / 16)}${intMap(n % 16)}"
 
  def readByte(c1: Char, c2: Char): Option[Int] =
    (charMap get c1, charMap get c2).mapN(_ * 16 + _)

  def read(s: String): Option[Color] = s.toList match {
    case '#'::r1::r2::g1::g2::b1::b2::Nil ⇒
      (readByte(r1,r2),readByte(g1,g2),readByte(b1,b2)).mapN(new Color(_,_,_))
    case _                                ⇒ None
  }

  implicit lazy val eqI: Eq[Color] = Eq.fromUniversalEquals

  implicit lazy val readI: Read[Color] = Read.inst(s ⇒ s"Not a Color: $s")(read)

  implicit lazy val encodeI: Encoder[Color] = Encoder[String] contramap (_.toString)

  implicit lazy val decodeI: Decoder[Color] = readI.decoder

  def interpolate(f: Double)(c1: Color, c2: Color): Color = {
    def inter(a: Int, b: Int): Int =
      if (f <= 0D) a
      else if (f >= 1D) b
      else (a + f * (b - a)).toInt

      new Color(inter(c1.r, c2.r), inter(c1.g, c2.g), inter(c1.b, c2.b))
  }
}

// vim: set ts=2 sw=2 et:
