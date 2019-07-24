/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui

import io.circe.{Json, Decoder ⇒ CirceDecoder, parser}

/**
  * Some utility functions
  */
trait sharedUtil {
  val ioUnit: IO[Unit] = msf ioNow unit

  // --------------------------------------------------------
  //             *** Parsing and decoding JSON ***
  // --------------------------------------------------------

  def parseJson(s: String): ValNel[Err,Json] =
    unthrow(parser parse s)

  /**
    * Tries to decode a Json object into a given data type
    */
  def decode[A:CirceDecoder](json: Json): ValNel[Err,A] =
    unthrow(CirceDecoder[A] decodeJson json)

  def parseAndDecode[A:CirceDecoder](s: String): ValNel[Err,A] =
    parseJson(s) andThen decode[A]

  /**
    * Wraps an Either possibly containing a caught Throwable
    * from impure code.
    */
  def unthrow[E<:Throwable,A](ei: Either[E,A]): ValNel[Err,A] =
    ei.fold(e ⇒ fail(Serious(e).e), valid)
}

// vim: set ts=2 sw=2 et:

