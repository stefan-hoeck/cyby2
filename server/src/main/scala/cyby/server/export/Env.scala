/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package export

import org.http4s.headers._
import org.http4s.Charset.`UTF-8`
import org.http4s.dsl.io._

import fs2.Stream.emits
import fs2.{io, text}

import java.nio.file.{Path ⇒ PathNIO}

/**
  * Utility functions for exporting data
  */
trait Env[E] extends ServerEnv {
  import ImplicitContextShift.cs

  val M: CyByMonadIO[E,Unit]

  /**
    * Given a filename, creates a path pointing to the file on disk.
    */
  protected def path(fn: String): PathNIO

  /**
    * Prepares a response for downloading the file at the given
    * location
    */
  def download(pth: String): M.Prog[Response] = M lift Ok(
    blocking.flatMap{b ⇒ io.file.readAll[IO](path(pth), b, 4096)},
    `Content-Type`(mediaType(pth), `UTF-8`)
  )

  /**
    * Writes a stream of lines to a file on disk.
    */
  def store(ss: IStream[String], fn: PathNIO): IO[Unit] =
    blocking.flatMap(b ⇒ ss.through(text.utf8Encode).through(io.file.writeAll(fn,b)))
      .compile.drain

  /**
    * Converts lists of strings (lines) into a
    * stream of lines.
    *
    * @param strs : creates lines of exported data from a data object
    * @param prefix : lines to be prepended to the block of exported data
    * @param postfix : lines to be appended the block of exported data
    */
  def strings[A](as: List[A])(
    strs: A ⇒ List[String],
    prefix: List[String] = Nil,
    postfix: List[String] = Nil,
  ): IStream[String] = {
    val all = emits(prefix) ++
              emits(as).flatMap(s ⇒ emits(strs(s))) ++
              emits(postfix)

    all.covary[IO]
  }

  val statsOdf: (String,Option[Double]) ⇒ String = {
    case (_,od) ⇒ OfficeValueType.fOpt(od)(OfficeValueType.fDouble)
  }

  /**
    * Creates txt header for fields paired with their name
    */
  def txtHeader[A](fields: List[(A,String)]): List[String] =
    textLines(List(fields map (_._2)))

  /**
    * Creates odf headers for fields paired with their name
    */
  def odfHeader[A](fields: List[(A,String)]): List[String] =
    OfficeValueType odfRow List(fields map { case (_,s) ⇒ OfficeValueType fString s })
}
