/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat
package example

import io.circe.shapes._, io.circe.generic.JsonCodec
import cyby.dat.{Plain, CasNr, Mol, FileName, Name}

/**
  * Data type representing errors occuring at the server.
  *
  * If such an error occurs, it is sent as a response to the client
  * and displayed there (properly localized).
  *
  * Using a sum type to represent errors allows us to easily store
  * additional information about these errors while giving us the
  * power and convenience of (exhaustive) pattern matching.
  */
@JsonCodec sealed trait DataErr{
  def e: DataErr = this

  def nel: Nel[DataErr] = Nel of e
}

case class BatchExists(b: Plain, p: Container.Path) extends DataErr
case class CasNrExists(c: CasNr, p: Sub.Path) extends DataErr
case class Exists(n: Name, p: Path) extends DataErr
case class SubExists(n: Plain, p: Sub.Path) extends DataErr
case class FilExists(f: FileName, p: Path) extends DataErr
case class NotFound(url: String) extends DataErr
case class PathNotFound(p: Path) extends DataErr
case class NameNotFound(n: Name) extends DataErr
case class BatchNotFound(b: Plain) extends DataErr
case class StillLinked(p: Path) extends DataErr
case class ReadErr(s: String) extends DataErr
case class QueryErr(f: ExportField, s: String) extends DataErr
case class Serious(msg: String) extends DataErr
case class StructureExists(m: Mol, p: Sub.Path) extends DataErr
case class Unset(s: String) extends DataErr
case object AuthenticationFailed extends DataErr
case object CantChangeAlias extends DataErr
case object CantChangeLevel extends DataErr
case object CantDeleteSelf extends DataErr
case object EmptyStructure extends DataErr
case object InvalidCreds extends DataErr
case object NotLoggedIn extends DataErr
case object Unauthorized extends DataErr


object DataErr {
  implicit val eqI: cats.Eq[DataErr] = cats.Eq.fromUniversalEquals

  val queryErr: cyby.query.QueryErr[ExportField] ⇒ DataErr = {
    case cyby.query.QueryErr(f, str) ⇒ QueryErr(f, str)
  }
}

// vim: set ts=2 sw=2 et:
