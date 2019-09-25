/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat
package example

import tagInstances._

/**
  * Sum type representing responses sent from the server
  * to the client.
  */
@io.circe.generic.JsonCodec sealed trait Result{ def r: Result = this }

case class Errors(errs: Nel[DataErr]) extends Result

case object PasswordChanged extends Result

case object BioDataValidated extends Result

case class BioStatsRes(res: EditRes[BioStats]) extends Result

case class MetRes(res: EditRes[Method.Cli]) extends Result

case class ProRes(res: EditRes[Project.Cli]) extends Result

case class StoRes(res: EditRes[Location.Cli]) extends Result

case class CpdRes(res: EditRes[Compound.Cli]) extends Result

case class SupRes(res: EditRes[Sup.Cli]) extends Result

case class UseRes(res: EditRes[Use.Cli]) extends Result

case class ExportRes(path: String) extends Result

case class LoggedIn(hash: String, user: Use.Cli, settings : USettings) extends Result

case class SettingsChanged(settings: USettings) extends Result

case object LoggedOut extends Result

object Result {
  implicit val eqI: cats.Eq[Result] = cats.Eq.fromUniversalEquals
}

// vim: set ts=2 sw=2 et:
