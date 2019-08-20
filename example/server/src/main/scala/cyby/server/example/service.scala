/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

import cats.data.{OptionT, Kleisli}, cats.implicits._

import cyby.dat.{example ⇒ Z}, Z.{FilT, DataType ⇒ DT, Path ⇒ P}

import org.http4s.dsl.io._

/**
  * Component providing the complete HTTP API of CyBy.
  */
case class Service(coreSettings: CoreSettings) extends CyByZ {
  import ImplicitContextShift.cs

  val auth   = Auth(coreSettings)
  val loader = Loader(coreSettings)
  val export = Export(coreSettings)
  val files  = Files(coreSettings)
  val query  = Query(coreSettings)
  val mutate = Mutate(coreSettings)

  val M = CyByMonadIO.env[Unit]
  val FM = CyByMonadIO.authEnv[Unit]

  implicit def authM = CyByMonadIO.env[auth.State]
  implicit def mutM = CyByMonadIO.editEnv[St]

  val make: IO[HttpService] = for {
    st      <- loader.loadSt
    mutPair <- statefulV(mutate.prog)(st)
    autPair <- statefulV(auth.prog)(Map.empty)
    (sig, mut) = mutPair
    (_,   aut) = autPair
  } yield http(sig, r ⇒ M.chain(aut(r))(cyby(mut)(r)))

  private def cyby(mut: Request ⇒ SLProg[EditEnv,Z.Result])
    : Request ⇒ SLProg[LoggedInEnv,Response] = r ⇒
    FM.info(s"${r.method} : ${r.uri.path}").as(r).flatMap {
      case GET  -> Root / "login"           ⇒ FM toResponse FM.ask.map(_.loginRes)
      case GET  -> Root / "logout"          ⇒ FM toResponse FM.pure(Z.LoggedOut)
      case _    -> Root / "query"  /  _     ⇒ FM toResponse query.prog(r)
      case POST -> Root / "mutate" /  _     ⇒ FM toResponse mut(r).cmapEnv(_.editEnv)
      case GET  -> Root / DT(FilT) / P(p) /fn  ⇒ files.prog(p)
      case POST -> Root / "export"          ⇒ FM toResponse export.prog(r)
      case GET  -> Root / "export" / pth    ⇒ export.download(pth)
      case r                                ⇒ FM toResponse FM.raise(Z.NotFound(r.uri.path))
    }

  private def http(
    sig: Ref[St],
    prog: Request ⇒ SLProg[Env[St],Response]
  ): HttpService = Kleisli{ r ⇒ env(sig) >>= M.responseO(prog(r)) }

  private def env(sig: Ref[St]): OptionT[IO,Env[St]] =
    Env.opt(sig) map modL(lens[Env[St]].logger)(_ filtered Info)
}

// vim: set ts=2 sw=2 et:
