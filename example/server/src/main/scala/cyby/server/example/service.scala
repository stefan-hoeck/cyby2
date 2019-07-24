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
  implicit def mutM = CyByMonadIO.authEnv[St]

  val make: IO[HttpService] = for {
    st      <- loader.loadSt
    mutPair <- statefulV(mutate.prog)(st)
    autPair <- statefulV(auth.prog)(Map.empty)
    (sig, mut) = mutPair
    (_,   aut) = autPair
  } yield http(sig, M.chain(aut)(cyby(mut)))

  private def cyby(mut: SLProg[LoggedInEnv,Z.Result]): SLProg[LoggedInEnv,Response] =
    FM.ask.map(_.env.req)
      .flatMap(r ⇒ FM.info(s"${r.method} : ${r.uri.path}") as r)
      .flatMap {
        case GET  -> Root / "login"           ⇒ FM toResponse FM.ask.map(_.loginRes)
        case GET  -> Root / "logout"          ⇒ FM toResponse FM.pure(Z.LoggedOut)
        case _    -> Root / "query"  /  _     ⇒ FM toResponse query.prog
        case POST -> Root / "mutate" /  _     ⇒ FM toResponse mut
        case GET  -> Root / DT(FilT) / P(p) /fn  ⇒ files.prog(p)
        case POST -> Root / "export"          ⇒ FM toResponse export.prog
        case GET  -> Root / "export" / pth    ⇒ export.download(pth)
        case r                                ⇒ FM toResponse FM.raise(Z.NotFound(r.uri.path))
      }

  private def http(
    sig: Ref[St],
    prog: SLProg[Env[St],Response]
  ): HttpService = Kleisli{ env(sig)(_) >>= M.responseO(prog) }

  private def env(sig: Ref[St])(r: Request): OptionT[IO,Env[St]] =
    Env.opt(r,sig) map modL(lens[Env[St]].logger)(_ filtered Info)
}

// vim: set ts=2 sw=2 et:
