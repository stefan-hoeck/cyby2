/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server

import cats.Monad, cats.implicits._

import org.http4s.util.CaseInsensitiveString
import org.http4s.dsl.io._

import fs2.io.file.readAll

import java.nio.file.Paths

/**
  * Helper trait to authenticate registered users
  *
  * The API of this module comes with two entry points:
  * User logging in for the first time provide their
  * credentials in clear text (hopefully over an encrypted
  * connection :-) ). If authentication is successfull,
  * a random hash is generated and returned to the callers.
  * This hash is also stored together with some user settings
  * in a hash map. It expires after a predefined timespan
  * (see cyby.server.Env.timeout). Until then, users can connect
  * by just passing this hash as a query param in their URL.
  * The validity of the hash and the user's identity are then
  * checked using the hash map.
  *
  * @tparam Auth : information returned after successfull authentication
  *                (see function mkAuth)
  * @tparam St   : (in memory) state needed to authenticate a user
  */
trait auth[Auth] extends ServerEnv {
  import ImplicitContextShift.cs

  type State = Map[String,Entry]

  private object UName extends QueryParamDecoderMatcher[String]("user")

  private object Hash extends QueryParamDecoderMatcher[String]("hash")

  private val PW = CaseInsensitiveString("password")

  val M: CyByMonadIO[Env[St],State]

  /**
    * error to be returned to the client when authentication failed
    */
  val authFailed: Err

  /**
    * error to be returned to the client when a user is not logged in
    */
  val notLoggedIn: Err

  /**
    * error to be returned if a reqest links to an unknown URL
    */
  def notFound(r: Request): Err

  /**
    * creates an Auth from the given parameters
    */
  def mkAuth(env: Env[St], u: UseS, hash: String): Auth

  /**
    * tries to authenticate the user
    */
  def doAuth(st: St, name: String, pw: String): ErrNel[Err,UseS]

  /**
    * lookup a user by its ID
    */
  def find(st: St, id: UseId): ErrNel[Err,UseS]

  /**
    * program representing the login facility: users
    * login at /login?user=[username] giving their password
    * in clear text in the header. logout happens
    * at /logout?user=[username],hash=[userhash]
    *
    * calls to other URL typically must include
    * the user and hash query parameters in order for users
    * to get properly authenticated.
    */
  lazy val prog: Request ⇒ M.Prog[Auth] = {
    case r@(GET -> Root / "login" :? UName(name))  ⇒
      login(name, r.headers.get(PW).fold("")(_.value))

    case r@(GET -> Root / "logout" :? UName(n) +& Hash(h))  ⇒ logout(n, h)

    case r@(_ :? UName(n) +& Hash(h)) ⇒ auth(n, h)

    case r                            ⇒ M.raiseErrors(Nel of notFound(r))
  }


  def login(name: String, pw: String): M.Prog[Auth] = for {
    _    <- M debug s"$name about to log in"
    env  <- M.ask
    st   <- M.get
    u    <- M wrapEither doAuth(env.st, name, pw)
    hash <- M lift mkUserHash(st, u)
    _    <- Entry(hash, useIdS(u), env.expiresAt) setAt name
    _    <- M info s"$name logged in"
  } yield mkAuth(env, u, hash)

  private def auth(name: String, hash: String): M.Prog[Auth] =
    ifLoggedIn(name, hash){ (c, e) ⇒ e.prolong(c).setAt(name) }

  private def logout(name: String, hash: String): M.Prog[Auth] =
    ifLoggedIn(name, hash){ (c,e) ⇒ M.modify(_ - name) }

  private def ifLoggedIn(name: String, hash: String)
    (f: (Env[St], Entry) ⇒ M.Prog[Unit]): M.Prog[Auth] = for {
    _    <- M debug s"Authenticating $name"
    env  <- M.ask
    st   <- M.get
    res  <- st get name match {
              case Some(e) if e.loggedIn(hash, env) ⇒ for {
                u <- M wrapEither find(env.st, e.id)
                _ <- f(env, e)
              } yield mkAuth(env, u, e.hash)
              case Some(_) ⇒ M.raiseErrors[Auth](Nel of authFailed)
              case None    ⇒ M.raiseErrors[Auth](Nel of notLoggedIn)
            }
    _    <- M.info(s"$name authenticated")
  } yield res

  private def mkUserHash(st: State, u: UseS): IO[String] =
    st.toList.collect{ case (_,Entry(h,id,_)) if useIdS(u) === id ⇒ h }
             .headOption
             .fold(doMkUserHash)(Monad[IO].pure)

  private lazy val doMkUserHash: IO[String] = blocking.flatMap { b ⇒ 
    readAll[IO](Paths get "/dev/urandom", b, 32)
      .take(32)
  }.compile.toVector.map(_ map (d ⇒ (d & 0xFFFF).toHexString) mkString)


  /**
    * Authentication entry.
    *
    * @param hash    : hash sent to re-authenticate without the need
    *                  of sending the (clear text) password again
    * @param id      : ID of the authenticated user
    * @param expires : time when the user session expires and the user
    *                  has to be reauthenticated
    */
  case class Entry(hash: String, id: UseId, expires: Timestamp) {
    def loggedIn(h: String, c: Env[St]): Boolean =
      (h === hash) && expires >= c.timestamp

    def prolong(s: Env[St]): Entry = Entry(hash, id, s.expiresAt)

    def setAt(name: String): M.Prog[Unit] = M.modify(_ + (name -> this))
  }
}

// vim: set ts=2 sw=2 et:
