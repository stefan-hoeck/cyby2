/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server

import cats._, cats.implicits.{none ⇒ _, _}
import cats.data.{OptionT}
import cats.effect.concurrent.MVar

import org.http4s.dsl.io._
import org.http4s.circe._
import org.http4s.EntityDecoder
import org.http4s.multipart.Multipart

import io.circe.{ Json, Decoder }
import io.circe.syntax._

import doobie._
import doobie.implicits._


import io.circe.{Json,Decoder,parser}
import io.circe.syntax._

trait ServerEnv extends cyby.dat.CyByEnv {
  val coreSettings: CoreSettings

  object ImplicitContextShift {
    implicit def cs: cats.effect.ContextShift[IO] = coreSettings.contextShift
  }

  def dataPath = coreSettings.dataPath

  def transactor = coreSettings.transactor

  type UseIdType

  type UseId = cyby.Id[UseIdType]

  def useId(l: Long): UseId = cyby.Id(l)


  type UseS

  def useIdS(u: UseS): UseId


  type Err

  type Errs = Nel[Err]

  def errsToRes(es: Errs): Result

  def throwableToErr(t: Throwable): Err

  type DataE[A] = ErrNel[Err,A]

  type DataV[A] = ValNel[Err,A]

  def logErr(e: Err): Log


  /**
    * Actual server state
    */
  type St

  /**
    * Tries to parse the given String as Json
    */
  def parseJson(s: String): DataV[Json] =
    handleThrowable(parser parse s)

  /**
    * Tries to decode a Json object into a given data type
    */
  def decode[A:Decoder](json: Json): DataV[A] =
    handleThrowable(Decoder[A] decodeJson json)

  def parseAndDecode[A:Decoder](s: String): DataV[A] =
    parseJson(s) andThen decode[A]

  def parseAndDecodeE[A:Decoder](s: String): DataE[A] =
    parseAndDecode[A](s).toEither
 
  /**
    * Wraps an Either possibly containing a caught Throwable
    * from impure code.
    */
  def handleThrowable[E<:Throwable,A](ei: Either[E,A]): DataV[A] =
    ei.fold(t ⇒ fail(throwableToErr(t)), valid)



  //-------------------------------------------------------------------
  //                        User Settings
  //-------------------------------------------------------------------

  type SettingsMap = Map[UseId,USettings]


  // @TODO: Use proper localization when logging errors
  def settings(log: Logger[IO]): IO[SettingsMap] = {
    import implicits._

    def succ(i: Long) =
      log.log(s"Settings for user $i loaded successfully", Debug)

    def fail(i: Long)(e: Err) =
      log.log(s"Error when loading settings for user ${i}: ${e}", Warning)

    def decode(p: (Long,String)) = p match {
      case (i,s) ⇒ parseAndDecode[USettings](s).toEither match {
        case Right(us) ⇒ succ(i) as some(useId(i) -> us)
        case Left(nel) ⇒ nel traverse_ fail(i) as none
      }
    }

    def toMap(ps: List[(Long,String)]) =
      ps.traverse(decode).map(_.collect{ case Some(p) ⇒ p}.toMap)

    sql"""select id, json from settings"""
      .query[(Long,String)]
      .to[List]
      .transact(transactor)
      .flatMap(toMap)
  }


  def updateSettings(uid: UseId, ss: USettings): ConnectionIO[Unit] = {
    import implicits._

    val idQ    = sql"""select id from settings where id = ${uid.v}"""

    for {
      o      <- idQ.query[Long].option

      str    = ss.asJson.noSpaces
      insert = sql"insert into settings (id, json) values (${uid.v}, $str)"
      update = sql"update settings set json = $str where id = ${uid.v}"
      fr     = o.fold(insert)(_ ⇒ update)

      _      <- fr.update.run
    } yield ()
  }


  //------------------------------------------------------------------------
  //      Effectful Computations
  //------------------------------------------------------------------------
  
  type Logs = List[Log]

  type CyByProg[F[_],Conf,S,A] = ProgT[F,Conf,Log,S,Err,A]

  abstract class CyByMonad[F[_]:Monad,Conf,S] extends ProgHelper[F,Conf,S,Log,Err]{
    def log(msg: ⇒ String, lvl: LogLevel): Prog[Unit] = doLog(Log(lvl,msg))
  
    def debug(msg: ⇒ String): Prog[Unit] = log(msg, Debug)
  
    def info(msg: ⇒ String): Prog[Unit] = log(msg, Info)
  
    def warn(msg: ⇒ String): Prog[Unit] = log(msg, Warning)
  
    def error(msg: ⇒ String): Prog[Unit] = log(msg, Error)
  }
  

  trait CyByMonadIO[C,S] extends CyByMonad[IO,C,S] {
    import implicits.{resEncI}

    def runLogged[A](p: Prog[A]): C ⇒ S ⇒ IO[Either[Errs,(S,A)]] = c ⇒ s ⇒ {
      for {
        res1 <- p.run(c,Nil,s)
        res2 <- res1.fold[IO[Either[Errs,(S,A)]]](
                  { case (e,l)   ⇒ handleLogs(c)(l).as(Left(e)) },
                  { case (l,s,a) ⇒ handleLogs(c)(l).as(Right(s -> a)) }
                )
      } yield res2
    }
  
    // *** Running programs *** //

    def handleErr(c: C): Err ⇒ IO[Unit]

    def handleErrs(c: C): Errs ⇒ IO[Unit] = _ traverse_ handleErr(c)

    def handleLog(c: C): Log ⇒ IO[Unit]

    def handleLogs(c: C): Logs ⇒ IO[Unit] = _.reverse traverse_ handleLog(c)

    def run_(s: S, c: C)(p: Prog[Unit]): IO[Unit] =
      runT(s, c, handleErrs(c))(p) map (_._2)
  
    def run[A](s: S, c: C, toA: Errs ⇒ A)(p: Prog[A]): IO[(S,A)] =
      runT(s, c, err ⇒ Monad[IO] pure toA(err))(p)

    def runT[A](s: S, c: C, toA: Errs ⇒ IO[A])(p: Prog[A]): IO[(S,A)] = {
      val onErr: Errs ⇒ IO[(S,A)] = e ⇒ handleErrs(c)(e) *> toA(e).map(s -> _)
  
      runLogged(p)(c)(s) >>= { _.fold[IO[(S,A)]](onErr, Monad[IO].pure) }
    }

    def toResponse(p: Prog[Result]): Prog[Response] = for {
      result <- p
      response <- lift(Ok(result.asJson))
    } yield response
  
    def response(p: Prog[Response])(c: C)(implicit M: Monoid[S]): IO[Response] = {
      for {
        ei  <- runLogged(p)(c)(M.empty)
        res <- ei.fold[IO[Response]](
                 nel ⇒ handleErrs(c)(nel) *> Ok(errsToRes(nel).asJson),
                 p   ⇒ IO pure p._2
               )
      } yield res
    }
  
    def responseO(p: Prog[Response])(c: C)(implicit M: Monoid[S])
      : OptionT[IO,Response] =
      OptionT[IO,Response](response(p)(c) map Some.apply)
  
    def decodeP[A:Decoder](json: Json): Prog[A] =
      wrapValidated(decode[A](json))
  
    def parseAndDecodeP[A:Decoder](s: String): Prog[A] =
      wrapValidated(parseAndDecode[A](s))
  
    def decodeReq[A:Decoder](r: Request): Prog[A] = for {
      json <- entityDecode[Json](r)
      a    <- decodeP[A](json)
    } yield a
  
    def entityDecode[A](r: Request)(implicit D: EntityDecoder[IO,A]): Prog[A] =
      for {
        eith <- lift(D.decode(r, true).value)
        res  <- wrapValidated(handleThrowable(eith))
      } yield res
      
  
    def decodeFD(r: Request): Prog[Multipart[IO]] = entityDecode(r)
  }

  /**
    * A computation involving mutable state
    *
    * All modifications of the given state must pass
    * through the function 'prog'. These computations
    * are then appended to a Queue in order to make
    * sure that they are being processed sequentially
    * and without interleaving with each other.
    *
    * Changes to the database and authentication (which
    * keeps a record of valid temporary hash codes, similar
    * to session cookies) should be done using this method.
    * 
    * The changing state is made available as a
    * thread-safe input (read-only) for other computations
    * wrapped up in a Signal.
    */
  def statefulV[C,S,A](prog: CyByProg[IO,C,S,A])(ini: S)
    (implicit M: CyByMonadIO[C,S],
      CS: cats.effect.ContextShift[IO]): IO[(Ref[S], CyByProg[IO,C,Unit,A])] = {

    val MU = new CyByMonadIO[C,Unit]{
      def handleErr(c: C) = M handleErr c
      def handleLog(c: C) = M handleLog c
    }

    def locked[X](q: MVar[IO,Unit])(io: IO[X]): IO[X] = {
      def acquire: IO[Unit] = q.take
      def release: IO[Unit] = q.put(())

      acquire.bracket(_ ⇒ io)(_ ⇒ release)
    }

    def stateless(c: C, r: Ref[S]): IO[Either[Errs,A]] = for {
      sold <- r.get
      e    <- M.runLogged(prog)(c)(sold)
      r    <- e.traverse{ case (s,a) ⇒ r.set(s).as(a) }
    } yield r

    def run(mv: MVar[IO,Unit], r: Ref[S]): CyByProg[IO,C,Unit,A] = for {
      c <- MU.ask
      e <- MU.lift(locked(mv)(stateless(c, r)))
      a <- MU wrapEither e
    } yield a

    for {
      mv  <- MVar.of[IO,Unit](())
      ref <- cats.effect.concurrent.Ref.of[IO,S](ini)
    } yield (ref, run(mv, ref))
  }

//  @deprecated("use statefulV instead", "2.2")
//  def stateful[C,S,A](prog: CyByProg[IO,C,S,A])(ini: S)
//    (implicit M: CyByMonadIO[C,S], CS: cats.effect.ContextShift[IO])
//    : IStream[(ISignal[S], CyByProg[IO,C,Unit,A])] = {
//
//    val MU = new CyByMonadIO[C,Unit]{
//      def handleErr(c: C) = M handleErr c
//      def handleLog(c: C) = M handleLog c
//    }
//
//    // Result of a stateful computation with the risk of failure
//    type Res[S2] = Either[Errs,(S2,A)]
//
//    // Callback to be invoked once the result is ready
//    type Callback = Either[Throwable,Res[Unit]] ⇒ Unit
//
//    // A mutator is a stateful computation with the potential
//    // of failure and side effects plus a callback to be
//    // invoked once the computation's result is ready
//    type Mutator = (S ⇒ IO[Res[S]], Callback)
//
//    // Mutate a given state S and invoke the
//    // callback before returning the new state
//    def mutate(st: S, m: Mutator): IO[S] = m match {
//      case (mutIO, callback) ⇒ for {
//        e          <- mutIO(st)
//        _          <- delay(callback(Right(e map (p ⇒ () -> p._2))))
//      } yield e.map(_._1) getOrElse st
//    }
//
//    def enqueue(c: C, q: IQueue[Mutator])(callback: Callback): Unit =
//      q.enqueue1(M.runLogged(prog)(c) -> callback).unsafeRunAsync(_ ⇒ ())
//      
//
//    def stateless(q: IQueue[Mutator]): CyByProg[IO,C,Unit,A] =
//      MU.ask flatMap { c ⇒ MU.wrap(_ ⇒ _ ⇒ async(enqueue(c,q))) }
//
//    for {
//      q    <- eval(Queue.unbounded[IO,Mutator])
//      sig  <- q.dequeue.evalScan[IO,S](ini)(mutate).hold(ini)
//    } yield sig -> stateless(q)
//  }
}
