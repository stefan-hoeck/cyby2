/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server

import cats.implicits._

import cyby.dat._
import doobie.implicits._

import io.circe.{Decoder, Encoder}
import fs2.text.utf8Decode

/**
  * Environmental trait for mutating data objects
  */
trait MutateEnv[E] extends ServerEnv {
  val M: CyByMonadIO[E,St]

  protected def env(e: E): Env[St]

  def readErr(s: String): Err

  /**
    * Creates a log for an editing object
    */
  protected def logEd[I,A,M](prefix: String, ed: Edit[I @@ IsValid,A,M])(get: A ⇒ I): Log =
    ed match {
      case Add(a)    ⇒ Log info s"${prefix} added: ${get(a)}"
      case Mod(i, _) ⇒ Log info s"${prefix} updated: ${i}"
      case Del(i)    ⇒ Log warn s"${prefix} deleted: ${i}"
    }

  /**
    * stores user settings in the database
    */
  def storeSettings(p: (UseId, USettings)): M.Prog[Unit] =
    M lift updateSettings(p._1, p._2).transact(transactor)

  def applyEdit[A:Decoder,B:Encoder](
    run: (E,A) ⇒ DataE[(St @@ Adjusted,B,Result)],
    log: B ⇒ Log,
    dt : DataType,
    post: A ⇒ IO[Unit] = (a: A) ⇒ ioUnit
  ): M.Prog[Result] = applyEditP[A,B](
    (as,a) ⇒ M.wrapEither(run(as,a)) <* M.lift(post(a)),
    log,
    dt
  )

  def applyEditP[A:Decoder,B:Encoder](
    run: (E,A) ⇒ M.Prog[(St @@ Adjusted,B,Result)],
    log: B ⇒ Log,
    dt : DataType,
  ): M.Prog[Result] = decodeAndRunP(run)(
    b ⇒ M.lift(appendLine(dt, stripEnc(b).noSpaces)) *> M.doLog(log(b))
  )

  def decodeAndRun[A:Decoder,B](run: (E,A) ⇒ (St @@ Adjusted,B,Result))
    (process: B ⇒ M.Prog[Unit]): M.Prog[Result] =
    decodeAndRunP[A,B]((le,a) ⇒ M pure run(le,a))(process)

  private def decodeAndRunP[A:Decoder,B](run: (E,A) ⇒ M.Prog[(St @@ Adjusted,B,Result)])
    (process: B ⇒ M.Prog[Unit]): M.Prog[Result] = for {
    le   <- M.ask
    a    <- M.decodeReq[A](env(le).req)
    t    <- run(le,a)
    (newSt,b,res) = t
    _    <- process(b)
    _    <- M set newSt
  } yield res

  /**
    * Appends a line of text to the file representing edits
    * of the given datatype.
    */
  def appendLine(dt: DataType, s: String): IO[Unit] = delay {
    val fw = new java.io.FileWriter(s"${dataPath}/${dt}.json", true)
    try { fw.write(s"\n${s}") }
    finally fw.close()
  }

  /**
    * Decodes a multipart object containing a file blob and a
    * JSON-encoded data object.
    */
  def decodeAddFile[A:Decoder](r: Request): M.Prog[(A,Array[Byte])] = for {
    mp <- M.decodeFD(r)
    p  <- mp.parts.toList.map{ p ⇒ p.name -> p}.sortBy(_._1) match {
            case (Some("file"),pf)::(Some("json"),pj)::Nil ⇒ for {
              txt <- M.lift(pj.body.through(utf8Decode).compile.toList.map(_ mkString ""))
              arr <- M.lift(pf.body.compile.to[Array])
              st  <- M.parseAndDecodeP[A](txt)
            } yield st -> arr
            case _ ⇒ M.raise(readErr("error when decoding multipart object"))
          }
  } yield p
}

