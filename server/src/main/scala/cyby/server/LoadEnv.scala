/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server

import cats.implicits._

/**
  * Environmental trait for loading data objects from disk
  */
trait LoadEnv extends ServerEnv {
  val logger: Logger[IO]
    
  def debugLog(m: ⇒ String): IO[Unit] = logger(Log(Debug, m))

  def infoLog(m: ⇒ String): IO[Unit] = logger(Log(Info, m))

  def warnLog(m: ⇒ String): IO[Unit] = logger(Log(Warning, m))

  def errorLog(m: ⇒ String): IO[Unit] = logger(Log(Error, m))

  /**
    * Results of loading a data object. We either get a non-empty
    * lists of logging messages or an updated state together
    * with the line number of the processed entry in the data file
    */
  type LoadE = Either[Nel[Log],(Long,St)]

  /**
    * Function taking a line of encoded updating information
    * plus the actual state object
    * resulting either in a non-empty list of error messages or
    * a properly updated state object
    */
  type Editor = String ⇒ St ⇒ DataE[St @@ Adjusted]

  /**
    * Given an editing function and an enum representing the type
    * of objects actually being processed, the actual state and the
    * line just read from disk, this updates the state logging any
    * errors together with the processed line number
    */
  def applyEdit(editor: Editor, dt: DataType)(le: LoadE, s: String): LoadE = {
    def logErr(lineNr: Long)(e: Err): Log =
      Log(Error, s"error in ${dt} line ${lineNr}: ${e}")

    def ed(p: (Long,St)) = p match {
      case (n,st2) ⇒ mapErr(editor(s)(st2) map ((n+1) -> _))(logErr(n))
    }

    if (s.isEmpty) le else le flatMap ed
  }

  def ignoreLine(s: String): Boolean =
    s.isEmpty || s.startsWith("//")

  /**
    * Loads all editing entries from disk (the location is determined
    * by the data type given) and applies them using the given Editor
    * and initial state. If things are successful, returns the updated state.
    * Otherwise, the unmodified state is returned and all errors are properly
    * logged.
    */
  def load[I,A](dt: DataType, m: St ⇒ Map[I,A], e: Editor)(st: St): IO[St] = {
    def sizeStr(st: St) = m(st).size match {
      case 1 ⇒ s"Loaded 1 ${dt}"
      case n ⇒ s"Loaded ${n} ${dt}s"
    }

    utf8Lines(s"${dataPath}/${dt}.json")(coreSettings.contextShift)
      .filter(s ⇒ !ignoreLine(s))
      .evalMap(s ⇒ debugLog(s"Processing: ${s take 40}") as s)
      .scan[LoadE](Right(1L -> st))(applyEdit(e,dt))
      .compile
      .last
      .map(_ getOrElse Right(1L -> st))
      .flatMap {
        case Right((_,st2)) ⇒ infoLog(sizeStr(st2)) as st2
        case Left(e)        ⇒ e.traverse_(logger.apply) as st
      }
  }
}
