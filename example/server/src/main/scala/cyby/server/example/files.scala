/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

import cyby.dat.example.{NotFound ⇒ _, _}

import org.http4s.dsl.io._

/**
  * Component for downloading files
  */
case class Files(coreSettings: CoreSettings) extends FileEnv with CyByZ {
  val M = CyByMonadIO.authEnv[Unit]

  def prog(pth: Path): M.Prog[Response] = pth match {
    case CpdFilP(h) ⇒ srv(h)(CpdFilS.child, pth)
    case ConFilP(h) ⇒ srv(h)(ConFilS.child, pth)
    case BioFilP(h) ⇒ srv(h)(BioFilS.child, pth)
    case _          ⇒ M lift notFound
  }

  def srv[P](p: P)(f: (St, P) ⇒ DataE[CpdFilS.Srv], pth: Path): M.Prog[Response] =
    for {
      fil <- M.ask map (ae ⇒ f(ae.st, p).toOption)
      _   <- M debug s"looking for file ${pth}: found ${fil}"
      res <- M lift fil.fold(notFound){ f ⇒ for {
               bytesO <- get(pth)
               res    <- bytesO.fold(notFound)(Ok(_))
             } yield res}
    } yield res
}

// vim: set ts=2 sw=2 et:
