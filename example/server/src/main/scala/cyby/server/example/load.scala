/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

import cats.implicits._
import tagInstances._
import io.circe.Decoder

import cyby.dat._, example._

/**
  * Component for loading user settings and all data into
  * memory when starting the application.
  */
case class Loader(coreSettings: CoreSettings) extends LoadEnv with CyByZ {
  lazy val logger = Main.logger
    
  def loadSt: IO[St] =
    settings(logger).map(St.L.sets.set(St.empty)) >>=
    load(UseT, _.uses, doLoad(UseS)(hnil))        >>=
    load(SupT, _.sups, doLoad(SupS)(hnil))        >>=
    load(ProT, _.pros, doLoad(ProjectS)(hnil))        >>=
    load(StoT, _.stos, doLoad(StoS)(hnil))        >>=
    load(MetT, _.mets, doLoad(MethodS)(hnil))        >>=
    load(SubT, _.subs, edSub)

  def doLoad(e: ExampleEditor)(p: e.ParentPath)(implicit D: Decoder[e.LoadEd]): Editor =
    s ⇒ st ⇒ parseAndDecodeE[e.LoadEd](s) flatMap (e.load(st,p,_))

  val edSub: Editor = s ⇒ st ⇒ parseAndDecodeE[SubTreeL](s) flatMap {
    case SubEdit(ed)       ⇒ SubS.load(st, hnil, ed)
    case ConEdit(p, ed)    ⇒ ContainerS.load(st, p, ed)
    case BioEdit(p, ed)    ⇒ BiodataEntryS.load(st, p, ed)
    case SubFilEdit(p, ed) ⇒ SubFilS.load(st, p, ed)
    case ConFilEdit(p, ed) ⇒ ConFilS.load(st, p, ed)
    case BioFilEdit(p, ed) ⇒ BioFilS.load(st, p, ed)
  }
}

// vim: set ts=2 sw=2 et:
