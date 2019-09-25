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
    load(UseT, _.uses, doLoad(UserS)(hnil))        >>=
    load(SupT, _.sups, doLoad(SupplierS)(hnil))        >>=
    load(ProT, _.pros, doLoad(ProjectS)(hnil))        >>=
    load(StoT, _.stos, doLoad(LocationS)(hnil))        >>=
    load(MetT, _.mets, doLoad(MethodS)(hnil))        >>=
    load(CpdT, _.subs, edCpd)

  def doLoad(e: ExampleEditor)(p: e.ParentPath)(implicit D: Decoder[e.LoadEd]): Editor =
    s ⇒ st ⇒ parseAndDecodeE[e.LoadEd](s) flatMap (e.load(st,p,_))

  val edCpd: Editor = s ⇒ st ⇒ parseAndDecodeE[CpdTreeL](s) flatMap {
    case CpdEdit(ed)       ⇒ CompoundS.load(st, hnil, ed)
    case ConEdit(p, ed)    ⇒ ContainerS.load(st, p, ed)
    case BioEdit(p, ed)    ⇒ BiodataEntryS.load(st, p, ed)
    case CpdFilEdit(p, ed) ⇒ CpdFilS.load(st, p, ed)
    case ConFilEdit(p, ed) ⇒ ConFilS.load(st, p, ed)
    case BioFilEdit(p, ed) ⇒ BioFilS.load(st, p, ed)
  }
}

// vim: set ts=2 sw=2 et:
