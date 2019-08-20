/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

import cats.implicits.{none ⇒ _, _}
import cyby.dat.{UserLevel, Found, EditRes, example, Edit, Mod}, example.Pro
import cyby.syntax._

import shapeless.{::, HNil, HList}

/**
  * Default editing environment in this example application.
  *
  * Provides utility functions and default implementations
  * in all data editors.
  */
trait ExampleEditor extends Editor {
  type St          = cyby.server.example.St
  type Env         = cyby.server.example.EditEnv
  type Err         = cyby.dat.example.DataErr
  type Res         = cyby.dat.example.Result
  type ResEnv      = UseS.Srv
  type Acc

  type MF = (Boolean,String) ⇒ Option[cyby.query.MapFilter[Id,Acc]]

  final type AccDB = Map[Id,Acc]

  lazy val L = lens[Acc]

  val unauthorized = cyby.dat.example.Unauthorized

  def acc(ae: AuthEnv, s: Srv): Option[Acc]

  def accE(ae: AuthEnv)(s: Srv): DataE[Acc] =
    optionToErrNel(unauthorized)(acc(ae,s))

  final def accP(ae: AuthEnv): ((Id,Srv)) ⇒ Option[(Id,Acc)] =
    { case (i,s) ⇒ acc(ae, s) map (i -> _) }


  final def accDB(a: AuthEnv, db: DB): AccDB =
    db collect scala.Function.unlift(accP(a))

  /**
    * Case class for authorizing by project access.
    *
    * We use a class instead of an instance of AuthorizerImpl for
    * better testability.
    */
  case class ProAuth(
    ap: Lens[Add,Pure[Pro.Id]],
    sp: Lens[Srv,Pure[Pro.Id]],
    mp: Lens[Mod,Option[Pro.Id]],
  ){
    lazy val auth = AuthorizerImpl[ProAuthEnv,ProAuthEnv,UserLevel](
      (p,m)   ⇒ p._2.authAdd(ap.get(m).v :: p._1),
      (p,m,o) ⇒ p._2.authMod(sp.get(m).v :: p._1, mp.get(o)),
      (u,_,_) ⇒ asAdmin(u),
    )
  }

  def basicAuth = AuthorizerImpl[UserLevel,UserLevel,UserLevel](
    (u,_)   ⇒ asUser(u),
    (u,_,_) ⇒ asUser(u),
    (u,_,_) ⇒ asAdmin(u),
  )
}

trait DBEditor extends ExampleEditor {
  type Cli

  val cliToRes: EditRes[Cli] ⇒ Res

  val asmbl: Asmbl[Acc,Cli]

  val dbasmbl: Asmbl[AccDB,List[Cli]]

  def getAll(as: AuthSt, db: DB): DataV[Res] =
    dbasmbl.run(as.st, accDB(as.env,db)) map (cs ⇒ cliToRes(Found(cs,cs.size,0)))

  def toResP[I,A,M](ae: AuthSt, s: Srv, e: Edit[I,A,M]): DataE[Res] =
    e.editRes(s).traverse(accE(ae.env)(_) >>= asmbl.runE(ae.st)).map(cliToRes)
}

/**
  * Editor for data types whose objects can be identified
  * by a heterogenous path of IDs. This is typical for heterogeneous
  * trees where parent objects are of one type and child objects
  * are of another type. This type of editor could also be used
  * for editing items in a homogeneous tree at a predefined depth
  * but not for items at arbitrary depths (the depth is encoded in
  * the type of the ParentPath).
  */
trait HListEditor extends ExampleEditor {
  type ParentPath <: HList
  type NS         <: HList
  type N
  type Path = Id :: ParentPath
  type EdSt       = HQ[N,ParentPath,NS]

  val toResNS    : (ResEnv, Srv::N::NS) ⇒ DataE[Res]
  val getStNS    : (N::NS) ⇒ St
  val notFound   : Path ⇒ Err
  val dbL        : Lens[N,Map[Id,Srv]]

  val getSt  = hq ⇒ getStNS(hq.nodes)

  val getSrv = (i,s) ⇒ childEd(i,s) map (_.node)

  val childEd: (Id, EdSt) ⇒ DataE[HQ[Srv,Path,N::NS]] =
    (i,e) ⇒ e.queryNel((k: Id) ⇒ dbL at k)(notFound, i)

  val childEdP: (St,Path) ⇒ DataE[HQ[Srv,Path,N::NS]] =
    (s,p) ⇒ edSt(s,p.tail).flatMap(childEd(p.head,_))

  val child: (St,Path) ⇒ DataE[Srv] = childEdP(_,_) map (_.node)

  val edit   = (i,o,s) ⇒ HQ(s.setter, s.path, s.setL(dbL.at(i), o))
}

/**
  * Used for editing types at the root of the data tree, like
  * compounds, suppliers etc. The corresponding data objects
  * are direct children of an St object.
  */
trait RootEditor extends HListEditor with DBEditor {
  type NS          = HNil
  type N           = St
  type ParentPath  = HNil

  val toRes   = (u,s,edSt,e) ⇒ toResP(AuthSt(edSt.node,u),s,e)

  val toResNS = (u,ns) ⇒ toResP(AuthSt(ns.tail.head,u),ns.head,Mod("",""))

  val getStNS = _.head

  val edSt    = (s,_)   ⇒ Right(s.root)

}

/**
  * Used for editing types not at the root of the data tree.
  */
trait ChildEditor extends HListEditor {
  val parent: HListEditor

  type ParentPath = parent.Path
  type NS         = parent.N :: parent.NS
  type N          = parent.Srv

  lazy val toRes   = (r,s,st,_) ⇒ toResNS(r,s :: st.nodes)
  lazy val toResNS = (r,ns)     ⇒ parent.toResNS(r,ns.tail)
  lazy val getStNS = ns         ⇒ parent getStNS ns.tail
  lazy val edSt    = parent.childEdP
}
