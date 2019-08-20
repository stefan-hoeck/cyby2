/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

import cats.implicits.{none ⇒ _, _}

import cyby.dat._, cyby.dat.example._
import shapeless.{::}

object SubFilS extends FilEditor {
  val parent   = SubS
  val notFound = p ⇒ PathNotFound(SubFilP(p))
  val dbL      = lens[SubS.Srv].files
  val pth      = SubFilP

  def proIds(e: EdSt) = e.nodes match {
    case s::_ ⇒ List(s.project.v)
  }
}

object ConFilS extends FilEditor {
  val notFound = p ⇒ PathNotFound(ConFilP(p))
  val parent   = ConS
  val dbL      = lens[ConS.Srv].files
  val pth      = ConFilP

  def proIds(e: EdSt) = e.nodes match {
    case c::s::_ ⇒ List(c.project.v, s.project.v)
  }
}

object BioFilS extends FilEditor {
  val notFound = p ⇒ PathNotFound(BioFilP(p))
  val parent   = BioS
  val dbL      = lens[BioS.Srv].files
  val pth      = BioFilP

  def proIds(e: EdSt) = e.nodes match {
    case b::c::s::_ ⇒ List(b.project.v, c.project.v, s.project.v)
  }
}

trait FilEditor extends ChildEditor {
  //----------------------------------------------------------------------
  //                         Types
  //----------------------------------------------------------------------
  
  type Id            = Fil.Id
  type ZCli          = Fil.Cli
  type Add           = Fil[Pure,Undef,Pro.Id,Undef,Undef]
  type Mod           = Fil[Option,Undef,Pro.Id,Undef,Undef]
  type Srv           = Fil[Pure,Id,Pro.Id,TimeStamp,EditInfo]
  type SrvAdd        = Fil[Pure,Id,Pro.Id,TimeStamp,EditInfo]
  type SrvMod        = Fil[Option,Undef,Pro.Id,Undef,EditInfo]
  type Acc           = Fil[Pure,Id,Pro.AccId,TimeStamp,EditInfo]

  //----------------------------------------------------------------------
  //                         Util
  //----------------------------------------------------------------------
  val getId      = _.id
  val pth: Path ⇒ cyby.dat.example.Path

  def proIds(e: EdSt): List[Pro.Id]

  def envs(ee: Env, edSt: EdSt) = {
    val e   = ee loggedInEnv getSt(edSt)
    val pp  = edSt.path
    val fs  = dbL get edSt.node
    val ae  = proIds(edSt) -> e.ae
    Envs(ae,ae,e.lvl,pp -> fs,pp -> fs,(),e.ei -> fs.keySet,e.ei,e.u)
  }

  lazy val asmbl = dbAsmbl[Id,Acc,Fil.Cli](_.sortBy(_.name.v.v))

  //----------------------------------------------------------------------
  //                         Valdation
  //----------------------------------------------------------------------
  
  val exists    = (p: parent.Path) ⇒ (f: Fil.Id ,n: FileName) ⇒ FilExists(n, pth(f::p))

  val valid = ValidatorImpl[(ParentPath,DB),(ParentPath,DB),Unit](
    (p,f)   ⇒ uniqAdd(p._2, f.path)(_.path)(exists(p._1)),
    (p,o,f) ⇒ uniqMod(p._2, o.id, f.path)(_.path)(exists(p._1)),
    (_,_,_) ⇒ Nil,
  )

  //----------------------------------------------------------------------
  //                         Authorization
  //----------------------------------------------------------------------
  
  val au = ProAuth(lens[Add].project, lens[Srv].project, lens[Mod].project)
  lazy val auth = au.auth

  def acc(ae: AuthEnv, s: Srv) =
    ae.accPro(s.project) map (ps ⇒ s.copy(project = ps))

  //----------------------------------------------------------------------
  //                         Editing
  //----------------------------------------------------------------------
  
  val mo = DerivedModifier[Srv,SrvMod]
  
  val cud = CUDImpl[(EditInfo,Set[Id]),EditInfo](
    a ⇒ {case (e,i) ⇒ a.copy(id = nextId(i), created  = e.timestamp, modified = e)},
    identity,
    (ei,m) ⇒ m.copy(modified = ei),
    mo.apply
  )
}
