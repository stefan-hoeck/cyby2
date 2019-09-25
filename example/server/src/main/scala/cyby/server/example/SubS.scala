/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

import cats.implicits._
import cyby.chem.Mol
import cyby.dat.{Mol ⇒ _, _}, cyby.dat.example._

object SubS extends RootEditor {
  //----------------------------------------------------------------------
  //                         Types
  //----------------------------------------------------------------------
  
  type Id            = Sub.Id
  type Add           = Sub[Pure,Undef,Mol,Pro.Id,Undef,Undef,Undef,Undef]
  type Mod           = Sub[Option,Undef,Mol,Pro.Id,Undef,Undef,Undef,Undef]
  type Srv           = Sub[Pure,Id,Mol,Pro.Id,ContainerS.DB,SubFilS.DB,TimeStamp,EditInfo]
  type SrvAdd        = Sub[Pure,Id,Mol,Pro.Id,Undef,Undef,TimeStamp,EditInfo]
  type SrvMod        = Sub[Option,Undef,Mol,Pro.Id,Undef,Undef,Undef,EditInfo]
  type Acc           = Sub[Pure,Id,Mol,Pro.AccId,ContainerS.AccDB,SubFilS.AccDB,TimeStamp,EditInfo]
  type Cli           = Sub.Cli

  //----------------------------------------------------------------------
  //                         Util
  //----------------------------------------------------------------------

  val notFound   = p ⇒ PathNotFound(SubP(p))
  val exists     = (i: Id,n: Name) ⇒ Exists(n, SubP(i::hnil))

  val dbL        = St.L.subs
  val getId      = _.id

  def envs(ee: Env, edSt: EdSt) = {
    val e  = ee loggedInEnv getSt(edSt)
    Envs(Nil -> e.ae, Nil -> e.ae,e.lvl,
      e.st.subs,e.st.subs,(), e.ei -> e.st.subs.keySet,e.ei,e.u)
  }

  implicit lazy val filA: Asmbl[SubFilS.AccDB,List[Fil.Cli]] = SubFilS.asmbl
  implicit lazy val conA: Asmbl[ContainerS.AccDB,List[Container.Cli]] = ContainerS.asmbl
  lazy val asmbl = asmblD[Acc,Sub.Cli]
  lazy val dbasmbl = dbAsmbl[Id,Acc,Sub.Cli](identity)(asmbl)
  val cliToRes = SubRes

  //----------------------------------------------------------------------
  //                         Valdation
  //----------------------------------------------------------------------
  
  val subExists = (i: Sub.Id, p: Plain) ⇒ SubExists(p, i::hnil)
  val casExists = (i: Sub.Id, n: CasNr) ⇒ CasNrExists(n, i::hnil)
  val strExists = (i: Sub.Id, p: (Mol,Boolean)) ⇒
                    StructureExists(p._1.toDatMol, i::hnil)

  val mp  = (s: Pure[Maybe[Mol]], a: Pure[Boolean]) ⇒ s map (_.o map (_ -> a.v))

  val mpa = (s: Add) ⇒ mp(s.structure, s.abs)
  val mps = (s: Srv) ⇒ mp(s.structure, s.abs)
  val mpm = (s: Srv, m: Mod) ⇒ mp(m.structure.fold(s.structure)(Pure.apply),
                                  m.abs.fold(s.abs)(Pure.apply))


  def structEmpty(s: Maybe[Mol], p: Plain) =
    mustNot(s.isEmpty && p.v.isEmpty)(EmptyStructure)

  def optP(p: Plain): Option[Plain] = if (p.v.isEmpty) None else Some(p)
  def optC(c: CasNr): Option[CasNr] = if (c.v.isEmpty) None else Some(c)

  val valid = ValidatorImpl[Map[Id,Srv],Map[Id,Srv],Unit](
    (d,u)   ⇒ uniqAddO(d,optP(u.name))(_.name map optP)(subExists)   :::
              uniqAddO(d,optC(u.casNr))(_.casNr map optC)(casExists) :::
              uniqAddO(d,mpa(u))(mps)(strExists)                     :::
              structEmpty(u.structure.v, u.name.v),

    (d,o,u) ⇒ uniqModO(d,o.id,u.name map optP)(_.name map optP)(subExists)   :::
              uniqModO(d,o.id,u.casNr map optC)(_.casNr map optC)(casExists) :::
              uniqModO(d,o.id,some(mpm(o,u).v))(mps)(strExists)              :::
              structEmpty(u.structure getOrElse o.structure.v, u.name getOrElse o.name.v),

    (_,_,_) ⇒ Nil,
  )

  //----------------------------------------------------------------------
  //                         Authorization
  //----------------------------------------------------------------------
  
  val au = ProAuth(lens[Add].project, lens[Srv].project, lens[Mod].project)
  lazy val auth = au.auth

  def acc(ae: AuthEnv, s: Srv) = ae.accPro(s.project) map (
    ps ⇒ s.copy(project = ps, files = ConFilS.accDB(ae, s.files), containers = ContainerS.accDB(ae, s.containers))
  )

  //----------------------------------------------------------------------
  //                         Editing
  //----------------------------------------------------------------------
  
  val mo = DerivedModifier[Srv,SrvMod]

  val cud = CUDImpl[(EditInfo,Set[Id]),EditInfo](
    a ⇒ {case (e,i) ⇒ a.copy(id = nextId(i), created  = e.timestamp, modified = e)},
    a ⇒ a.copy(containers = Map(), files = Map()),
    (ei,m) ⇒ m.copy(modified = ei),
    mo.apply
  )
}

