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

object CompoundS extends RootEditor {
  //----------------------------------------------------------------------
  //                         Types
  //----------------------------------------------------------------------
  
  type Id            = Compound.Id
  type Add           = Compound[Pure,Undef,Mol,Project.Id,Undef,Undef,Undef,Undef]
  type Mod           = Compound[Option,Undef,Mol,Project.Id,Undef,Undef,Undef,Undef]
  type Srv           = Compound[Pure,Id,Mol,Project.Id,ContainerS.DB,CpdFileS.DB,TimeStamp,EditInfo]
  type SrvAdd        = Compound[Pure,Id,Mol,Project.Id,Undef,Undef,TimeStamp,EditInfo]
  type SrvMod        = Compound[Option,Undef,Mol,Project.Id,Undef,Undef,Undef,EditInfo]
  type Acc           = Compound[Pure,Id,Mol,Project.AccId,ContainerS.AccDB,CpdFileS.AccDB,TimeStamp,EditInfo]
  type Cli           = Compound.Cli

  //----------------------------------------------------------------------
  //                         Util
  //----------------------------------------------------------------------

  val notFound   = p ⇒ PathNotFound(CpdP(p))
  val exists     = (i: Id,n: Name) ⇒ Exists(n, CpdP(i::hnil))

  val dbL        = St.L.subs
  val getId      = _.id

  def envs(ee: Env, edSt: EdSt) = {
    val e  = ee loggedInEnv getSt(edSt)
    Envs(Nil -> e.ae, Nil -> e.ae,e.lvl,
      e.st.subs,e.st.subs,(), e.ei -> e.st.subs.keySet,e.ei,e.u)
  }

  implicit lazy val filA: Asmbl[CpdFileS.AccDB,List[File.Cli]] = CpdFileS.asmbl
  implicit lazy val conA: Asmbl[ContainerS.AccDB,List[Container.Cli]] = ContainerS.asmbl
  lazy val asmbl = asmblD[Acc,Compound.Cli]
  lazy val dbasmbl = dbAsmbl[Id,Acc,Compound.Cli](identity)(asmbl)
  val cliToRes = CpdRes

  //----------------------------------------------------------------------
  //                         Valdation
  //----------------------------------------------------------------------
  
  val subExists = (i: Compound.Id, p: Plain) ⇒ CpdExists(p, i::hnil)
  val casExists = (i: Compound.Id, n: CasNr) ⇒ CasNrExists(n, i::hnil)
  val strExists = (i: Compound.Id, p: (Mol,Boolean)) ⇒
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
    ps ⇒ s.copy(project = ps, files = ConFileS.accDB(ae, s.files), containers = ContainerS.accDB(ae, s.containers))
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

