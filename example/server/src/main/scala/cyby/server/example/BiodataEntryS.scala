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

/**
  * CRUD of bio data
  */
object BiodataEntryS extends ChildEditor {
  //----------------------------------------------------------------------
  //                         Types
  //----------------------------------------------------------------------
  
  type Id            = BiodataEntry.Id
  type ZCli          = BiodataEntry.Cli
  type Add           = BiodataEntry[Pure,Undef,Met.Id,Sup.Id,Pro.Id,Undef,Undef,Undef]
  type Mod           = BiodataEntry[Option,Undef,Met.Id,Sup.Id,Pro.Id,Undef,Undef,Undef]
  type Srv           = BiodataEntry[Pure,Id,Met.Id,Sup.Id,Pro.Id,BioFilS.DB,TimeStamp,EditInfo]
  type SrvAdd        = BiodataEntry[Pure,Id,Met.Id,Sup.Id,Pro.Id,Undef,TimeStamp,EditInfo]
  type SrvMod        = BiodataEntry[Option,Undef,Met.Id,Sup.Id,Pro.Id,Undef,Undef,EditInfo]
  type Acc           = BiodataEntry[Pure,Id,Met.Id,Sup.Id,Pro.AccId,BioFilS.AccDB,TimeStamp,EditInfo]

  //----------------------------------------------------------------------
  //                         Util
  //----------------------------------------------------------------------

  val parent     = ConS
  val notFound   = p ⇒ PathNotFound(BioP(p))

  val dbL        = lens[ConS.Srv].bio
  val getId      = _.id

  def envs(ee: Env, edSt: EdSt) = {
    import shapeless.{::}
    val c::s::st::_ = edSt.nodes
    val e  = ee loggedInEnv getSt(edSt)
    val ae = List(c.project.v, s.project.v) -> e.ae
    Envs(ae,ae,e.lvl,(),(),(),e.ei -> c.bio.keySet,e.ei,e.u)
  }

  implicit lazy val filA: Asmbl[BioFilS.AccDB,List[Fil.Cli]] = BioFilS.asmbl
  lazy val asmbl = dbAsmbl[Id,Acc,BiodataEntry.Cli](_.sortBy(_.method.v._2.v))

  //----------------------------------------------------------------------
  //                         Valdation
  //----------------------------------------------------------------------
  
  val valid = ValidatorImpl[Unit,Unit,Unit]((_,_) ⇒ Nil, (_,_,_) ⇒ Nil, (_,_,_) ⇒ Nil)

  //----------------------------------------------------------------------
  //                         Authorization
  //----------------------------------------------------------------------
  
  val au = ProAuth(lens[Add].project, lens[Srv].project, lens[Mod].project)
  lazy val auth = au.auth

  def acc(ae: AuthEnv, s: Srv) = ae.accPro(s.project) map (
    ps ⇒ s.copy(project = ps, files = BioFilS.accDB(ae, s.files))
  )
  
  //----------------------------------------------------------------------
  //                         Editing
  //----------------------------------------------------------------------
  
  val mo = DerivedModifier[Srv,SrvMod]
  
  val cud = CUDImpl[(EditInfo,Set[Id]),EditInfo](
    a ⇒ {case (e,i) ⇒ a.copy(id = nextId(i), created  = e.timestamp, modified = e)},
    a ⇒ a.copy(files = Map()),
    (ei,m) ⇒ m.copy(modified = ei),
    mo.apply
  )
}
