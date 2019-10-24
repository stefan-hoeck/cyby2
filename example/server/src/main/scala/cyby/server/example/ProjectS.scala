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

object ProjectS extends RootEditor {
  //----------------------------------------------------------------------
  //                         Types
  //----------------------------------------------------------------------
  
  type Id            = Project.Id
  type Add           = Project[Pure,Undef,User.Id,Undef,Undef]
  type Mod           = Project[Option,Undef,User.Id,Undef,Undef]
  type Srv           = Project[Pure,Id,User.Id,TimeStamp,EditInfo]
  type SrvAdd        = Project[Pure,Id,User.Id,TimeStamp,EditInfo]
  type SrvMod        = Project[Option,Undef,User.Id,Undef,EditInfo]
  type Acc           = Project[Pure,Project.AccId,User.Id,TimeStamp,EditInfo]
  type Cli           = Project.Cli

  //----------------------------------------------------------------------
  //                         Util
  //----------------------------------------------------------------------
  

  val notFound   = p ⇒ PathNotFound(ProP(p))
  val linked     = (i: Id) ⇒ StillLinked(ProP(i::hnil))
  val exists     = (i: Id,n: Name) ⇒ Exists(n, ProP(i::hnil))

  val dbL        = St.L.pros
  val getId      = _.id

  def envs(ee: Env, edSt: EdSt) = {
    val e  = ee loggedInEnv getSt(edSt)
    Envs(e.lvl,e.u,e.lvl,e.st.pros,e.st.pros,
      e.st.linkedPros,e.ei -> e.st.pros.keySet,e.ei,e.u)
  }

  val link       = (s: St, i: Project.AccId) ⇒ getSrv(i.v,s.root).map(t ⇒ i -> t.name.v)

  lazy val asmbl    = asmblD[Acc,Project.Cli]
  lazy val dbasmbl  = dbAsmbl[Id,Acc,Project.Cli](_.sortBy(_.name.v.v))(asmbl)
  val cliToRes      = ProRes

  //----------------------------------------------------------------------
  //                         Valdation
  //----------------------------------------------------------------------
  
  val valid = ValidatorImpl[Map[Id,Srv],Map[Id,Srv],List[Id]](
    (d,u)   ⇒ uniqAdd(d, u.name)(_.name)(exists),
    (d,o,u) ⇒ uniqMod(d, o.id, u.name)(_.name)(exists),
    (l,_,i) ⇒ if (l.exists(i === _)) List(linked(i)) else Nil,
  )

  //----------------------------------------------------------------------
  //                         Authorization
  //----------------------------------------------------------------------
  
  val auth = AuthorizerImpl[UserLevel,UserS.Srv,UserLevel](
    (u,_)   ⇒ asSuperUser(u),
    (u,p,_) ⇒ if (isAdmin(u)) Nil else asUser(u) ::: asOwner(u,p),
    (u,_,_) ⇒ asAdmin(u),
  )

  def acc(ae: AuthEnv, s: Srv) = ae.project(s.id) map (i ⇒ s.copy(id = i))

  //----------------------------------------------------------------------
  //                         Editing
  //----------------------------------------------------------------------
  
  val mo = DerivedModifier[Srv,SrvMod]

  val cud = CUDImpl[(EditInfo,Set[Id]),EditInfo](
    a ⇒ {case (e,i) ⇒ a.copy(id = nextId(i), created  = e.timestamp, modified = e)},
    a ⇒ a,
    (ei,m) ⇒ m.copy(modified = ei),
    mo.apply
  )
}
