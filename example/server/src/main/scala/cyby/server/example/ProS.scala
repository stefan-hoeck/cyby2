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

object ProS extends RootEditor {
  //----------------------------------------------------------------------
  //                         Types
  //----------------------------------------------------------------------
  
  type Id            = Pro.Id
  type Add           = Pro[Pure,Undef,Use.Id,Undef,Undef]
  type Mod           = Pro[Option,Undef,Use.Id,Undef,Undef]
  type Srv           = Pro[Pure,Id,Use.Id,TimeStamp,EditInfo]
  type SrvAdd        = Pro[Pure,Id,Use.Id,TimeStamp,EditInfo]
  type SrvMod        = Pro[Option,Undef,Use.Id,Undef,EditInfo]
  type Acc           = Pro[Pure,Pro.AccId,Use.Id,TimeStamp,EditInfo]
  type Cli           = Pro.Cli

  //----------------------------------------------------------------------
  //                         Util
  //----------------------------------------------------------------------
  

  val notFound   = p ⇒ PathNotFound(ProP(p))
  val linked     = (i: Id) ⇒ StillLinked(ProP(i::hnil))
  val exists     = (i: Id,n: Name) ⇒ Exists(n, ProP(i::hnil))

  val dbL        = St.L.pros
  val getId      = _.id

  def envs(e: Env, edSt: EdSt) = Envs(e.lvl,e.u,e.lvl,e.st.pros,e.st.pros,
    e.st.linkedPros,e.ei -> e.st.pros.keySet,e.ei,e.u)

  val link       = (s: St, i: Pro.AccId) ⇒ getSrv(i.to,s.root).map(t ⇒ i -> t.name.v)

  lazy val asmbl    = asmblD[Acc,Pro.Cli]
  lazy val dbasmbl  = dbAsmbl[Id,Acc,Pro.Cli](_.sortBy(_.name.v.v))(asmbl)
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
  
  val auth = AuthorizerImpl[UserLevel,UseS.Srv,UserLevel](
    (u,_)   ⇒ asSuperUser(u),
    (u,p,_) ⇒ if (isAdmin(u)) Nil else asUser(u) ::: asOwner(u,p),
    (u,_,_) ⇒ asAdmin(u),
  )

  def acc(ae: AuthEnv, s: Srv) = ae.accPro(s.id) map (i ⇒ s.copy(id = i))

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
