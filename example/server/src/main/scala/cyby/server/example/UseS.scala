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

object UseS extends RootEditor {
  //----------------------------------------------------------------------
  //                         Types
  //----------------------------------------------------------------------
  
  type Id            = Use.Id
  type Srv           = Use[Pure,Id,PWHash,TimeStamp,EditInfo]
  type Add           = Use[Pure,Undef,PWHash,Undef,Undef]
  type Mod           = Use[Option,Undef,PWHash,Undef,Undef]
  type SrvAdd        = Use[Pure,Id,PWHash,TimeStamp,EditInfo]
  type SrvMod        = Use[Option,Undef,PWHash,Undef,EditInfo]
  type Acc           = Use[Pure,Use.AccId,Undef,TimeStamp,EditInfo]
  type Cli           = Use.Cli

  //----------------------------------------------------------------------
  //                         Util
  //----------------------------------------------------------------------
  
  val notFound   = p ⇒ PathNotFound(UseP(p))
  val linked     = (i: Id) ⇒ StillLinked(UseP(i::hnil))
  val exists     = (i: Id,a: Alias) ⇒ Exists(a.name, UseP(i::hnil))
  val dbL        = St.L.uses
  val getId      = _.id

  lazy val asmbl   = asmblD
  lazy val dbasmbl = dbAsmbl[Id,Acc,Use.Cli](_.sortBy(_.alias.v.v))(asmbl)
  val cliToRes      = UseRes

  def envs(ee: Env, edSt: EdSt) = {
    val e  = ee loggedInEnv getSt(edSt)
    Envs(e.lvl,e.u,e.u,e.st.uses,e.st.uses,
      e.st.linkedUses,e.ei -> e.st.uses.keySet,e.ei,e.u)
  }

  val link       = (s: St, i: Id) ⇒ child(s,i::hnil).map(u ⇒ i -> u.alias.name)

  //----------------------------------------------------------------------
  //                         Valdation
  //----------------------------------------------------------------------
  
  val valid = ValidatorImpl[Map[Id,Srv],Map[Id,Srv],List[Id]](
    (d,u)   ⇒ uniqAdd(d, u.alias)(_.alias)(exists),
    (d,o,u) ⇒ uniqMod(d, o.id, u.alias)(_.alias)(exists),
    (l,_,i) ⇒ if (l.exists(i === _)) List(linked(i)) else Nil,
  )


  //----------------------------------------------------------------------
  //                         Authorization
  //----------------------------------------------------------------------
  
  val auth = AuthorizerImpl[UserLevel,UseS.Srv,UseS.Srv](
    (u,_)   ⇒ asAdmin(u),

    (u,o,n) ⇒ must(
        ((same(o.alias, n.alias) && same(o.level, n.level)) || isAdmin(u)) &&
        ((isUser(u) && actual(u)(o.id)) || isAdmin(u))
      )(unauthorized)                                                       ::: 
      mustNot(actual(u)(o.id) && changed(o.alias,n.alias))(CantChangeAlias) :::
      mustNot(actual(u)(o.id) && changed(o.level,n.level))(CantChangeLevel),

    (u,_,i) ⇒ asAdmin(u) ::: mustNot(actual(u)(i))(CantDeleteSelf),
  )

  def acc(ae: AuthEnv, s: Srv) =
    ae.accUse(s.id) map (i ⇒ s.copy(id = i, password = Pure(undef)))

  //----------------------------------------------------------------------
  //                         Editing
  //----------------------------------------------------------------------
  
  val mo = DerivedModifier[Srv,SrvMod]

  val cud = CUDImpl[(EditInfo,Set[Id]),EditInfo](
    a ⇒ {case (e,i) ⇒ a.copy(id = nextId(i), created = e.timestamp, modified = e)},
    identity,
    (ei,m) ⇒ m.copy(modified = ei),
    mo.apply
  )
}
