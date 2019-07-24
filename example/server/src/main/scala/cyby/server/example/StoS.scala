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

object StoS extends RootEditor {
  //----------------------------------------------------------------------
  //                         Types
  //----------------------------------------------------------------------
  
  type Id            = Sto.Id
  type Add           = Sto[Pure,Undef,Undef,Undef]
  type Mod           = Sto[Option,Undef,Undef,Undef]
  type Srv           = Sto[Pure,Id,TimeStamp,EditInfo]
  type SrvAdd        = Sto[Pure,Id,TimeStamp,EditInfo]
  type SrvMod        = Sto[Option,Undef,Undef,EditInfo]
  type Acc           = Srv
  type Cli           = Sto.Cli

  //----------------------------------------------------------------------
  //                         Util
  //----------------------------------------------------------------------
  
  val notFound   = p ⇒ PathNotFound(StoP(p))
  val linked     = (i: Id) ⇒ StillLinked(StoP(i::hnil))
  val exists     = (i: Id,n: Name) ⇒ Exists(n, StoP(i::hnil))

  val dbL        = St.L.stos
  val getId      = _.id

  def envs(e: Env, edSt: EdSt) = Envs(e.lvl,e.lvl,e.lvl,e.st.stos,e.st.stos,
    e.st.linkedStos,e.ei -> e.st.stos.keySet,e.ei,e.u)

  val link       = (s: St, i: Id) ⇒ child(s,i::hnil).map(t ⇒ i -> t.name.v)

  lazy val asmbl = asmblD[Acc,Sto.Cli]
  lazy val dbasmbl = dbAsmbl[Id,Acc,Sto.Cli](_.sortBy(_.name.v.v))(asmbl)
  val cliToRes      = StoRes

  //----------------------------------------------------------------------
  //                         Valdation
  //----------------------------------------------------------------------
  
  val valid = ValidatorImpl[DB,DB,List[Id]](
    (d,u)   ⇒ uniqAdd(d, u.name)(_.name)(exists),
    (d,o,u) ⇒ uniqMod(d, o.id, u.name)(_.name)(exists),
    (l,_,i) ⇒ if (l.exists(i === _)) List(linked(i)) else Nil,
  )

  //----------------------------------------------------------------------
  //                         Authorization
  //----------------------------------------------------------------------
  
  val auth = basicAuth

  def acc(ae: AuthEnv, s: Srv) = some(s)

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
