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

object SupplierS extends RootEditor {
  //----------------------------------------------------------------------
  //                         Types
  //----------------------------------------------------------------------
  
  /**
    * ID used to identify suppliers
    */
  type Id            = Supplier.Id

  /**
    * Data needed to create a new supplier as sent
    * by the client. ID, time of creation and information
    * about last modification cannot be specified.
    */
  type Add           = Supplier[Pure,Undef,Undef,Undef]

  /**
    * Data needed to modify an existing supplier as sent by the client
    * Again, information created by the server is not specified.
    * Neither is the supplier's ID. This is provided by
    * the wrapping {{{cyby.dat.Mod}}}.
    */
  type Mod           = Supplier[Option,Undef,Undef,Undef]

  /**
    * Datatype used to store suppliers in memory at the server.
    * In case of suppliers, this is the same as SrvAdd. Note, however,
    * that this is not the case for more complex data trees like
    * compounds or containers.
    */
  type Srv           = Supplier[Pure,Id,TimeStamp,EditInfo]

  /**
    * Information to create a new supplier as stored on
    * disk. Unlike type Add, all information created at the
    * server (ID, time of creation, information about last
    * modification) has to be specified.
    */
  type SrvAdd        = Supplier[Pure,Id,TimeStamp,EditInfo]

  /**
    * Data needed to modify an existing supplier as stored on disk.
    * Only information about last modification is added by
    * the server. Neither the time of creation nor the supplier's ID
    * can be changed.
    */
  type SrvMod        = Supplier[Option,Undef,Undef,EditInfo]

  type Acc           = Srv

  /**
    * Suppliers as seen by the client.
    */
  type Cli           = Supplier.Cli

  //----------------------------------------------------------------------
  //                         Util
  //----------------------------------------------------------------------
  
  /**
    * Error in case a requested supplier was not found in
    * the data tree.
    */
  val notFound   = p ⇒ PathNotFound(SupP(p))

  /**
    * Error in case a supplier marked for deletion is still
    * linked to other data objects (for instance containers).
    */
  val linked     = (i: Id) ⇒ StillLinked(SupP(i::hnil))

  /**
    * Error in case supplier with the given name exists already.
    */
  val exists     = (i: Id,n: Name) ⇒ Exists(n, SupP(i::hnil))

  /**
    * Lens from the root data object (of type cyby.server.example.St)
    * to the set of suppliers.
    */
  val dbL        = St.L.sups

  val getId      = _.id

  def envs(ee: Env, edSt: EdSt) = {
    val e  = ee loggedInEnv getSt(edSt)
    Envs(e.lvl,e.lvl,e.lvl,e.st.sups,e.st.sups,
      e.st.linkedSups,e.ei -> e.st.sups.keySet,e.ei,e.u)
  }

  val link       = (s: St, i: Id) ⇒ child(s,i::hnil).map(t ⇒ i -> t.name.v)

  lazy val asmbl = asmblD[Acc,Supplier.Cli]
  lazy val dbasmbl = dbAsmbl[Id,Acc,Supplier.Cli](_.sortBy(_.name.v.v))(asmbl)
  val cliToRes      = SupRes

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
