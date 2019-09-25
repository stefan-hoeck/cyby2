/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example
import cyby.dat.{Mol ⇒ DMol, _}, example._

/**
  * Data type representing the application's in-memory
  * state containing all loaded data and actual user settings.
  */
case class St(
  pros: ProjectS.DB,
  stos: StoS.DB,
  subs: SubS.DB,
  sups: SupS.DB,
  mets: MetS.DB,
  uses: UseS.DB,
  sets: Map[Use.Id,USettings],
){

  //--------------------------------------------------------------------
  //                   Users and Settings
  //--------------------------------------------------------------------
  
  def settingsFor(id: Use.Id): USettings =
    sets get id getOrElse St.defaultSettings

  lazy val names: Map[Alias, Use.Id] = 
    uses.toList map { case (i,u) ⇒ u.alias.v -> i } toMap

  lazy val root: StEnv = HQ root this

  def linkedSups: List[Sup.Id] =
    subElems(_ ⇒ Nil, c ⇒ List(c.supplier), b ⇒ List(b.supplier), _ ⇒ Nil)

  def linkedPros: List[Project.Id] =
    subElems(x ⇒ List(x.project),x ⇒ List(x.project),x ⇒ List(x.project),x ⇒ List(x.project))

  def linkedMets: List[Met.Id] =
    subElems(_ ⇒ Nil, _ ⇒ Nil, b ⇒ List(b.method), _ ⇒ Nil)

  def linkedStos: List[Sto.Id] =
    subElems(_ ⇒ Nil, c ⇒ List(c.location), _ ⇒ Nil, _ ⇒ Nil)
 
  def linkedUses: List[Use.Id] =
    pros.values.toList flatMap { p ⇒ p.owner.v :: p.users.v }

  def subElems[A](
    sub: SubS.Srv ⇒ List[A],
    con: ContainerS.Srv ⇒ List[A],
    bio: BiodataEntryS.Srv ⇒ List[A],
    fil: SubFilS.Srv ⇒ List[A],
  ): List[A] = {
    val ss = subs.toList map (_._2)
    val cs = ss flatMap (_.containers.toList map (_._2))
    val bs = cs flatMap (_.bio.toList map (_._2))
    val fs = ss.flatMap(_.files.toList map (_._2)) :::
             cs.flatMap(_.files.toList map (_._2)) :::
             bs.flatMap(_.files.toList map (_._2))

    ss.flatMap(sub) ::: cs.flatMap(con) :::
    bs.flatMap(bio) ::: fs.flatMap(fil)
  }


  //--------------------------------------------------------------------
  //                  Lists of Data Objects
  //--------------------------------------------------------------------

  lazy val metList: List[MetS.Srv] = mets.values.toList

  lazy val proList: List[ProjectS.Srv] = pros.values.toList

  lazy val supList: List[SupS.Srv] = sups.values.toList

  lazy val stoList: List[StoS.Srv] = stos.values.toList
}

object St {
  type Edit[A] = AuthEnv ⇒ St ⇒ DataE[(St,EditRes[A])]

  val L: Lens[St,St] = lens

  val empty: St = St(Map(),Map(),Map(),Map(), Map(), Map(), Map())

  implicit val eqI: cats.Eq[St] = cats.Eq.fromUniversalEquals

  lazy val defaultSettings: USettings = UserSettings(
    List(SubMol(DMol.Structure).ef,
         SubId.ef,
         SubName.ef,
         SubCasNr.ef,
         SubProject.ef,
         SubMol(DMol.Mass).ef,
         SubMol(DMol.Formula).ef,
         SubAbs.ef,
       ),

    List(SubMol(DMol.Structure).ef,
         SubId.ef,
         SubMol(DMol.Mass).ef,
         SubMol(DMol.LogP).ef,
         SubMol(DMol.Tpsa).ef,
       ),

    Map(), Map(), Map(), Map(), None, None, None, None, None, None, None, None
  )
}

// vim: set ts=2 sw=2 et:
