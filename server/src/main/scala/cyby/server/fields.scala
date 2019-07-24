/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server

import cats.implicits._

import cyby.dat.{Mol ⇒ DMol, _}
import cyby.chem.Mol

import cyby.query.{ReadPred ⇒ RP, Pred}

import cyby.server.export.{OfficeValueType ⇒ OVT, date}

/**
  * Utility functions and types for working with
  * field accessors.
  *
  * Combined queries and exported values are defined using
  * enumerations of "fields" of data objects. A list of
  * "field" objects for instances defines what to include
  * when exporting the results of a query to a csv file
  * for instance.
  */
trait fields extends ServerEnv {
  /**
    * Encapsulates function used during exporting, sorting,
    * and querying.
    */
  case class Act[A,B](
    odf:  A ⇒ String,
    txt:  A ⇒ String,
    sdf:  A ⇒ String,
    que:  St ⇒ RP[B],
    sort: List[A] ⇒ List[A],
  ){
    def setSort(f: List[A] ⇒ List[A]): Act[A,B] = copy(sort = f)

    def setQue(f: St ⇒ RP[B]): Act[A,B] = copy(que = f)

    def setSdf(f: A ⇒ String): Act[A,B] = copy(sdf = f)
  }

  def dummy[A,B]: Act[A,B] = Act(_ ⇒ OVT.empty, _ ⇒ "", _ ⇒ "", _ ⇒ RP.always, identity)

  def created(ts: TimeStamp): String = date(ts.v)

  lazy val editInfoSummary: EditInfo ⇒ String = ei ⇒
    s"""${date(ei.timestamp.v)} (${ei.name.fold("")(_.v)})"""

  def mol[A,B](f: DMol.Field)(g: A ⇒ Option[DMol], h: B ⇒ Option[Mol]): Act[A,B] = f match {
    case DMol.Structure      ⇒ stringO[A,B](g(_) map (_.inchi), h(_) map (_.inchi))
                                 .setQue(_ ⇒ RP.always)
    case DMol.ExactStructure ⇒ dummy[A,B].setQue(_ ⇒ RP.cmap(exactStructureP)(h))
    case DMol.SubStructure   ⇒ dummy[A,B].setQue(_ ⇒ RP.cmap(subStructureP)(h))
    case DMol.NoStructure    ⇒ dummy[A,B].setQue(_ ⇒ _ ⇒ some(h(_).isEmpty))
    case DMol.Svg            ⇒ dummy
    case DMol.Inchi          ⇒ stringO(g(_) map (_.inchi), h(_) map (_.inchi))
    case DMol.Smiles         ⇒ stringO(g(_) >>= (_.smiles), h(_) >>= (_.smiles))
    case DMol.Mass           ⇒ doubleO(g(_) map (_.mass), h(_) map (_.mass))
    case DMol.ExactMass      ⇒ doubleO(g(_) map (_.exactMass), h(_) map (_.exactMass))
    case DMol.Formula        ⇒ stringO[A,B](g(_) map (_.formulaStr), h(_) map (_.formulaStr))
                                 .setSort(_ sortBy (g(_) map (_.formula)))
    case DMol.LogP           ⇒ doubleO(g(_) >>= (_.logP), h(_) >>= (_.logP))
    case DMol.Tpsa           ⇒ doubleO(g(_) >>= (_.tpsa), h(_) >>= (_.tpsa))
    case DMol.Lipinski       ⇒ boolO(g(_) >>= (_.lipinski), h(_) >>= (_.lipinski))
  }

  private val subStructureP: RP[Option[Mol]] = s ⇒ 
    (chem readForQuery s).toOption.map{
      case (m,fp) ⇒ (m2: Option[Mol]) ⇒ m2.exists (_.hasSubgraph(m, fp))
    }

  private val exactStructureP: RP[Option[Mol]] = s ⇒ 
    Mol.read(s).map(m ⇒ (m2: Option[Mol]) ⇒ m2.exists(_.inchi === m.inchi))


  def edit[A,B](f: EditInfo.Field)(g: A ⇒ EditInfo, h: B ⇒ EditInfo): Act[A,B] = f match {
    case EditInfo.Timestamp ⇒ timestamp(g(_).timestamp, h(_).timestamp)
    case EditInfo.UserId    ⇒ long(g(_).id, h(_).id)
    case EditInfo.UserName  ⇒ Act(
      a ⇒ OVT.fOpt(g(a).name)(n ⇒ OVT fString n.v),
      g(_).name.fold("")(_.v),
      g(_).name.fold("")(_.v),
      _ ⇒ RP.long(h(_).id),
      _.sortBy(g(_).name)
    )

    case EditInfo.Summary   ⇒ Act(
      a ⇒ OVT fString editInfoSummary(g(a)),
      a ⇒ editInfoSummary(g(a)),
      a ⇒ editInfoSummary(g(a)),
      _ ⇒ RP.always,
      _ sortBy(g(_).timestamp)
    )
  }

  def string[A,B](g: A ⇒ String, h: B ⇒ String): Act[A,B] = Act(
    a ⇒ OVT fString g(a), g, g, _ ⇒ RP string h, _ sortBy g
  )

  def stringO[A,B](g: A ⇒ Option[String], h: B ⇒ Option[String]): Act[A,B] = Act(
    a ⇒ OVT.fOpt(g(a))(OVT.fString), g(_) getOrElse "", g(_) getOrElse "",
    _ ⇒ RP stringO h, _ sortBy g
  )

  def id[A,B,K](g: A ⇒ Id[K], h: B ⇒ Id[K]): Act[A,B] = Act(
    a ⇒ OVT fLong g(a).v, g(_).toString, g(_).toString, _ ⇒ RP id h, _ sortBy g
  )

  def bool[A,B](g: A ⇒ Boolean, h: B ⇒ Boolean): Act[A,B] = Act(
    a ⇒ OVT fBool g(a), g(_).toString, g(_).toString, _ ⇒ RP bool h, _ sortBy g
  )

  def boolO[A,B](g: A ⇒ Option[Boolean], h: B ⇒ Option[Boolean]): Act[A,B] = Act(
    a ⇒ OVT.fOpt(g(a))(OVT.fBool), g(_).fold("")(_.toString), g(_).fold("")(_.toString),
    _ ⇒ RP boolO h, _ sortBy g
  )

  def doubleO[A,B](g: A ⇒ Option[Double], h: B ⇒ Option[Double]): Act[A,B] = Act(
    a ⇒ OVT.fOpt(g(a))(OVT.fDouble), g(_).fold("")(_.toString), g(_).fold("")(_.toString),
    _ ⇒ RP doubleO h, _ sortBy g
  )

  def listOf[A,B,V:cats.Order](g: A ⇒ List[V], h: B ⇒ List[V])
    (disp: V ⇒ String, rp: RP[V]): Act[A,B] = Act(
      a ⇒ OVT fString g(a).map(disp).mkString(", "),
      g(_).map(disp).mkString(", "),
      g(_).map(disp).mkString(", "),
      _ ⇒ RP.cmap(RP.list(rp))(h),
      _ sortBy g
    )

  def long[A,B](g: A ⇒ Long, h: B ⇒ Long): Act[A,B] = Act(
    a ⇒ OVT fLong g(a), g(_).toString, g(_).toString, _ ⇒ RP long h, _ sortBy g
  )

  def double[A,B](g: A ⇒ Double, h: B ⇒ Double): Act[A,B] = Act(
    a ⇒ OVT fDouble g(a), g(_).toString, g(_).toString, _ ⇒ RP double h, _ sortBy g
  )

  def timestamp[A,B](g: A ⇒ TimeStamp, h: B ⇒ TimeStamp): Act[A,B] = Act(
    a ⇒ OVT created g(a), a ⇒ created(g(a)), a ⇒ created(g(a)), _ ⇒ RP long (h(_).v), _ sortBy g
  )

  def dateAct[A,B](g: A ⇒ Date, h: B ⇒ Date): Act[A,B] = Act(
    a ⇒ OVT fDate g(a), a ⇒ date(g(a).v), a ⇒ date(g(a).v), _ ⇒ RP long (h(_).v), _ sortBy g
  )

  def link[A,B,V,K](g: A ⇒ Link[K], h: B ⇒ K, vs: St ⇒ List[V])
    (n: V ⇒ Name, k: V ⇒ K, rpk: RP[K]): Act[A,B] = Act(
      a ⇒ OVT link g(a), g(_)._2.v,
      g(_)._2.v,
      st ⇒ s ⇒ RP.cmap(rpk)(h)(s) orElse idPred(vs(st))(k, n, h)(s),
      _ sortBy (g(_)._2.v)
    )

  def links[A,B,V,K](g: A ⇒ List[Link[K]], h: B ⇒ List[K], vs: St ⇒ List[V])
    (n: V ⇒ Name, k: V ⇒ K, rpk: RP[K]): Act[A,B] = Act(
      a ⇒ OVT links g(a),
      g(_).map(_._2.v).mkString(", "),
      g(_).map(_._2.v).mkString(", "),
      st ⇒ s ⇒ RP.cmap(RP.list(rpk))(h)(s) orElse idsPred(vs(st))(k, n, h)(s),
      _ sortBy (g(_).map(_._2.v))
    )

  def linksNel[A,B,V,K](g: A ⇒ Nel[Link[K]], h: B ⇒ Nel[K], vs: St ⇒ List[V])
    (n: V ⇒ Name, k: V ⇒ K, rpk: RP[K]): Act[A,B] =
    links[A,B,V,K](g(_).toList, h(_).toList, vs)(n,k,rpk)

  private def idPred_[K,V](vs: List[V])(get: V ⇒ K, name: V ⇒ Name): RP[K] =
    RP.string_ apply _ map {p ⇒
      val ks = vs filter (v ⇒ p(name(v).v)) map get toSet

      ks : Pred[K]
    }

  private def idPred[A,K,V](vs: List[V])(get: V ⇒ K, name: V ⇒ Name, id: A ⇒ K): RP[A] =
    RP.cmap(idPred_(vs)(get, name))(id)

  private def idsPred[A,K,V](vs: List[V])(get: V ⇒ K, name: V ⇒ Name, ids: A ⇒ List[K]): RP[A] =
    RP.cmap(RP.list(idPred_(vs)(get, name)))(ids)

}
