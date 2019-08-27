/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

import cats.implicits._

import cyby.query.{ContainsCI, Query, Q, Prim, Chain, Contains, Comp, QuickSearch}
import Comp.{Or, And}


/**
  * This module provides core data types plus functionality
  * shared between client and server. For a detailed introduction
  * about the concepts used for defining flexible data types,
  * see type Sup.
  *
  * @TODO: Look for redundant stuff and move to CyByEnv
  */
package object example {
  type USettings = UserSettings[ExportField,ExportField,Unit]
  type ZQuery = Query[ExportField,ExportField]
  type ZQ = Q[ExportField]
  type ESettings = cyby.export.Settings[ExportField,ExportField]

  def quick(q: QuickSearch): ZQ = quick(q.query, q.includeEmpty, q.caseSensitive)

  def quick(r: String, includeEmpty: Boolean, caseSensitive: Boolean): ZQ = {
    val pre = if (caseSensitive) Contains else ContainsCI
    val strs = r.split(" ").toList

    def csub(f: SubField): ZQ = Prim(ExportSub(f).f, s"$pre $r", false)
    def ccon(f: ConField): ZQ = Prim(ExportCon(f).f, s"$pre $r", false)
    def cbio(f: BioField): ZQ = Prim(ExportBio(f).f, s"$pre $r", false)

    lazy val sub: ZQ = Chain(List(
      csub(SubName),
      csub(SubProject),
    ) map (Or.c -> _))

    lazy val con: ZQ = Chain(List(
      ccon(ConSupplier),
      ccon(ConBatch),
      ccon(ConOrderNr),
      ccon(ConComment),
      ccon(ConLentTo),
      ccon(ConProject)
    ) map (Or.c -> _))

    lazy val bio: ZQ = Chain(List(
      cbio(BioSupplier),
      cbio(BioMethod),
      cbio(BioComment),
      cbio(BioProject)
    ) map (Or.c -> _))

    lazy val empty: ZQ = Prim(ExportCon(ConEmpty).f, s"false", false)

    def isId(s: String) = Read[Long].read(s).nonEmpty

    def isCas(s: String) = Read[CasNr].read(s).nonEmpty

    val query: ZQ =
      if (strs forall isId) Prim(ExportSub(SubId).f, strs mkString " ", false)
      else if (isCas(r)) Prim(ExportSub(SubCasNr).f, s"== ${r}", false)
      else Chain(List(Or.c -> sub, Or.c -> con, Or.c -> bio))

    if (includeEmpty) query
    else Chain(List(And.c -> empty, And.c -> query))
  }
}

// vim: set ts=2 sw=2 et:
