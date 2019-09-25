/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

import tagInstances._
import cats.implicits.{none ⇒ _, _}
import cyby.dat.{Mol ⇒ _, _}, example._

trait BioImplicits extends AuthUtil with EditArbs {
  val S = BiodataEntryS
  val imps = new Implicits
}

object BioImplicits extends BioImplicits


class BioTest extends BioImplicits {
  import imps.{idArb ⇒ _, modArb ⇒ _, _}
  implicit val smA: org.scalacheck.Arbitrary[S.Mod] = imps.modArb

  val mod: S.Mod = BiodataEntry[Option,Undef,Met.Id,Sup.Id,Pro.Id,Undef,Undef,Undef](None,None,None,None,None,None,None,None,None,None)

  //----------------------------------------------------------------------
  //                         Authorization
  //----------------------------------------------------------------------

  testProAuth(BiodataEntryS, "Bio", "biodata")(BiodataEntryS.au, _.id)

  //----------------------------------------------------------------------
  //                         Editing
  //----------------------------------------------------------------------

  property("BR-Bio-cud-1: new biodata are adjusted correctly"){
    forAll{ (is: Set[BiodataEntry.Id], s: S.Add, ei: EditInfo) ⇒
      val as = S.cud.doAdd(s)(ei -> is)
      is shouldNot contain(as.id)
      as.created shouldEq ei.timestamp
      as.modified shouldEq ei
    }
  }

  property("BR-Bio-cud-2: biodata are created correctly"){
    forAll{ s: S.SrvAdd ⇒ S.cud.toSrv(s).files.v shouldEq Map() }
  }

  property("BR-Bio-cud-3: biodata modifications are adjusted correctly"){
    forAll{ (s: S.Mod, ei: EditInfo) ⇒
      S.cud.adjMod(ei, s).modified shouldEq ei
    }
  }

  property("BR-Bio-cud-4: biodata are modified correctly"){
    forAll{ (s: S.Srv, m: S.SrvMod) ⇒
      val s2 = S.cud.doMod(s,m)

      s2.id shouldEq s.id
      testMod(s2.value, s.value, m.value)
      testMod(s2.method, s.method, m.method)
      testMod(s2.supplier, s.supplier, m.supplier)
      testMod(s2.date, s.date, m.date)
      testMod(s2.comment, s.comment, m.comment)
      testMod(s2.project, s.project, m.project)
      s2.files shouldEq s.files
      s2.created shouldEq s.created
      s2.modified shouldEq m.modified
    }
  }


  //----------------------------------------------------------------------
  //                         Util
  //----------------------------------------------------------------------

  def est(st: St, s: SubS.Srv, c: ContainerS.Srv): S.EdSt =
    extract(S.edSt(st, c.id :: s.id :: hnil))

  property("BiodataEntryS edEnv") {
    forAll{ (s: SubS.Srv, c: ContainerS.Srv) ⇒
      val st = conSt(c,s)
      val e = PathNotFound(ConP(c.id.inc :: s.id :: hnil)).e

      est(st,s,c).node shouldEq c
      errs(S.edSt(st,c.id.inc::s.id::hnil)) shouldEq Nel.of(e)
    }
  }

  property("BiodataEntryS getSrv") {
    forAll{ (s: SubS.Srv, c: ContainerS.Srv, b: S.Srv) ⇒
      val st = bioSt(b, c, s)
      val e = PathNotFound(BioP(b.id.inc :: c.id :: s.id :: hnil)).e

      extract(S.getSrv(b.id, est(st, s, c))) shouldEq b
      errs(S.getSrv(b.id.inc, est(st, s, c))) shouldEq Nel.of(e)
    }
  }

  //----------------------------------------------------------------------
  //                         Json
  //----------------------------------------------------------------------

  checkLaws("json", JsonLaws[S.LoadEd].fromToStripped)
}
