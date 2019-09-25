/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

import Maybe.{maybe, nothing}
import tagInstances._
import cats.implicits.{none ⇒ _, _}, cyby.shapelessImplicits._
import cyby.chem.Mol
import cyby.dat.{Mol ⇒ _, _}, example._
import org.scalacheck.{Arbitrary,Gen}

trait CpdImplicits extends AuthUtil with EditArbs {
  val S = CompoundS
  val imps = new Implicits

  lazy val subP: Gen[Compound.Path] = arb[Compound.Id] map (_ :: hnil)
  lazy val conP: Gen[Container.Path] = (arb[Container.Id],subP).mapN(_ :: _)
  lazy val bioP: Gen[BiodataEntry.Path] = (arb[BiodataEntry.Id],conP).mapN(_ :: _)

  implicit lazy val treeArb: Arbitrary[CpdTreeL] = Arbitrary(Gen.frequency(
    5 -> imps.loadArb.arbitrary.map(CpdEdit(_) : CpdTreeL),
    5 -> (subP,ConImplicits.imps.loadArb.arbitrary).mapN(ConEdit(_,_) : CpdTreeL),
    2 -> (conP,BioImplicits.imps.loadArb.arbitrary).mapN(BioEdit(_,_) : CpdTreeL),
    1 -> (subP,FilImplicits.imps.loadArb.arbitrary).mapN(CpdFilEdit(_,_) : CpdTreeL),
    1 -> (conP,FilImplicits.imps.loadArb.arbitrary).mapN(ConFilEdit(_,_) : CpdTreeL),
    1 -> (bioP,FilImplicits.imps.loadArb.arbitrary).mapN(BioFilEdit(_,_) : CpdTreeL),
  ))

}

class CpdTest extends CpdImplicits {
  import imps.{idArb ⇒ _, modArb ⇒ _, _}
  implicit val smA: org.scalacheck.Arbitrary[S.Mod] = imps.modArb

  val mod: S.Mod = Compound[Option,Undef,Mol,Project.Id,Undef,Undef,Undef,Undef](None,None,None,None,None,None,None,None,None,None)

  //----------------------------------------------------------------------
  //                         Authorization
  //----------------------------------------------------------------------

  testProAuth(CompoundS, "Cpd", "substances")(CompoundS.au, _.id)

  //----------------------------------------------------------------------
  //                         Valdation
  //----------------------------------------------------------------------

  property("BR-Cpd-valid-1: new substances with an existing name are invalid"){
    forAll{ (s: S.Srv, a: S.Add) ⇒
      val db  = Map(s.id -> s)
      val a2  = a.copy(name = s.name)

      if (s.name.v.v.nonEmpty) {
        val err = CpdExists(s.name.v, s.id :: hnil)
        S.valid.add(db,a2) should contain(err)
      }
    }
  }

  property("BR-Cpd-valid-2: new substances with an existing cas-nr are invalid"){
    forAll{ (s: S.Srv, a: S.Add) ⇒
      val db = Map(s.id -> s)
      val a2 = a.copy(casNr = s.casNr)

      if (s.casNr.v.v.nonEmpty) {
        val err = CasNrExists(s.casNr.v, s.id :: hnil)
        S.valid.add(db,a2) should contain(err)
      }
    }
  }

  property("BR-Cpd-valid-3: new substances with an existing structure and abs flag are invalid"){
    forAll{ (s: S.Srv, a: S.Add) ⇒
      val db = Map(s.id -> s)
      val a2 = a.copy(structure = s.structure, abs = s.abs)

      if (s.structure.v.nonEmpty) {
        val err = StructureExists(s.structure.v.get.toDatMol, s.id :: hnil)
        S.valid.add(db,a2) should contain(err)
      }
    }
  }

  property("BR-Cpd-valid-4: new substances with an existing structure but distinct abs flag are valid"){
    forAll{ (s: S.Srv, a: S.Add) ⇒
      val db = Map(s.id -> s)
      val a2 = a.copy(structure = s.structure, abs = Pure(!s.abs))

      if (s.structure.v.nonEmpty) {
        val err = StructureExists(s.structure.v.get.toDatMol, s.id :: hnil)
        S.valid.add(db,a2) shouldNot contain(err)
      }
    }
  }

  property("BR-Cpd-valid-5: new substances with undefined structure and name are invalid"){
    forAll{ a: S.Add ⇒
      if (a.structure.v.isEmpty && a.name.v.v.isEmpty)
        S.valid.add(Map(),a) should contain(EmptyStructure)
    }
  }

  property("BR-Cpd-valid-6: new substances with new name, casNr, and structure are valid"){
    forAll{ a: S.Add ⇒
      if (a.structure.v.nonEmpty || a.name.v.v.nonEmpty)
        S.valid.add(Map(),a) shouldEq Nil
    }
  }

  property("BR-Cpd-valid-7: changing a substance's name to an existing one is invalid"){
    forAll{ (s: S.Srv, o: S.Srv, p: Plain) ⇒
      val s2  = s.copy(name = Pure(p))
      val db  = Map(s2.id -> s2)
      val m   = lens[S.Mod].name.set(mod)(some(p))
      val err = CpdExists(p, s.id :: hnil)

      if ((s.id =!= o.id) && p.v.nonEmpty) S.valid.mod(db,o,m) should contain(err)
      else S.valid.mod(db,o,m) shouldNot contain(err)
    }
  }

  property("BR-Cpd-valid-8: changing a substance's casNr to an existing one is invalid"){
    forAll{ (s: S.Srv, o: S.Srv, n: CasNr) ⇒
      val s2  = s.copy(casNr = Pure(n))
      val db  = Map(s2.id -> s2)
      val m   = lens[S.Mod].casNr.set(mod)(some(n))
      val err = CasNrExists(n, s.id :: hnil)

      if ((s.id =!= o.id) && n.v.nonEmpty) S.valid.mod(db,o,m) should contain(err)
      else S.valid.mod(db,o,m) shouldNot contain(err)
    }
  }

  property("BR-Cpd-valid-9: changing a substance's structure to an existing one is invalid"){
    forAll{ (s: S.Srv, o: S.Srv, n: Mol) ⇒
      val s2  = s.copy(structure = Pure(maybe(n)), abs = o.abs)
      val db  = Map(s2.id -> s2)
      val m   = lens[S.Mod].structure.set(mod)(some(maybe(n)))
      val err = StructureExists(n.toDatMol, s.id :: hnil)

      if (s.id =!= o.id) S.valid.mod(db,o,m) should contain(err)
      else S.valid.mod(db,o,m) shouldNot contain(err)
    }
  }

  property("BR-Cpd-valid-10: emptying a substance's name and structure is invalid"){
    forAll{ s: S.Srv ⇒
      val db  = Map(s.id -> s)
      val m   = lens[S.Mod].name.set(
                  lens[S.Mod].structure.set(mod)(some(nothing[Mol]))
                )(some(Plain.default))
      val err = EmptyStructure

      S.valid.mod(db,s,m) should contain(err)
    }
  }


  //----------------------------------------------------------------------
  //                         Editing
  //----------------------------------------------------------------------

  property("BR-Cpd-cud-1: new substances are adjusted correctly"){
    forAll{ (is: Set[Compound.Id], s: S.Add, ei: EditInfo) ⇒
      val as = S.cud.doAdd(s)(ei -> is)
      is shouldNot contain(as.id)
      as.created shouldEq ei.timestamp
      as.modified shouldEq ei
    }
  }

  property("BR-Cpd-cud-2: substances are created correctly"){
    forAll{ s: S.SrvAdd ⇒
      S.cud.toSrv(s).containers.v shouldEq Map()
      S.cud.toSrv(s).files.v shouldEq Map()
    }
  }

  property("BR-Cpd-cud-3: substance modifications are adjusted correctly"){
    forAll{ (s: S.Mod, ei: EditInfo) ⇒
      S.cud.adjMod(ei, s).modified shouldEq ei
    }
  }

  property("BR-Cpd-cud-4: substances are modified correctly"){
    forAll{ (s: S.Srv, m: S.SrvMod) ⇒
      val s2 = S.cud.doMod(s,m)

      s2.id shouldEq s.id
      testMod(s2.name, s.name, m.name)
      testMod(s2.structure, s.structure, m.structure)
      testMod(s2.abs, s.abs, m.abs)
      testMod(s2.casNr, s.casNr, m.casNr)
      testMod(s2.project, s.project, m.project)
      s2.containers shouldEq s.containers
      s2.files shouldEq s.files
      s2.created shouldEq s.created
      s2.modified shouldEq m.modified
    }
  }


  //----------------------------------------------------------------------
  //                         Util
  //----------------------------------------------------------------------

  def est(st: St): S.EdSt = extract(S.edSt(st, hnil))

  property("CompoundS edEnv") { est(St.empty).nodes shouldEq (St.empty :: hnil) }

  property("CompoundS getSrv") {
    forAll{ s: S.Srv ⇒
      val e = PathNotFound(CpdP(s.id.inc :: hnil)).e
      val st = St.empty.copy(subs = mkDB(s)(_.id))

      extract(S.getSrv(s.id, est(st))) shouldEq s
      errs(S.getSrv(s.id.inc, est(st))) shouldEq Nel.of(e)
    }
  }

  //----------------------------------------------------------------------
  //                         Json
  //----------------------------------------------------------------------

  checkLaws("json", JsonLaws[S.LoadEd].fromToStripped)

  checkLaws("edit json", JsonLaws[CpdTreeL].fromToStripped)
}
