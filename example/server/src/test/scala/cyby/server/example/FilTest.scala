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

trait FilImplicits extends AuthUtil with EditArbs {
  val S = ConFilS
  val imps = new Implicits
}

object FilImplicits extends FilImplicits


class FilTest extends FilImplicits {
  import imps.{idArb ⇒ _, modArb ⇒ _, srvAddArb ⇒ _, _}
  implicit val smA: org.scalacheck.Arbitrary[S.Mod] = imps.modArb

  val mod: S.Mod = Fil[Option,Undef,Project.Id,Undef,Undef](None,None,None,None,None,None,None)

  //----------------------------------------------------------------------
  //                         Authorization
  //----------------------------------------------------------------------

  testProAuth(ConFilS, "Fil", "files")(ConFilS.au, _.id)


  //----------------------------------------------------------------------
  //                         Valdation
  //----------------------------------------------------------------------

  property("BR-Con-valid-1: new files with an existing path are invalid"){
    forAll{ (s: S.Srv, a: S.Add, si: Compound.Id, ci: Container.Id) ⇒
      val db  = Map(s.id -> s)
      val a2  = a.copy(path = s.path)
      val pp  = ci :: si :: hnil
      val err = FilExists(s.path.v, ConFilP(s.id :: pp))

      S.valid.add(pp -> db,a2) should contain(err)
    }
  }

  property("BR-Con-valid-2: changing a file's path to an existing one is invalid"){
    forAll{ (s: S.Srv, o: S.Srv, b: FileName, si: Compound.Id, ci: Container.Id) ⇒
      val pp  = ci :: si :: hnil
      val s2  = s.copy(path = Pure(b))
      val db  = Map(s2.id -> s2)
      val m   = lens[S.Mod].path.set(mod)(some(b))
      val err = FilExists(b, ConFilP(s.id :: pp))

      if (s.id =!= o.id) S.valid.mod(pp -> db,o,m) should contain(err)
      else S.valid.mod(pp -> db,o,m) shouldNot contain(err)
    }
  }

  //----------------------------------------------------------------------
  //                         Editing
  //----------------------------------------------------------------------

  property("BR-Fil-cud-1: new files are adjusted correctly"){
    forAll{ (is: Set[Fil.Id], s: S.Add, ei: EditInfo) ⇒
      val as = S.cud.doAdd(s)(ei -> is)
      is shouldNot contain(as.id)
      as.created shouldEq ei.timestamp
      as.modified shouldEq ei
    }
  }

  property("BR-Fil-cud-2: files are created correctly"){
    forAll{ s: S.SrvAdd ⇒ S.cud.toSrv(s) shouldEq s }
  }

  property("BR-Fil-cud-3: files modifications are adjusted correctly"){
    forAll{ (s: S.Mod, ei: EditInfo) ⇒
      S.cud.adjMod(ei, s).modified shouldEq ei
    }
  }

  property("BR-Fil-cud-4: files are modified correctly"){
    forAll{ (s: S.Srv, m: S.SrvMod) ⇒
      val s2 = S.cud.doMod(s,m)

      s2.id shouldEq s.id
      testMod(s2.name, s.name, m.name)
      testMod(s2.path, s.path, m.path)
      testMod(s2.comment, s.comment, m.comment)
      testMod(s2.project, s.project, m.project)
      s2.created shouldEq s.created
      s2.modified shouldEq m.modified
    }
  }

  //----------------------------------------------------------------------
  //                         Util
  //----------------------------------------------------------------------

  def est(st: St, s: CompoundS.Srv, c: ContainerS.Srv): S.EdSt =
    extract(S.edSt(st, c.id :: s.id :: hnil))

  property("ConFil edEnv") {
    forAll{ (s: CompoundS.Srv, c: ContainerS.Srv) ⇒
      val st = conSt(c,s)
      val e = PathNotFound(ConP(c.id.inc :: s.id :: hnil)).e

      est(st,s,c).node shouldEq c
      errs(S.edSt(st,c.id.inc::s.id::hnil)) shouldEq Nel.of(e)
    }
  }

  property("ConFil getSrv") {
    forAll{ (s: CompoundS.Srv, c: ContainerS.Srv, b: S.Srv) ⇒
      val st = conFilSt(b, c, s)
      val e = PathNotFound(ConFilP(b.id.inc :: c.id :: s.id :: hnil)).e

      extract(S.getSrv(b.id, est(st, s, c))) shouldEq b
      errs(S.getSrv(b.id.inc, est(st, s, c))) shouldEq Nel.of(e)
    }
  }

  //----------------------------------------------------------------------
  //                         Json
  //----------------------------------------------------------------------

  checkLaws("json", JsonLaws[S.LoadEd].fromToStripped)
}
