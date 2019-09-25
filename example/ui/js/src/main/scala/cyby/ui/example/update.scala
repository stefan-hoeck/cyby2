/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui
package example

import cats.implicits.{none ⇒ _, _}

import cyby.dat._
import cyby.dat.example._, SubTreeEd._

import io.circe.Json

import shapeless.{HNil,::}

import msf.js.UIEvent

trait EditZ extends cyby.ui.editor.EditEnv with CoreZ {

  lazy val hnilJson: HNil ⇒ Json ⇒ Json = _ ⇒ j ⇒ j

  object Edit extends Editor with DomZ[(St,Creds),IO[Unit],Option[UIEvent]] {
    def add(dt: DataType, p: Path)(env: Env) = ad(dt,p,env._2)

    def ad(dt: DataType, p: Path, c: Creds) = (dt,p) match {
      case (BioT,ConP(p)) ⇒ proEnvC(p).bioC.add(c,SubT,p)(bioJson)
      case (FilT,BioP(p)) ⇒ filAdd(p,proEnvB(p),c)(bioFilJson)
      case (ConT,SubP(p)) ⇒ proEnvS(p).conC.add(c,SubT,p)(conJson)
      case (FilT,ConP(p)) ⇒ filAdd(p,proEnvC(p),c)(conFilJson)
      case (ProT,RootP)   ⇒ proC.add(c,ProT,HNil)(hnilJson)
      case (MetT,RootP)   ⇒ metC.add(c,MetT,HNil)(hnilJson)
      case (StoT,RootP)   ⇒ stoC.add(c,StoT,HNil)(hnilJson)
      case (SubT,RootP)   ⇒ proEnv.subC.add(c,SubT,HNil)(subJson)
      case (FilT,SubP(p)) ⇒ filAdd(p,proEnvS(p),c)(subFilJson)
      case (SupT,RootP)   ⇒ supC.add(c,SupT,HNil)(hnilJson)
      case (UseT,RootP)   ⇒ useC.add(c,UseT,HNil)(hnilJson)
      case (dt,p)         ⇒ warnH(s"invalid datatpye, path combo: $dt, $p") *>
                            Src.srcNoneHtml[Load]
    }

    def delete(p: Path)(env: Env) = del(p, env._2)

    def edit(f: String, p: Path)(e: Env) = p match {
      case BioP(p@h::t)    ⇒ proEnvC(t).bioC.ed(e,SubT,t,h,f)(bioJson,fbioE(p))
      case BioFilP(p@h::t) ⇒ proEnvB(t).filC.ed(e,SubT,t,h,f)(bioFilJson,fbioFilE(p))
      case ConP(p@h::t)    ⇒ proEnvS(t).conC.ed(e,SubT,t,h,f)(conJson,fconE(p))
      case ConFilP(p@h::t) ⇒ proEnvC(t).filC.ed(e,SubT,t,h,f)(conFilJson,fconFilE(p))
      case ProP(p@h::t)    ⇒ proC.ed(e,ProT,t,h,f)(hnilJson,fpro(_)(h.to) map proE)
      case MetP(p@h::t)    ⇒ metC.ed(e,MetT,t,h,f)(hnilJson,fmet(_)(h) map metE)
      case StoP(p@h::t)    ⇒ stoC.ed(e,StoT,t,h,f)(hnilJson,fsto(_)(h) map stoE)
      case SubP(p@h::t)    ⇒ proEnv.subC.ed(e,SubT,t,h,f)(subJson, fsub(_)(h) map subE)
      case SubFilP(p@h::t) ⇒ proEnvS(t).filC.ed(e,SubT,t,h,f)(subFilJson,fsubFilE(p))
      case SupP(p@h::t)    ⇒ supC.ed(e,SupT,t,h,f)(hnilJson,fsup(_)(h) map supE)
      case UseP(p@h::t)    ⇒ useC.ed(e,UseT,t,h,f)(hnilJson,fuse(_)(h) map useE)
      case RootP           ⇒ warnH("tried to edit root!") *> Src.srcNoneHtml[Load]
    }

    def del(p: Path, c: Creds) = p match {
      case BioP(h::t)    ⇒ proEnvC(t).bioC.del(c,SubT,t,h)(bioJson)
      case BioFilP(h::t) ⇒ proEnvB(t).filC.del(c,SubT,t,h)(bioFilJson)
      case ConP(h::t)    ⇒ proEnvS(t).conC.del(c,SubT,t,h)(conJson)
      case ConFilP(h::t) ⇒ proEnvC(t).filC.del(c,SubT,t,h)(conFilJson)
      case ProP(h::t)    ⇒ proC.del(c,ProT,t,h)(hnilJson)
      case MetP(h::t)    ⇒ metC.del(c,MetT,t,h)(hnilJson)
      case StoP(h::t)    ⇒ stoC.del(c,StoT,t,h)(hnilJson)
      case SubP(h::t)    ⇒ proEnv.subC.del(c,SubT,t,h)(subJson)
      case SubFilP(h::t) ⇒ proEnvS(t).filC.del(c,SubT,t,h)(subFilJson)
      case SupP(h::t)    ⇒ supC.del(c,SupT,t,h)(hnilJson)
      case UseP(h::t)    ⇒ useC.del(c,UseT,t,h)(hnilJson)
      case RootP         ⇒ Src.srcNone[Load]
    }

    def clone(pth: Path): Eff[Option[Src[Option[Load]]]] = pth match {
      case ConP(p@h::t) ⇒ at(UId.CreateCont(ConT,SubP(t)))(
        prepare(UId.Create(ConT,SubP(t))) >>= {
          case (st,c) ⇒ proEnvS(t).conC.addO(c,SubT,t,fconE(p)(st))(conJson)
        }
      ) map some
      case _       ⇒ Eff pure none[Src[Option[Load]]]
    }

    def filAdd[B](b: B, p: ProEnv, cs: Creds)(f: B ⇒ Json ⇒ Json)
      : Html[Src[Option[Load]]] = p.filC.addFil(cs,FilT,b)(f)

    private lazy val metL = edLink(met(_).desc)(fmet, _.id)
    implicit lazy val metE: Editable[Link[Met.Id]] = Editable.wrapped(metL)
   
    private lazy val useL = edLink(use(_).desc)(fuse, _.id)
    implicit lazy val useE: Editable[Link[Use.Id]] = Editable.wrapped(useL)
    implicit lazy val usesE: Editable[Links[Use.Id]] = Editable.list(useL)
   
    private lazy val supL = edLink(sup(_).desc)(fsup, _.id)
    implicit lazy val supE: Editable[Link[Sup.Id]] = Editable.wrapped(supL)
   
    private lazy val stoL = edLink(sto(_).desc)(fsto, _.id)
    implicit lazy val stoE: Editable[Link[Sto.Id]] = Editable.wrapped(stoL)
    
    type BioE = BiodataEntry[Pure,Undef,Link[Met.Id],Link[Sup.Id],Link[Pro.AccId],Undef,Undef,Undef]
    type ConE = Con[Pure,Undef,Link[Sto.Id],Link[Sup.Id],Link[Pro.AccId],Undef,Undef,Undef,Undef]
    type FilE = Fil[Pure,Undef,Link[Pro.AccId],Undef,Undef]
    type MetE = Met[Pure,Undef,Undef,Undef]
    type ProE = Pro[Pure,Undef,Link[Use.Id],Undef,Undef]
    type StoE = Sto[Pure,Undef,Undef,Undef]
    type SubE = Sub[Pure,Undef,Mol,Link[Pro.AccId],Undef,Undef,Undef,Undef]
    type SupE = Sup[Pure,Undef,Undef,Undef]
    type UseE = cyby.dat.example.Use[Pure,Undef,Option[Password],Undef,Undef]

    def bioE(b: BiodataEntry.Cli): BioE = b.copy(id = undef, files = undef, created = undef, modified = undef)
    def conE(c: Con.Cli): ConE = c.copy(id = undef, bio = undef, files = undef, created = undef, modified = undef)
    def filE(f: Fil.Cli): FilE = f.copy(id = undef, created = undef, modified = undef)
    def metE(m: Met.Cli): MetE = m.copy(id = undef, created = undef, modified = undef)
    def proE(p: Pro.Cli): ProE = p.copy(id = undef, created = undef, modified = undef)
    def stoE(s: Sto.Cli): StoE = s.copy(id = undef, created = undef, modified = undef)
    def subE(s: Sub.Cli): SubE = s.copy(id = undef, containers = undef, files = undef, created = undef, modified = undef)
    def supE(s: Sup.Cli): SupE = s.copy(id = undef, created = undef, modified = undef)
    def useE(u: cyby.dat.example.Use.Cli): UseE = u.copy(id = undef, password = None, created = undef, modified = undef)

    lazy val fsubFilE: Sub.FilPath ⇒ St ⇒ Option[FilE] =
      p ⇒ s ⇒ fsubFil(s)(p) map (p ⇒ filE(p._1))

    lazy val fconFilE: Con.FilPath ⇒ St ⇒ Option[FilE] =
      p ⇒ s ⇒ fconFil(s)(p) map (p ⇒ filE(p._1))

    lazy val fbioFilE: BiodataEntry.FilPath ⇒ St ⇒ Option[FilE] =
      p ⇒ s ⇒ fbioFil(s)(p) map (p ⇒ filE(p._1))

    lazy val fbioE: BiodataEntry.Path ⇒ St ⇒ Option[BioE] =
      p ⇒ s ⇒ fbio(s)(p) map (p ⇒ bioE(p._1))

    lazy val fconE: Con.Path ⇒ St ⇒ Option[ConE] =
      p ⇒ s ⇒ fcon(s)(p) map (p ⇒ conE(p._1))

    lazy val metC = Createable[MetE]
    lazy val proC = Createable[ProE]
    lazy val stoC = Createable[StoE]
    lazy val supC = Createable[SupE]
    lazy val useC = Createable[UseE]

    case class ProEnv(p: St ⇒ Option[Pro.Cli]) {
      private lazy val proL = edLink(st ⇒ proDef(p(st))(st).desc)(fpro, _.id)
      implicit lazy val proE: Editable[Link[Pro.AccId]] = Editable.wrapped(proL)
      implicit lazy val prosE: Editable[Nel[Link[Pro.AccId]]] = Editable.nel(proL)

      lazy val bioC = Createable[BioE]
      lazy val conC = Createable[ConE]
      lazy val filC = Createable[FilE]
      lazy val subC = Createable[SubE]
    }

    def proEnv[B](f: St ⇒ Option[B])(ps: B ⇒ Link[Pro.AccId]): ProEnv =
      ProEnv(st ⇒ f(st) flatMap (b ⇒ fpro(st)(ps(b)._1)))

    def proEnvB(p: BiodataEntry.Path): ProEnv = proEnv(fbio(_)(p))(_._1.project.v)
    def proEnvC(p: Con.Path): ProEnv = proEnv(fcon(_)(p))(_._1.project.v)
    def proEnvS(p: Sub.Path): ProEnv = proEnv(fsub(_)(p.head))(_.project.v)

    def proEnv: ProEnv = ProEnv(_ ⇒ None)
  }
}

// vim: set ts=2 sw=2 et:
