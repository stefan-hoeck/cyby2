/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui
package example

import cats.implicits._
import cyby.chem.Mol
import cyby.dat.{Mol ⇒ DMol, _}, cyby.dat.example._
import cyby.dat.format.{Color, Gradient}
import cyby.export.{Sdf}
import cyby.ui.{WidgetType ⇒ WT, CompType ⇒ CT}

import shapeless.HNil

trait util extends DispZShared with DocEnv {
    
  def subField(p: SubField ⇒ Boolean, f: SubField, mod: WT ⇒ WT): String =
    selectEnumP(p)(mod(WT.Field2Sel), _ locName loc, f)
    
  def conField(p: ConField ⇒ Boolean, f: ConField, mod: WT ⇒ WT): String =
    selectEnumP(p)(mod(WT.Field2Sel), _ locName loc, f)
    
  def bioField(p: BioField ⇒ Boolean, f: BioField, mod: WT ⇒ WT): String =
    selectEnumP(p)(mod(WT.Field2Sel), _ locName loc, f)
    
  def dataType(p: DataType ⇒ Boolean, d: DataType, mod: WT ⇒ WT): String =
    selectEnumP(p)(mod(WT.FieldSel), _ locName loc, d)
    
  def dataTypeQ(d: DataType): String = dataType(_.canQuery, d, WT.Query)
    
  def dataTypeE(d: DataType): String = dataType(_.canExport, d, WT.Export)
    
  def subQ(s: SubField): String = subField(_.canQuery, s, WT.Query)
    
  def conQ(c: ConField): String = conField(_.canQuery, c, WT.Query)
  
  def expDescP(
    ef:    ExportField,
    expP:  DataType ⇒ Boolean,
    subP:  SubField ⇒ Boolean,
    conP:  ConField ⇒ Boolean,
    bioP:  BioField ⇒ Boolean,
    mod:   WT ⇒ WT,
    ct:    CompType,
  ): String = {
    def dt(d: DataType) = dataType(expP, d, mod)
  
    def stats(m: Met.Id, s: StatsType): String =
      metMod(mets.find(_.id === m).get, mod) ++ statsType(s, mod)

    def wrap(s: String) = Txt.div(Txt.cls := ct.c)(s)

    ef match {
      case ExportSub(f)     ⇒ dt(SubT)   ++ wrap(subField(subP, f, mod))
      case ExportCon(f)     ⇒ dt(ConT)   ++ wrap(conField(conP, f, mod))
      case ExportBio(f)     ⇒ dt(BioT)   ++ wrap(bioField(bioP, f, mod))
      case ExportStats(m,t) ⇒ dt(StatsT) ++ wrap(stats(m,t))
    }
  }

  def exportF(f: ExportField): String = expDescP(f, _.canExport,
    _.canExport, _.canExport, _.canExport, WT.Export, CT.ExportDetailRow)

  def queryF(f: ExportField): String = expDescP(f, _.canQuery,
    _.canQuery, _.canQuery, _.canQuery, WT.Query, CT.QueryDetailRow)

  def columnF(f: ExportField, mode: ExpMode): String = mode match {
    case MethodTable ⇒ expDescP(f, _.inStatsTable, _.inColumn,
      _.inColumn, _.inColumn, WT.Export, CT.ExportDetailRow)
    case _ ⇒ expDescP(f, _.inSubTable, _.inColumn, _.inColumn,
      _.inColumn, WT.Export, CT.ExportDetailRow)
  }

  val mets = List(mmp10, mmp13)

  def loc(s: Sto.Cli) = linksA(List(rt107, rt109), s, WT.Query)(_.name.v.v)

  def met(m: Met.Cli) = metMod(m, WT.Query)

  def metMod(m: Met.Cli, f: WT ⇒ WT) = linksA(mets, m, f)(_.name.v.v)

  //----------------------------------------------------------------------
  //                      Example Data
  //----------------------------------------------------------------------

  def accId[A](id: Id[A]): Id[A @@ HasAccess] = id.to

  def unaccId[A](id: Id[A @@ HasAccess]): Id[A] = id.to

  def alias(s: String): Alias = Alias unsafe s

  def aliasP(s: String): Pure[Alias] = Pure(alias(s))

  def fileName(s: String): FileName = FileName unsafe s

  def fileNameP(s: String): Pure[FileName] = Pure(fileName(s))

  def name(s: String): Name = Name unsafe s

  def nameP(s: String): Pure[Name] = Pure(name(s))

  def plain(s: String): Plain = Plain unsafe s

  def plainP(s: String): Pure[Plain] = Pure(plain(s))

  lazy val ts: TimeStamp = TimeStamp mk 0L

  lazy val ei: EditInfo = EditInfo(ts, 5L, Some(name("hock")))

  //----------------------------------------------------------------------
  //                      Bio
  //----------------------------------------------------------------------

  def bio(met: Met.Cli, p: (Double, Int)): Bio.Cli = Bio(
    Id(p._2),
    Pure(p._1 / 100D),
    metL(met),
    supL(sup3),
    Pure(Date mk 0L),
    plainP(""),
    proL(pro2),
    Nil,
    ts, ei
  )

  def bioMMP10(p: (Double, Int)): Bio.Cli = bio(mmp10, p)

  def bioMMP13(p: (Double, Int)): Bio.Cli = bio(mmp13, p)

  lazy val bio1: Bio.Cli = bioMMP10(1.25 -> 1)

  lazy val bio2: Bio.Cli = bioMMP13(10.17 -> 2)

  //----------------------------------------------------------------------
  //                      Containers
  //----------------------------------------------------------------------

  lazy val con1: Con.Cli = Con(
    Id(1),
    Pure(stoL(rt107)),
    supL(sup1),
    plainP(""),
    plainP("112466-2500ML"),
    plainP(""),
    plainP(""),
    Pure(Percent unsafe 99.5D),
    plainP("puriss, p.a."),
    Pure(Density unsafe 0.789D),
    Pure(Concentration.default),
    Pure(Amount unsafe 2500),
    Pure(false),
    proL(pro1),
    Nil,
    Nil,
    ts, ei
  )

  lazy val con2: Con.Cli = Con(
    Id(1),
    Pure(stoL(rt109)),
    supL(sup1),
    plainP(""),
    plainP("112468-1000ML"),
    plainP(""),
    plainP(""),
    Pure(Percent unsafe 95D),
    plainP("purum"),
    Pure(Density unsafe 0.789D),
    Pure(Concentration.default),
    Pure(Amount unsafe 1000),
    Pure(false),
    proL(pro1),
    Nil,
    Nil,
    ts, ei
  )

  def statsCon(
    i:     Long,
    btch: String,
    mmp10: List[Double],
    mmp13: List[Double],
  ): Con.Cli = con3.copy(
    id = Id(i),
    batch = plainP(btch),
    bio   = mmp10.zipWithIndex.map(bioMMP10) :::
            mmp13.zipWithIndex.map(bioMMP13)
  )

  lazy val con3: Con.Cli = Con(
    Id(3),
    Pure(stoL(rt109)),
    supL(sup3),
    plainP("HO-12-1-1"),
    plainP(""),
    plainP(""),
    plainP(""),
    Pure(Percent unsafe 96.0D),
    plainP("NMR Assay"),
    Pure(Density.default),
    Pure(Concentration.default),
    Pure(Amount unsafe 1),
    Pure(false),
    proL(pro2),
    List(bio1, bio2),
    Nil,
    ts, ei
  )

  //----------------------------------------------------------------------
  //                      Files
  //----------------------------------------------------------------------

  lazy val fil1: Fil.Cli = Fil(
    Id(1),
    nameP("a file"),
    fileNameP("file_path.pdf"),
    plainP("a comment"),
    proL(pro1),
    ts, ei
  )

  lazy val fil2: Fil.Cli = Fil(
    Id(2),
    nameP("another file"),
    fileNameP("file_path2.pdf"),
    plainP(""),
    proL(pro2),
    ts, ei
  )

  //----------------------------------------------------------------------
  //                      Methods
  //----------------------------------------------------------------------

  def metL(m: Met.Cli): Pure[Link[Met.Id]] = Pure(m.id -> m.name)

  lazy val mmp10: Met.Cli =
    Met(Id(1), nameP("MMP-10 IC50"), plainP("MMP-10 IC50 inhibition"), ts, ei)

  lazy val mmp13: Met.Cli =
    Met(Id(2), nameP("MMP-13 IC50"), plainP("MMP-13 IC50 inhibition"), ts, ei)

  //----------------------------------------------------------------------
  //                      Projects
  //----------------------------------------------------------------------

  def proL(p: Pro.Cli): Pure[Link[Pro.AccId]] = Pure(p.id -> p.name)

  lazy val pro1: Pro.Cli = Pro(
    Id(1),
    nameP("Allgemeine Chemikalien"),
    useL(hock),
    usesL(jodo,pott),
    plainP("research chemicals"),
    ts,
    ei
  )

  lazy val pro2: Pro.Cli = Pro(
    Id(2),
    nameP("MMP-13 Inhibitors"),
    useL(hock),
    usesL(jado),
    plainP(""),
    ts,
    ei
  )

  //----------------------------------------------------------------------
  //                      Substances
  //----------------------------------------------------------------------

  def commonSub(
    i: Long,
    smiles: String,
  ): Sub.Cli = Sub(
    Id(i),
    plainP(""),
    Pure(Maybe(Mol read smiles) map (_.toDatMol)),
    Pure(false),
    Pure(CasNr unsafe ""),
    proL(pro1),
    Nil,
    Nil,
    ts, ei
  )

  lazy val sub1: Sub.Cli = Sub(
    Id(1),
    plainP("Ethanol"),
    Pure(Maybe(Mol read "CCO") map (_.toDatMol)),
    Pure(false),
    Pure(CasNr unsafe "64-17-5"),
    proL(pro1),
    List(con1, con2),
    Nil,
    ts, ei
  )

  def statsSub(
    i: Long,
    smiles: String,
    cons: Con.Cli*
  ): Sub.Cli = Sub(
    Id(i),
    plainP(""),
    Pure(Maybe(Mol read smiles) map (_.toDatMol)),
    Pure(false),
    Pure(CasNr unsafe ""),
    proL(pro2),
    cons.toList,
    Nil,
    ts, ei
  )

  lazy val sub2: Sub.Cli = statsSub(2L, "c1c(F)cccc1C(=O)NCC1CCC(N(C)C)C1", con3)

  lazy val subs: List[Sub.Cli] = List(
    sub1, sub2,
    statsSub(
      3L,
      "c1c(F)cccc1C(=O)NCC1CCC(N(C)C)C1",
      statsCon(1, "MM-1-1", List(1.25, 1.4), List(10.12, 13.3, 11.1)),
      statsCon(2, "MM-1-2", List(1.20, 1.3), List(9.8)),
    ),
    statsSub(
      4L,
      "c1c(F)c(F)ccc1C(=O)NCC1CCC(N(C)CCO)C1",
      statsCon(1, "MM-2-1", List(5.25, 4.4), List(30.1, 31.2)),
    ),
    statsSub(
      5L,
      "c1cc(N)ccc1C(=O)NC(O)C1C(O)C(O)C(N(C)C(F)(F)O)C1CO",
      statsCon(1, "MM-3-1", List(12.0, 9.9), List(1.0, 0.9, 0.82)),
    ),
    statsSub(
      6L,
      "c1cc(N)cc(O)c1C(=O)NCC1CCC(N(C)C)C1CCO",
      statsCon(1, "MM-4-1", List(23.0), List(80.1, 59.2)),
      statsCon(1, "MM-4-2", List(20.0, 17.2), List(60.1, 58.2)),
    ),
  ) ::: (7 to 20).toList.map(i ⇒ commonSub(i, alcohol(i)))

  def alcohol(i: Int) = (1 to i).toList.as("C").mkString("") ++ "O"

  //----------------------------------------------------------------------
  //                      Storage Locations
  //----------------------------------------------------------------------

  def stoL(s: Sto.Cli): Link[Sto.Id] = s.id -> s.name

  lazy val rt107: Sto.Cli = Sto(Id(1), nameP("RT107, Cabinet 1"), plainP(""), ts, ei)

  lazy val rt109: Sto.Cli = Sto(Id(2), nameP("RT109, Cabinet 1"), plainP(""), ts, ei)


  //----------------------------------------------------------------------
  //                      Suppliers
  //----------------------------------------------------------------------

  def supL(t: Sup.Cli): Link[Sup.Id] = t.id.to -> t.name

  def supsL(ts: Sup.Cli*): Pure[List[Link[Sup.Id]]] = Pure(ts.toList map supL)

  lazy val sup1: Sup.Cli = Sup(Id(1), nameP("Sigma Aldrich"), plainP(""), ts, ei)

  lazy val sup2: Sup.Cli = Sup(Id(2), nameP("Acros"), plainP(""), ts, ei)

  lazy val sup3: Sup.Cli = Sup(Id(3), nameP("React. Biol."), plainP(""), ts, ei)

  //----------------------------------------------------------------------
  //                      Users
  //----------------------------------------------------------------------

  def useL(u: Use.Cli): Pure[Link[Use.Id]] = Pure(u.id.to -> u.alias.name)

  def usesL(us: Use.Cli*): Pure[List[Link[Use.Id]]] = us.toList traverse useL

  lazy val hock: Use.Cli = Use(
    Id(1), 
    aliasP("hock"),
    plainP("Stefan"),
    plainP("Höck"),
    plainP("hock@zhaw.ch"),
    Pure(undef),
    Pure(UserLevel.Superuser),
    ts, ei
  )

  lazy val jodo: Use.Cli = Use(
    Id(2), 
    aliasP("jodo"),
    plainP("John"),
    plainP("Doe"),
    plainP("jodo@zhaw.ch"),
    Pure(undef),
    Pure(UserLevel.CommonUser),
    ts, ei
  )

  lazy val jado: Use.Cli = Use(
    Id(3), 
    aliasP("jado"),
    plainP("Jane"),
    plainP("Doe"),
    plainP("jado@zhaw.ch"),
    Pure(undef),
    Pure(UserLevel.CommonUser),
    ts, ei
  )

  lazy val pott: Use.Cli = Use(
    Id(4), 
    aliasP("pott"),
    plainP("Harry"),
    plainP("Potter"),
    plainP("pott@zhaw.ch"),
    Pure(undef),
    Pure(UserLevel.CommonUser),
    ts, ei
  )

  //----------------------------------------------------------------------
  //                      St and Envs
  //----------------------------------------------------------------------

  lazy val st: St = St(
    List(pro1, pro2),
    List(rt107, rt109),
    List(sup1, sup2, sup3),
    List(mmp10, mmp13),
    List(hock, jodo, jado, pott),
    subs,
    Sub toStats subs
  )

  lazy val emptySettings: USettings = UserSettings.empty

  lazy val red: Color = Color(255,0,0).get

  lazy val green: Color = Color(0,255,0).get

  lazy val yellow: Color = Color(255,255,0).get

  lazy val massCol = SubMol(DMol.Mass).ef

  lazy val mmp10Col = ExportStats(mmp10.id, MeanStat).f

  lazy val mmp13Col = ExportStats(mmp13.id, MeanStat).f

  lazy val massGrad = Gradient[Double](
    List(50D -> red, 150D -> yellow, 350D -> green, 600D -> yellow, 900D -> red),
    1
  )

  lazy val mmp10Grad =
    Gradient[Double](List(0.01D -> green, 0.1D -> yellow, 0.2D -> red), 3)

  lazy val mmp13Grad =
    Gradient[Double](List(0.01D -> red, 0.1D -> yellow, 0.2D -> green), 3)

  lazy val settings: USettings = emptySettings.copy(
    substanceColumns = List(
      SubMol(DMol.Structure).ef,
      SubId.ef,
      SubName.ef,
      SubCasNr.ef,
      massCol,
      SubMol(DMol.Formula).ef,
    ),
    methodColumns = List(
      SubMol(DMol.Structure).ef,
      SubId.ef,
      ConBatch.ef,
      massCol,
      SubMol(DMol.Lipinski).ef,
      mmp10Col,
      mmp13Col,
    ),
    doubleFormats = Map(
      massCol -> massGrad,
      mmp10Col -> mmp10Grad,
      mmp13Col -> mmp13Grad,
    ),
    exportFormat = Some(Sdf),
    exportSelectionO = Some(false),
    exportFieldsO = Some(List(
      SubMol(DMol.Structure).ef,
      SubId.ef,
      SubName.ef,
      SubCasNr.ef,
      massCol,
      SubMol(DMol.Formula).ef,
      ConLocation.ef,
      ConAmount.ef,
    ))
  )

  lazy val exp: Set[UIdP] = Set(
    UId.DataList(ConT, SubP(sub2.id :: HNil)),
    UId.DataList(BioT, ConP(con3.id :: sub2.id :: HNil)),
  )

  lazy val creds: Creds = Creds(hock, "", settings, NoAuth)

  lazy val dispEnv: DispEnv = DispEnv(None, exp, st, creds, NoChange(NotEditing))

  lazy val dispEnvUse: DispEnv = deExp(Set(
    UId.DataList(UseT, RootP),
    UId.Dat(UseP(hock.id.to[Use.type] :: HNil)),
  ))

  lazy val dispEnvPro: DispEnv = deExp(Set(
    UId.DataList(ProT, RootP),
    UId.Dat(ProP(pro2.id.to[Pro.type] :: HNil)),
    UId.DataList(MetT, ProP(pro2.id.to[Pro.type] :: HNil)),
    UId.Dat(MetP(mmp10.id :: HNil)),
  ))

  lazy val dispEnvTagSup: DispEnv = deExp(Set(
    UId.DataList(StoT, RootP),
    UId.DataList(SupT, RootP),
    UId.Dat(SupP(sup1.id :: HNil)),
  ))

  def deExp(s: Set[UIdP]): DispEnv = dispEnv.copy(exp = s)

  lazy val expSt: ExpSt = ExpSt.ini(settings)

  lazy val expEnv: ExpEnv = ExpEnv(expSt, dispEnv)

  lazy val expEnvEdit: ExpEnv = lens[ExpEnv].dispEnv.cmd.set(expEnv)(NoChange(Editing))

  lazy val statsEnv: ExpEnv = lens[ExpEnv].expSt.mode.set(expEnv)(MethodTable)

  lazy val gridEnv: ExpEnv = lens[ExpEnv].expSt.mode.set(expEnv)(SubstanceGrid)
}
