/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui
package example

import cats.implicits.{none ⇒ _, _}
import cyby.dat._, UserLevel._
import cyby.dat.example._

import CompType._, IconType.{Clone ⇒ _, _}

import shapeless.{HList, HNil}

import UId.{Item ⇒ _, _}

trait DispZShared extends CyByZ {
  import tags.{id, cls, title}
   
  def htmlIni(d: DispEnv): String = htmlLeft(d) ++ Txt.mainView(d.mode)

  def htmlLeft(d: DispEnv): String =
    Txt.leftView(d.creds, changes, navSections(d))

  def navSections(de: DispEnv): String = {
    def nav[A](dt: DataType, u: UserLevel, disp: DispEnv ⇒ A ⇒ String)
      (get: St ⇒ List[A]): String =
      Txt.navSection(de, dt, RootP, loc dataTypePlural dt, u, get(de.st) map disp(de): _*)

    nav(ProT, Superuser,  pro)(_.pros)  ++
    nav(MetT, CommonUser, met)(_.mets)  ++
    nav(StoT, CommonUser, sto)(_.stos)  ++
    nav(SupT, CommonUser, sup)(_.sups)  ++
    nav(UseT, Admin,      use)(_.uses)
  }


  def changes = List(
  )

  def pro = Txt.dispNav(proDets)(
    _.name.v,
    p ⇒ ProP(proId(p.id) :: HNil),
    Pro.name,
    some(Admin),
    some(Superuser),
    List(NSSub, NSConAll, NSConNonEmpty, NSBio),
  )

  def met = Txt.dispNav(metDets)(
    _.name.v,
    m ⇒ MetP(m.id :: HNil),
    Met.name,
    some(Admin),
    some(CommonUser),
    List(NSBio),
  )

  def sto = Txt.dispNav(stoDets)(
    _.name.v,
    m ⇒ StoP(m.id :: HNil),
    Met.name,
    some(Admin),
    some(CommonUser),
    List(NSConAll, NSConNonEmpty),
  )

  def sup = Txt.dispNav(supDets)(
    _.name.v,
    s ⇒ SupP(s.id :: HNil),
    Sup.name,
    some(Admin),
    some(CommonUser),
    List(NSConAll, NSConNonEmpty,NSBio),
  )

  def use = Txt.dispNav(useDets)(
    _.alias.v.name,
    u ⇒ UseP(useId(u.id) :: HNil),
    Use.alias,
    some(Admin),
    some(Admin),
    Nil,
  )

  def proDets(pr: Pro.Cli, de: DispEnv) = {
    val pp = proId(pr.id) :: HNil
    val p = ProP(pp).path
    Txt.navDets(de, p)(
      Txt.linkRow(p, Pro.owner, pr.owner, de, some(Superuser)),
      Txt.linksRow(p, Pro.users, pr.users, de, some(Superuser)),
      Txt.navDetRow(p, Pro.comment, pr.comment.v.v, de, some(Superuser)),
      Txt.createRow(pr.created),
      Txt.editRow(pr.modified),
    )
  }

  def supDets(s: Sup.Cli, de: DispEnv) = {
    val p = SupP(s.id :: HNil)
    Txt.navDets(de, p)(
      Txt.navDetRow(p, Sup.address, s.address.v.v, de, some(CommonUser)),
      Txt.createRow(s.created),
      Txt.editRow(s.modified),
    )
  }

  def stoDets(s: Sto.Cli, de: DispEnv) = {
    val p = StoP(s.id :: HNil)
    Txt.navDets(de, p)(
      Txt.navDetRow(p, Sto.comment, s.comment.v.v, de, some(CommonUser)),
      Txt.createRow(s.created),
      Txt.editRow(s.modified),
    )
  }

  def metDets(m: Met.Cli, de: DispEnv) = {
    val p = MetP(m.id :: HNil)
    Txt.navDets(de, p)(
      Txt.navDetRow(p, Met.comment, m.comment.v.v, de, some(CommonUser)),
      Txt.createRow(m.created),
      Txt.editRow(m.modified),
    )
  }

  def useDets(u: Use.Cli, de: DispEnv) = {
    val p = UseP(useId(u.id) :: HNil).path
    val editLvl = if (u.id === de.creds.user.id) some(CommonUser)
                  else some(Admin)

    Txt.navDets(de, p)(
      Txt.navDetRow(p, Use.firstName, u.firstName.v.v, de, editLvl),
      Txt.navDetRow(p, Use.lastName, u.lastName.v.v, de, editLvl),
      Txt.navDetRow(p, Use.email, u.email.v.v, de, editLvl),
      Txt.navDetRow(p, Use.level, loc dispUserLevel u.level.v, de, Some(Admin)),
      Txt.editRow(u.modified),

      if (some(de.creds.user.level.v) >= editLvl) (
        Txt.li(cls := CompType.NavDetailRow(Use.password).c)(
          Txt.div(id := EditCont(Use.password.name, p))(
            Txt.button(
              id := ClickEdit(Use.password.name, p),
              cls := WidgetType.EditPasswordBtn.c,
            )(loc.editPassword),
          )
        )
      ) else ""
    )
  }

  def dispSub(e: ExpEnv)(s: Sub.Cli): String = {
    val row = e.expSt.columns map singleCell(e, s) mkString ""
    rest(e, s, row)
  }
  
  def dispStats(e: ExpEnv)(s: BioStats): String = {
    val row = e.expSt.columns map singleCellStats(e, s) mkString ""

    Txt.li(cls := Comp(CompType.ExplorerSubRow))(row)
  }

  def rest(e: ExpEnv, s: Sub.Cli, row: String): String = {
    val pth = s.id :: HNil
    val p = SubP(pth).path

    Txt.div(id := EditCont(Sub.structure.name,p))() ++
    Txt.li(cls := ExplorerSubRow.c)(row) ++
    mkCons(e, pth, s.containers) ++
    mkFils(e, pth, s.files)(SubP, SubFilP)
  }

  def singleCell(e: ExpEnv, s: Sub.Cli)(d: Column) = d match {
    case ExportSub(f) ⇒ f match {
      case SubId                 ⇒ Txt.subCell(d, s.id.toString)
      case SubName               ⇒ nameDesc(e)(s)
      case SubAbs                ⇒ abs(e)(s)
      case SubCasNr              ⇒ casNr(e)(s)
      case SubProject            ⇒ project(e)(s)
      case SubCreated            ⇒ Txt.created(SubCreated.ef, s.created)
      case SubMol(fld)           ⇒ fld match {
        case Mol.Structure         ⇒ Txt.structCell(d, s, some(Admin), some(CommonUser), e)
        case _                     ⇒ Txt.mol(fld, s.structure.o, e.expSt)
      }
      case SubContainers         ⇒ ""
      case SubEditInfo(ef)       ⇒ Txt.editInfo(d, s.modified, ef)
      case SubFil(_)             ⇒ ""
    }
    case _            ⇒ ""
  }

  def singleCellStats(e: ExpEnv, s: BioStats)(c: Column): String = c match {
    case ExportSub(f) ⇒ f match {
      case SubMol(fld)           ⇒ fld match {
        case Mol.Structure         ⇒ Txt.structCell(c, s, None, None, e)
        case _                     ⇒ Txt.mol(fld, s.sub.structure.v.o, e.expSt)
      }
      case SubAbs                ⇒ Txt.subCellBool(c, s.sub.abs.v)
      case SubCasNr              ⇒ Txt.subCell(c, s.sub.casNr.v.v)
      case SubContainers         ⇒ ""
      case SubCreated            ⇒ Txt.created(c, s.sub.created)
      case SubEditInfo(ef)       ⇒ Txt.editInfo(c, s.sub.modified, ef)
      case SubFil(_)             ⇒ ""
      case SubId                 ⇒ Txt.subCell(c, s.sub.id.toString)
      case SubName               ⇒ Txt.subCell(c, s.sub.name.v.v)
      case SubProject            ⇒ Txt.subCell(c, s.sub.project.v._2.v)
    }
    case ExportCon(f) ⇒ f match {
      case ConAmount        ⇒ Txt.gradientCell(c, some(s.con.amount.v.v), e.expSt)
      case ConBatch         ⇒ Txt.subCell(c, s.con.batch.v.v)
      case ConComment       ⇒ Txt.subCell(c, s.con.comment.v.v)
      case ConConcentration ⇒ Txt.gradientCell(c, some(s.con.concentration.v.v), e.expSt)
      case ConCreated       ⇒ Txt.created(c, s.con.created)
      case ConDensity       ⇒ Txt.gradientCell(c, some(s.con.density.v.v), e.expSt)
      case ConEditInfo(ef)  ⇒ Txt.editInfo(c, s.con.modified, ef)
      case ConEmpty         ⇒ Txt.subCellBool(c, s.con.empty.v)
      case ConId            ⇒ Txt.subCell(c, s.con.id.toString)
      case ConLentTo        ⇒ Txt.subCell(c, s.con.lentTo.v.v)
      case ConLocation      ⇒ Txt.subCell(c, s.con.location.v._2.v)
      case ConOrderNr       ⇒ Txt.subCell(c, s.con.orderNr.v.v)
      case ConPurity        ⇒ Txt.gradientCell(c, some(s.con.purity.v.v), e.expSt)
      case ConPurityStr     ⇒ Txt.subCell(c, s.con.purityStr.v.v)
      case ConProject       ⇒ Txt.subCell(c, s.con.project.v._2.v)
      case ConSupplier      ⇒ Txt.subCell(c, s.con.supplier.v._2.v)
      case ConFil(_)        ⇒ ""
    }
    case b@ExportStats(_,_)  ⇒ metCell(b, e.expSt)(s)
    case _                   ⇒ ""
  }

    def metCell(c: ExportStats, expSt: ExpSt)(s: BioStats): String =
      Txt.statsCell(c, c.stat, s.stats get c.mid, expSt)

  def abs(e: ExpEnv) = Txt.editableSubCellBool[Sub.Cli](
    SubAbs.ef, _.abs.v, some(CommonUser), e)

  def casNr(e: ExpEnv) = Txt.editableSubCell[Sub.Cli](
    SubCasNr.ef, _.casNr.v.v, some(CommonUser), e)

  def nameDesc(e: ExpEnv) = Txt.editableSubCell[Sub.Cli](
    SubName.ef, _.name.v.v, some(CommonUser), e)

  def project(e: ExpEnv) = Txt.editableSubCell[Sub.Cli](
    SubProject.ef, _.project.v._2.v, some(CommonUser), e)


  //----------------------------------------------------------------------
  //                      Containers
  //----------------------------------------------------------------------

  def mkCon(e: ExpEnv, pth: Sub.Path)(c: Con.Cli): String =  {
    val cpth = c.id :: pth
    val p = ConP(cpth).path
    val ename = Con.empty.name

    val purityDets =
      if (e isEditingAs CommonUser)
        Txt.conDetRow(p, Con.purity, Txt text s"${c.purity.v}", some(CommonUser), e) ++
        Txt.conDetRow(p, Con.purityStr, Txt text c.purityStr.v.v, some(CommonUser), e)
      else
        Txt.conDetRow(p, Con.purity, Txt text purity(c), some(CommonUser), e)

    Txt.li(id := Dat(p), cls := ExplorerConRow.c)(
      Txt.div(id := EditCont(ename, p), cls := ConIconCell.c){
        val flaskCls = if (c.empty) FlaskEmpty.c
                       else if (Con isLent c) FlaskLent.c
                       else Flask.c

        val flaskI = if (e.mode.isEditing)
                       Txt.div(id := UId.Edit(ename,p), cls := flaskCls)()
                     else Txt.div(cls := flaskCls)()

        val cloneI = e.ifEditingAs(CommonUser)(
                       Txt.div(
                         id := Clone(p),
                         cls := IconType.Clone.c,
                         title := loc.cloneContainer
                       )(),
                     )

        flaskI ++ cloneI
      },
      Txt.ul(cls := ConRestCell.c)(
        Txt.li(cls := ExplorerConRow.c)(
          Txt.div(cls := ConDetails.c)(
            Txt.conDetRow(p, Con.location, Txt text c.location.v._2.v, some(CommonUser), e),
            Txt.conDetRow(p, Con.project, Txt text c.project.v._2.v, some(CommonUser), e),
          ),
          Txt.div(cls := ConDetails.c)(
            Txt.conDetRow(p, Con.comment, Txt text c.comment.v.v, some(CommonUser), e),
            Txt.conDetRow(p, Con.lentTo, Txt text c.lentTo.v.v, some(CommonUser), e),
          ),
          Txt.div(cls := ConDetails.c)(
            Txt.conDetRow(p, Con.supplier, Txt text c.supplier.v._2.v, some(CommonUser), e),
            Txt.conDetRow(p, Con.batch, Txt text c.batch.v.v, some(CommonUser), e),
            Txt.conDetRow(p, Con.orderNr, Txt text c.orderNr.v.v, some(CommonUser), e),
          ),
          Txt.div(cls := ConDetails.c)(
            Txt.conDetRow(p, Con.amount, Txt text s"${c.amount.v}", some(CommonUser), e),
            purityDets,
            Txt.conDetRow(p, Con.density, Txt text s"${c.density.v}", some(CommonUser), e),
            Txt.conDetRow(p, Con.concentration, Txt text s"${c.concentration.v}", some(CommonUser), e),
          ),
          Txt.div(cls := ConDetails.c)(
            Txt.conDetRow(p, Con.created, Txt timeStampStr c.created, None, e),
            Txt.conDetRow(p, Con.modified, Txt editStr c.modified, None, e),
          ),
          e.ifEditingAs(Admin)(Txt.div(id := DeleteId(p), cls := DeleteHidden.c)()),
        ),
        mkBios(e, cpth, c.bio),
        mkFils(e, cpth, c.files)(ConP, ConFilP),
      )
    )
  }

  def mkCons(e: ExpEnv, pth: Sub.Path, cons: List[Con.Cli]): String =  {
    val p = SubP(pth).path
    val i = DataList(ConT,p).i

    Txt.li(cls := ExplorerConRowHeader.c)(
      Txt.expBtn(e.exp, i),
      Txt.h1(cls := TitleType.ConRow.c)(Txt text s"${loc name Sub.containers} (${cons.size})"),
      e.ifEditingAs(CommonUser)(Txt.div(id := Create(ConT,p), cls := AddHidden.c)()),
    ) ++
    Txt.ul(
      id := CreateCont(ConT,p),
      cls := NavCreateContainer.c
    )() ++
    Txt.ul(id := i, cls := ContainerDets.c, Txt.hide(e.exp,i))(
      cons map mkCon(e, pth) mkString ""
    ),
  }

  def location(c: Con.Cli) = Txt text c.location.v._2.v

  def purity(c: Con.Cli) = c.purityStr.v.v match {
    case "" ⇒ s"${c.purity.v}"
    case s  ⇒ s"${c.purity.v} ($s)"
  }

  //----------------------------------------------------------------------
  //                      Bio Data
  //----------------------------------------------------------------------

  def mkBios(e: ExpEnv, conP: Con.Path, bs: List[Bio.Cli]): String =  {
    val p = ConP(conP).path
    val i = DataList(BioT, p).i

    Txt.li(cls := ExplorerConRowHeader.c)(
      Txt.expBtn(e.exp, i),
      Txt.h1(cls := Title(TitleType.ConRow))(Txt text s"${loc name Con.bio} (${bs.size})"),
      e.ifEditingAs(CommonUser)(Txt.div(id := Create(BioT,p), cls := AddHidden.c)()),
    ) ++
    Txt.ul(
      id := CreateCont(BioT,p),
      cls := NavCreateContainer.c
    )() ++
    Txt.ul(id := i, cls := ContainerDets.c, Txt.hide(e.exp,i))(
      bs map mkBio(e, conP) mkString ""
    ),
  }

  def mkBio(e: ExpEnv, conP: Con.Path)(b: Bio.Cli): String = {
    val bioP = b.id :: conP
    val p = BioP(bioP).path
    Txt.li(id := Dat(p), cls := ExplorerConRow.c)(
      Txt.div(cls := ConDetails.c)(
        Txt.conDetRow(p, Bio.value, b.value.v.toString, some(CommonUser), e),
        Txt.conDetRow(p, Bio.method, b.method.v._2.v, some(CommonUser), e),
        Txt.conDetRow(p, Bio.date, Txt dateStr b.date.v.v, some(CommonUser), e),
      ),
      Txt.div(cls := ConDetails.c)(
        Txt.conDetRow(p, Bio.supplier, b.supplier.v._2.v, some(CommonUser), e),
        Txt.conDetRow(p, Bio.project, b.project.v._2.v, some(CommonUser), e),
        Txt.conDetRow(p, Bio.comment, Txt text b.comment.v.v, some(CommonUser), e),
      ),
      Txt.div(cls := ConDetails.c)(
        Txt.conDetRow(p, Bio.created, Txt timeStampStr b.created, None, e),
        Txt.conDetRow(p, Bio.modified, Txt editStr b.modified, None, e),
      ),
      e.ifEditingAs(Admin)(Txt.div(id := DeleteId(p), cls := DeleteHidden.c)()),
    ) ++ mkFils(e, bioP, b.files)(BioP, BioFilP)
  }

  //----------------------------------------------------------------------
  //                      Files
  //----------------------------------------------------------------------

  def mkFils[P<:HList](e: ExpEnv, p: P, fs: List[Fil.Cli])(
    pth: P ⇒ Path,
    filPth: shapeless.::[Fil.Id,P] ⇒ Path,
  ): String =  {
    val ppth = pth(p)
    val i = DataList(FilT, ppth).i

    def mkFil(f: Fil.Cli): String = {
      val fpth = filPth(f.id :: p)
      val linkDets = if (e.isEditingAs(CommonUser))
                      Txt.div(cls := ConDetails.c)(
                        Txt.conDetRow(fpth, Fil.name, f.name.v.v, some(CommonUser), e),
                        Txt.conDetRow(fpth, Fil.path, f.path.v.v, some(CommonUser), e),
                      )
                    else
                      Txt.div(cls := ConDetails.c)(
                        Txt.fileLink(fpth, f.path.v, f.name.v, e.creds)
                      )

      Txt.li(id := Dat(fpth), cls := ExplorerConRow.c)(
        linkDets,
        Txt.div(cls := ConDetails.c)(
          Txt.conDetRow(fpth, Fil.project, f.project.v._2.v, some(CommonUser), e),
        ),
        Txt.div(cls := ConDetails.c)(
          Txt.conDetRow(fpth, Fil.comment, Txt text f.comment.v.v, some(CommonUser), e),
        ),
        Txt.div(cls := ConDetails.c)(
          Txt.conDetRow(fpth, Fil.created, Txt timeStampStr f.created, None, e),
          Txt.conDetRow(fpth, Fil.modified, Txt editStr f.modified, None, e),
        ),
        e.ifEditingAs(Admin)(Txt.div(id := DeleteId(fpth), cls := DeleteHidden.c)()),
      )
    }

    Txt.li(cls := ExplorerConRowHeader.c)(
      Txt.expBtn(e.exp, i),
      Txt.h1(cls := Title(TitleType.ConRow))(Txt text s"${loc name Sub.files} (${fs.size})"),
      e.ifEditingAs(CommonUser)(Txt.div(id := Create(FilT, ppth), cls := AddHidden.c)()),
    ) ++
    Txt.ul(
      id  := CreateCont(FilT, ppth),
      cls := NavCreateContainer.c
    )() ++
    Txt.ul(id := i, cls := ContainerDets.c, Txt.hide(e.exp,i))(
      fs map mkFil mkString ""
    ),
  }
}

// vim: set ts=2 sw=2 et:

