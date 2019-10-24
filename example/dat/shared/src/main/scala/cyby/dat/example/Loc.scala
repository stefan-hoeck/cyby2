/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat
package example

import shapeless.{::,HNil}

/**
  * Localization
  */
trait Loc extends cyby.dat.Loc {
  def logRes(r: Result): Vector[Log]
  def logDataErr(r: DataErr): Log
  def pathType(p: Path): (String,String)
  def translatePath(p: Path): String
  def itemRes[A](p: (String,String), r: EditRes[A]): String

  def filQuery(s: FilField): String

  def filField(s: FilField): String

  def subField(s: CpdField): String

  def conField(s: ConField): String

  def bioField(s: BioField): String

  def dataType(s: DataType): String
 
  def dataTypePlural(s: DataType): String

  def cloneContainer: String
}

trait LocEnUS extends Loc with cyby.dat.LocEnUS {
  lazy val localMap = coreLocalMap

  val cloneContainer = "clone container"

  val bioStatsP = ("method entry", "method entries")
  val filP = (name("file"), name("files"))
  val bioP = (name("bio"), name("bio"))
  val conP = (name("container"), name("containers"))
  val metP = (name("method"), name("methods"))
  val proP = (name("project"), name("projects"))
  val stoP = (name("storage"), name("storages"))
  val subP = (name("substance"), name("substances"))
  val supP = (name("supplier"), name("suppliers"))
  val useP = (name("user"), name("users"))

  def logRes(r: Result): Vector[Log] = r match {
    case Errors(es)         ⇒ Vector(es.toList: _*) map logDataErr
    case LoggedIn(_, u, _)  ⇒ Vector(Log debug loggedIn(u.alias.v.name, u.level))
    case LoggedOut          ⇒ Vector(Log debug loggedOut)
    case PasswordChanged    ⇒ Vector(Log info "password changed")
    case ProRes(r)          ⇒ Vector(Log info itemRes(proP,r))
    case MetRes(r)          ⇒ Vector(Log info itemRes(metP,r))
    case StoRes(r)          ⇒ Vector(Log info itemRes(stoP,r))
    case CpdRes(r)          ⇒ Vector(Log info itemRes(subP,r))
    case SupRes(r)          ⇒ Vector(Log info itemRes(supP,r))
    case UseRes(r)          ⇒ Vector(Log info itemRes(useP,r))
    case ExportRes(p)       ⇒ Vector(Log info s"File ${p} ready. Download should start automatically.")
    case BioStatsRes(r)     ⇒ Vector(Log info itemRes(bioStatsP,r))
    case SettingsChanged(r) ⇒ Vector(Log info "user settings changed and persisted")
    case BioDataValidated   ⇒ Vector(Log info s"""Bio data entries were successfully validated""")
  }

  def logDataErr(r: DataErr): Log = r match {
    case AuthenticationFailed  ⇒ Log error "authentication failed"
    case BatchExists(b,i)      ⇒ Log warn s"Batch $b already exists for this compound: (ID: $i)"
    case CantChangeAlias       ⇒ Log warn "even admins can't change their own alias"
    case CantChangeLevel       ⇒ Log warn "even admins can't change their own user level"
    case CantDeleteSelf        ⇒ Log warn "its always a bad idea to delete ones own account"
    case CasNrExists(c,i)      ⇒ Log warn s"CAS Number $c already exists: (Code: $i)"
    case FilExists(f, p)       ⇒ Log warn s"file $f already exists: (ID: ${translatePath(p)})"
    case Exists(n, p)          ⇒ Log warn s"A ${pathType(p)._1} with name ${n} already exists. (ID: ${translatePath(p)})"
    case CpdExists(n, p)       ⇒ Log warn s"A ${pathType(CpdP(p))._1} with name ${n} already exists. (ID: ${translatePath(CpdP(p))})"
    case StillLinked(p)        ⇒ Log warn s"This ${pathType(p)._1} is still linked to other data objects and cannot be deleted. (ID: ${translatePath(p)})"
    case InvalidCreds          ⇒ Log warn "invalid credentials"
    case NotFound(url)         ⇒ Log error s"the requested URL was not found: $url"
    case PathNotFound(p)       ⇒ Log error s"the following ${pathType(p)._1} does not exist (anymore): ${translatePath(p)}"
    case NotLoggedIn           ⇒ Log error s"You are not logged into CyBy2. Probably your session has expired due to prolonged inactivity."
    case Serious(msg)          ⇒ Log error s"A serious error occured at the server. This is a bug. Please inform your CyBy admin. The error message was: $msg"
    case ReadErr(s)            ⇒ Log error s"The server returned a read error. This is a bug. Get in touch with your CyBy admin. The string the server complained about was ${s}"
    case StructureExists(b,p)  ⇒ Log warn s"A compound with this structure already exists: (Code: ${p.head})"
    case Unauthorized          ⇒ Log error "you are not authorized for this action"
    case Unset(m)              ⇒ Log error s"""The server returnd a "result unset" error. This is a bug. Get in touch with your CyBy admin. The message from the server was $m."""
    case NameNotFound(n)       ⇒ Log error s"""The following name was not found in the database: $n"""
    case BatchNotFound(n)      ⇒ Log error s"""The following batch was not found in the database: $n"""
    case EmptyStructure        ⇒ Log warn "Structure and name of a compound cannot be both undefined"
    case QueryErr(f, str)      ⇒ Log error s"Unable to parse query for ${f}: ${str}. This is a bug. Contact your CyBy admin."
  }

  def pathType(p: Path): (String,String) = p match {
    case BioFilP(_) ⇒ filP
    case ConFilP(_) ⇒ filP
    case CpdFilP(_) ⇒ filP
    case BioP(_) ⇒ bioP
    case ConP(_) ⇒ conP
    case MetP(_) ⇒ metP
    case ProP(_) ⇒ proP
    case StoP(_) ⇒ stoP
    case CpdP(_) ⇒ subP
    case SupP(_) ⇒ supP
    case UseP(_) ⇒ useP
    case RootP   ⇒ ("Root", "Root")
  }

  def translatePath(p: Path): String = p match {
    case BioFilP(f::t) ⇒ s"${translatePath(BioP(t))}, ${filP._1} ${f}"
    case BioP(b::t)    ⇒ s"${translatePath(ConP(t))}, ${bioP._1} ${b}"
    case ConFilP(f::t) ⇒ s"${translatePath(ConP(t))}, ${filP._1} ${f}"
    case ConP(c::t)    ⇒ s"${translatePath(CpdP(t))}, ${conP._1} ${c}"
    case MetP(m::HNil) ⇒ s"${metP._1} ${m}"
    case ProP(p::HNil) ⇒ s"${proP._1} ${p}"
    case StoP(p::HNil) ⇒ s"${stoP._1} ${p}"
    case CpdFilP(f::t) ⇒ s"${translatePath(CpdP(t))}, ${filP._1} ${f}"
    case CpdP(s::HNil) ⇒ s"${subP._1} ${s}"
    case SupP(s::HNil) ⇒ s"${supP._1} ${s}"
    case UseP(u::HNil) ⇒ s"${useP._1} ${u}"
    case RootP         ⇒ s"Root"
  }

  def itemRes[A](p: (String,String), r: EditRes[A]) = r match {
    case Added(_)   ⇒ s"${p._1} added."
    case Updated(_) ⇒ s"${p._1} updated."
    case Deleted(_) ⇒ s"${p._1} deleted."
    case Found(as,tot,start) if as.size == 1 ⇒ s"One ${p._1} out of ${tot} found (starting at ${start})."
    case Found(as,tot,start)                 ⇒ s"${as.size} ${p._2} out of ${tot} found (starting at ${start})."
  }

  def bioStatsRes(count: Int, start: Int, total: Int) = count match {
    case 1 ⇒ s"One entry out of $total found (starting at $start)."
    case n ⇒ s"${n} entries out of $total found (starting at $start)."
  }

  def subField(s: CpdField): String = s match {
    case CpdId           ⇒ name("id")
    case CpdName         ⇒ name("name")
    case CpdAbs          ⇒ name("abs")
    case CpdCasNr        ⇒ name("casNr")
    case CpdProject      ⇒ name("project")
    case CpdCreated      ⇒ name("created")
    case CpdContainers   ⇒ name("containers")
    case CpdMol(f)       ⇒ molField(f)
    case CpdEditInfo(f)  ⇒ editInfoField(f)
    case CpdFil(f)       ⇒ filQuery(f)
  }

  def conField(s: ConField): String = s match {
    case ConId            ⇒ name("id")
    case ConLocation      ⇒ name("location")
    case ConSupplier      ⇒ name("supplier")
    case ConBatch         ⇒ name("batch")
    case ConOrderNr       ⇒ name("orderNr")
    case ConComment       ⇒ name("comment")
    case ConLentTo        ⇒ name("lentTo")
    case ConPurity        ⇒ name("purity")
    case ConPurityStr     ⇒ name("purityStr")
    case ConDensity       ⇒ name("density")
    case ConConcentration ⇒ name("concentration")
    case ConAmount        ⇒ name("amount")
    case ConEmpty         ⇒ name("empty")
    case ConProject       ⇒ name("project")
    case ConCreated       ⇒ name("created")
    case ConEditInfo(f)   ⇒ editInfoField(f)
    case ConFil(f)        ⇒ filQuery(f)
  }

  def bioField(s: BioField): String = s match {
    case BioId            ⇒ name("id")
    case BioValue         ⇒ name("value")
    case BioMethod        ⇒ name("method")
    case BioSupplier      ⇒ name("supplier")
    case BioDate          ⇒ name("date")
    case BioComment       ⇒ name("comment")
    case BioProject       ⇒ name("project")
    case BioCreated       ⇒ name("created")
    case BioEditInfo(f)   ⇒ editInfoField(f)
    case BioFil(f)        ⇒ filQuery(f)
  }

  def filQuery(s: FilField) = s"File: ${filField(s)}"

  def filField(s: FilField): String = s match {
    case FilId            ⇒ name("id")
    case FilName          ⇒ name("name")
    case FilPath          ⇒ name("path")
    case FilComment       ⇒ name("comment")
    case FilProject       ⇒ name("project")
    case FilCreated       ⇒ name("created")
    case FilEditInfo(f)   ⇒ editInfoField(f)
  }

  def dataType(s: DataType) = s match {
    case BioT   ⇒ "Biodata"
    case ConT   ⇒ "Container"
    case FilT   ⇒ "File"
    case MetT   ⇒ "Method"
    case ProT   ⇒ "Project"
    case StatsT ⇒ "Statistics"
    case StoT   ⇒ "Location"
    case CpdT   ⇒ "Compound"
    case SupT   ⇒ "Supplier"
    case UseT   ⇒ "User"
  }

  def dataTypePlural(s: DataType) = s match {
    case BioT   ⇒ "Biodata"
    case ConT   ⇒ "Containers"
    case FilT   ⇒ "Files"
    case MetT   ⇒ "Methods"
    case ProT   ⇒ "Projects"
    case StatsT ⇒ "Statistics"
    case StoT   ⇒ "Locations"
    case CpdT   ⇒ "Compounds"
    case SupT   ⇒ "Suppliers"
    case UseT   ⇒ "Users"
  }
}

object LocEnUS extends LocEnUS

// vim: set ts=2 sw=2 et:
