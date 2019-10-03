/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

import cyby.export._
import cyby.query.{Comp ⇒ QComp, Fingerprint}

/**
  * Localized name, description and error messages for a data type or field
  */
case class Local(
  name: String,
  desc: String,
  msg:  String ⇒ String
)

/**
  * Localization of types and messages common to most
  * CyBy applications.
  */
trait Loc {
  def localMap: Map[String,Local]

  def name(s: String): String = localMap.get(s).fold(s)(_.name)

  def desc(s: String): String = localMap.get(s).fold("")(_.desc)

  def msg(s: String): String ⇒ String =
    localMap.get(s).fold[String ⇒ String](identity)(_.msg)

  def name(s: Symbol): String = name(s.name)

  def desc(s: Symbol): String = desc(s.name)

  def msg(s: Symbol): String ⇒ String = msg(s.name)

  def symbol(s: Symbol): Option[Local] = localMap get s.name

  def dispUserLevel(l: UserLevel): String

  def substance: String
  def substances: String

  def container: String
  def containers: String

  def bio: String
  def bios: String

  def statistics: String

  def numberOfColumns: String
  def quickSearch: String
  def includeEmpty: String
  def caseSensitive: String
  def nostructure: String
  def exactstructure: String
  def substructure: String
  def similarity: String
  def loggedIn(n: Alias, lvl: UserLevel): String
  def loggedOut: String
  def changelog: String
  def explorer: String
  def info: String
  def log: String
  def login: String
  def loginInfo: String
  def loginStatus: String
  def logout: String
  def navigator: String
  def query: String
  def queries: String
  def runQuery: String
  def deleteQuery: String
  def saveQuery: String
  def loadQuery: String
  def userName: String
  def editPassword: String
  def stringQueryPrefix(s: String): String
  def export: String
  def exportSettings: String

  def bool(b: Boolean): String

  def exportFormat(f: Format): String

  def fingerprint(f: Fingerprint): String

  def queryComp(c: QComp): String

  def molField(f: Mol.Field): String

  def editInfoField(f: EditInfo.Field): String

  def statsType(s: StatsType): String


  def aliasMsg: String ⇒ String
  def amountMsg: String ⇒ String
  def casNrMsg: String ⇒ String
  def colorMsg: String ⇒ String
  def concentrationMsg: String ⇒ String
  def dateMsg: String ⇒ String
  def densityMsg: String ⇒ String
  def doubleMsg: String ⇒ String
  def fileNameMsg: String ⇒ String
  def idMsg: String ⇒ String
  def intMsg: String ⇒ String
  def levelMsg: String ⇒ String
  def longMsg: String ⇒ String
  def nameMsg: String ⇒ String
  def passwordMsg: String ⇒ String
  def percentMsg: String ⇒ String
  def plainMsg: String ⇒ String
  def predicateMsg: String ⇒ String
}

trait LocEnUS extends Loc {
  def dispUserLevel(u: UserLevel): String = u match {
    case UserLevel.Guest       ⇒ "Guest"
    case UserLevel.CommonUser  ⇒ "User"
    case UserLevel.Superuser   ⇒ "Superuser"
    case UserLevel.Admin       ⇒ "Administrator"
    case u                     ⇒ u.toString
  }

  val aliasMsg = v ⇒
    if (v.isEmpty) "an alias cannot be an empty string"
    else if (v.length > Alias.MaxLength) s"an alias cannot have more than ${Alias.MaxLength} characters"
    else "only characters in the range a - z and digits (0 - 9) are allowed"

  val amountMsg = s ⇒ s"not a non-negative number: $s"

  val casNrMsg = s ⇒ s"not a valid CAS-number: $s"

  val colorMsg = s ⇒ s"not a valid color: $s"

  val concentrationMsg = s ⇒ s"not a value between 0 and ${Concentration.MaxValue}: $s"

  val dateMsg = s ⇒ s"not a valid date: $s"

  val densityMsg = s ⇒ s"not a value between 0 and ${Density.MaxValue}: $s"

  val doubleMsg = s ⇒ s"not a number: $s"

  val idMsg = s ⇒ s"not a valid ID: $s"

  val intMsg = s ⇒ s"not an integer: $s"

  val levelMsg = s ⇒ s"not a valid user role: $s"

  val longMsg = s ⇒ intMsg(s)

  val fileNameMsg = v ⇒
    if (v.isEmpty) "empty file names are not allowed"
    else if (v.length > FileName.MaxLength) s"max length is ${FileName.MaxLength} characters"
    else "only the following characters are valid: A-Z, a-z, 0-9, -, _, ."

  val plainMsg = v ⇒
    if (v.length > Plain.MaxLength) s"max length is ${Plain.MaxLength} characters"
    else "no control characters allowed"

  val predicateMsg = v ⇒ s"invalide predicate: $v"

  val nameMsg = v ⇒
    if (v.isEmpty) "a name cannot be an empty string"
    else plainMsg(v)

  val passwordMsg = v ⇒
    if (v.length < Password.MinLength || v.length > Password.MaxLength)
      s"passwords must be between ${Password.MinLength} and ${Password.MaxLength} characters"
    else plainMsg(v)

  val percentMsg = s ⇒ s"not a value between 0 and 100: $s"

  lazy val coreLocalMap: Map[String,Local] = Map(
    "abs"              -> Local("Absolute configuration", "true, if the given molecule is chiral and enantiomerically pure", _ ⇒ ""),
    "address"          -> Local("Address", "", plainMsg),
    "address2"         -> Local("Address2", "", plainMsg),
    "alias"            -> Local("Alias", "", nameMsg),
    "amount"           -> Local("Amount [g or ml]", "", amountMsg),
    "batch"            -> Local("Batch", "", plainMsg),
    "bio"              -> Local("Biological data", "", _ ⇒ ""),
    "booleanFormats"   -> Local("Format Rules (Boolean)", "", _ ⇒ ""),
    "casNr"            -> Local("CAS-Number", "", casNrMsg),
    "children"         -> Local("Locations", "", _ ⇒ ""),
    "city"             -> Local("City", "", plainMsg),
    "code"             -> Local("Code", "", s ⇒ s"not a valid code: $s"),
    "color"            -> Local("Color", "", colorMsg),
    "comment"          -> Local("Comment", "", plainMsg),
    "concentration"    -> Local("Conc. [mol/l]", "Concentration [mol/l]", concentrationMsg),
    "container"        -> Local("Container", "", _ ⇒ ""),
    "containers"       -> Local("Containers", "", _ ⇒ ""),
    "count"            -> Local("Count", "", _ ⇒ ""),
    "created"          -> Local("Time of Creation", "", _ ⇒ ""),
    "date"             -> Local("Date", "", dateMsg),
    "density"          -> Local("Density [g/ml]", "", densityMsg),
    "doubleFormats"    -> Local("Format Rules (Number)", "", _ ⇒ ""),
    "email"            -> Local("EMail", "", plainMsg),
    "empty"            -> Local("Empty", "true, if the container is empty", _ ⇒ ""),
    "exactMass"        -> Local("MW [g/mol] (exact)", "", _ ⇒ ""),
    Mol.exactStructure -> Local(exactstructure, "", _ ⇒ ""),
    "exportFieldsO"    -> Local("Exported Fields", "", _ ⇒ ""),
    "exportFormat"     -> Local("File Type", "", _ ⇒ ""),
    "exportSelectionO" -> Local("Selected Only", "", _ ⇒ ""),
    "file"             -> Local("File", "", _ ⇒ ""),
    "filename"         -> Local("File", "", _ ⇒ ""),
    "files"            -> Local("Files", "", _ ⇒ ""),
    "firstName"        -> Local("First Name", "", plainMsg),
    "formula"          -> Local("Formula", "", _ ⇒ ""),
    "gradient"         -> Local("Gradient", "", _ ⇒ ""),
    "id"               -> Local("ID", "", idMsg),
    "inchi"            -> Local("InChI", "", _ ⇒ ""),
    "integerFormats"   -> Local("Format Rules (Integer)", "", _ ⇒ ""),
    "modified"         -> Local("Last modified", "", _ ⇒ ""),
    "lastName"         -> Local("Last Name", "", plainMsg),
    "lastName"         -> Local("Last Name", "", plainMsg),
    "lentTo"           -> Local("Lent to", "", plainMsg),
    "level"            -> Local("Role", "", levelMsg),
    "lipinski"         -> Local("Lipinski", "Lipinski's rule of five", _ ⇒ ""),
    "location"         -> Local("Location", "", _ ⇒ ""),
    "locations"        -> Local("Storage Locations", "", _ ⇒ ""),
    "logP"             -> Local("logP", "", _ ⇒ ""),
    "mass"             -> Local("MW [g/mol]", "molar weight", _ ⇒ ""),
    "method"           -> Local("Method", "", _ ⇒ ""),
    "methodColumns"    -> Local("Columns", "", _ ⇒ ""),
    "methods"          -> Local("Methods", "", _ ⇒ ""),
    "name"             -> Local("Name", "", nameMsg),
    "nod"              -> Local("Number of digits", "", s ⇒ s"not a non-negative integer: $s"),
    StatsQ             -> Local("Statistics", "", _ ⇒ ""),
    "orderNr"          -> Local("Catalogue Nr", "", plainMsg),
    "owner"            -> Local("Owner", "", _ ⇒ ""),
    "password"         -> Local("Password", "", passwordMsg),
    "path"             -> Local("Name on disk", "", fileNameMsg),
    "project"          -> Local("Project", "", _ ⇒ ""),
    "projects"         -> Local("Projects", "", _ ⇒ ""),
    "purity"           -> Local("Purity [%]", "", percentMsg),
    "purityStr"        -> Local("Purity (Desc.)", "", plainMsg),
    "otherO"           -> Local("Other Settings", "", _ ⇒ ""),
    "queryO"           -> Local("Query", "", _ ⇒ ""),
    "queriesO"         -> Local("Queries", "", _ ⇒ ""),
    "lastQueryO"       -> Local("Last Query", "", _ ⇒ ""),
    "quickO"           -> Local("Quick Search", "", _ ⇒ ""),
    "regex"            -> Local("Regular Expression", "", v ⇒ s"invalid regular expression: $v"),
    "reviewers"        -> Local("Reviewers", "", _ ⇒ ""),
    "size"             -> Local("Size", "", _ ⇒ ""),
    "smiles"           -> Local("Smiles", "", _ ⇒ ""),
    "state"            -> Local("State", "", plainMsg),
    "storages"         -> Local("Storage Locations", "", _ ⇒ ""),
    "stringFormats"    -> Local("Format Rules (String)", "", _ ⇒ ""),
    "structure"        -> Local("Structure", "", _ ⇒ ""),
    Mol.subStructure   -> Local(substructure, "", _ ⇒ ""),
    "substanceColumns" -> Local("Columns", "", _ ⇒ ""),
    "supplier"         -> Local("Supplier", "", _ ⇒ ""),
    "suppliers"        -> Local("Suppliers", "", _ ⇒ ""),
    "svg"              -> Local("SVG Image", "", _ ⇒ ""),
    "tag"              -> Local("Tag", "", _ ⇒ ""),
    "tags"             -> Local("Tags", "", _ ⇒ ""),
    "tpsa"             -> Local("TPSA", "Total Polar Surface Area", _ ⇒ ""),
    "user"             -> Local("User", "", _ ⇒ ""),
    "users"            -> Local("Users", "", _ ⇒ ""),
    "value"            -> Local("Value", "", doubleMsg),
    "zip"              -> Local("Zip", "", plainMsg)
  )


  val nostructure = "Structure: None"
  val exactstructure = "Structure (exact)"
  val substructure = "Sub-structure"
  val similarity = "Similarity"

  val logout             = "Logout"

  def loggedIn(n: Alias, l: UserLevel): String =
    s"Logged in as $n (Level: ${dispUserLevel(l)})<br>"

  val substance = "Substance"
  val substances = "Substances"

  val container = "Container"
  val containers = "Containers"

  val bio = "Biodata"
  val bios = "Biodata"

  val statistics = "Statistics"

  val loggedOut          = "Logged out"
  val login              = "Login"
  val loginInfo          = "Login info"
  val loginStatus        = "Login Status"
  val navigator          = "Navigator"
  val query              = "Query"
  val queries            = "Queries"
  val runQuery           = "Run Query"
  val deleteQuery        = "Delete Query"
  val saveQuery          = "Save Query"
  val loadQuery          = "Load Query"
  val userName           = "User Name"
  val changelog          = "Change Log"
  val editPassword       = "Change Password"
  val explorer           = "Explorer"
  val info               = "Info"
  val log                = "Log"
  val quickSearch        = "Quick Search"
  val includeEmpty       = "Include empty containers"
  val caseSensitive      = "Case sensitive search"
  val numberOfColumns    = "Number of Columns"
  val export             = "Export"
  val exportSettings     = "Export Settings"

  def bool(b: Boolean): String = b match {
    case true  ⇒ "True"
    case false ⇒ "False"
  }

  def stringQueryPrefix(s: String): String = s match {
    case cyby.query.Contains   ⇒ "contains (case sensitive)"
    case cyby.query.ContainsCI ⇒ "contains"
    case s                     ⇒ s
  }

  def exportFormat(f: Format): String = f match {
    case Sdf ⇒ "MDL sdf-File"
    case Txt ⇒ "Text (tab delimited)"
    case Odf ⇒ "Open Document Format (can be read by Excel)"
  }

  def fingerprint(f: Fingerprint): String = f match {
    case Fingerprint.Default ⇒ "CDK Extended"
    case Fingerprint.PubChem ⇒ "PubChem"
    case Fingerprint.MACCS   ⇒ "MACCS"
  }

  def queryComp(c: QComp): String = c match {
    case QComp.And ⇒ "And"
    case QComp.Or  ⇒ "Or"
  }

  def editInfoField(f: EditInfo.Field): String = f match {
    case EditInfo.Summary   ⇒ "Last Modified (Summary)"
    case EditInfo.Timestamp ⇒ "Last Modified (Timestamp)"
    case EditInfo.UserId    ⇒ "Last Modified (User ID)"
    case EditInfo.UserName  ⇒ "Last Modified (User Name)"
  }

  def molField(f: Mol.Field): String = f match {
    case Mol.Structure      ⇒ "Structure"
    case Mol.ExactStructure ⇒ exactstructure
    case Mol.SubStructure   ⇒ substructure
    case Mol.Similarity     ⇒ similarity
    case Mol.NoStructure    ⇒ nostructure
    case Mol.Svg            ⇒ "SVG"
    case Mol.Inchi          ⇒ "InCHi"
    case Mol.Mass           ⇒ "Mass [g/mol]"
    case Mol.ExactMass      ⇒ "Exact Mass [g/mol]"
    case Mol.Formula        ⇒ "Molecular Formula"
    case Mol.LogP           ⇒ "logP"
    case Mol.Tpsa           ⇒ "TPSA"
    case Mol.Lipinski       ⇒ "Linpinski"
    case Mol.Smiles         ⇒ "SMILES"
  }

  def statsType(s: StatsType): String = s match {
    case MeanStat   ⇒ "Mean"
    case MinStat    ⇒ "Minimum"
    case MaxStat    ⇒ "Maximum"
    case MedianStat ⇒ "Median"
  }
}

// vim: set ts=2 sw=2 et:
