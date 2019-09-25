/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat
package example

import cats.Eq, cats.implicits._
import io.circe.{Encoder, Decoder, KeyEncoder, KeyDecoder}
import cyby.dat.{format ⇒ F}

sealed trait ZHAWField {
  def canExport: Boolean
  def canQuery: Boolean
  def desc: F.ColumnDesc
  def inColumn: Boolean = desc =!= F.NoDesc
}

/**
  * Enum type representing fields of compounds. These
  * are used in queries, for sorting, for exporting, and
  * when defining the order and visibility of columns in
  * tabular views.
  */
sealed abstract class CpdField(
  val desc: F.ColumnDesc,
  val canExport: Boolean = true,
  val canQuery: Boolean = true,
)extends ZHAWField {
  def locName(l: Loc) = l subField this
  def ef: ExportField = ExportCpd(this)
}

case object CpdId         extends CpdField(F.strDesc("id"))
case object CpdName       extends CpdField(F.strDesc("name"))
case object CpdAbs        extends CpdField(F.strDesc("abs"))
case object CpdCasNr      extends CpdField(F.strDesc("casNr"))
case object CpdProject    extends CpdField(F.strDesc("project"))
case object CpdCreated    extends CpdField(F.strDesc("created"))
case object CpdContainers extends CpdField(F.NoDesc, false, false)

case class CpdMol(f: Mol.Field)
   extends CpdField(f.desc, f.canExport, f.canQuery) {
  override val toString = s"structure_${f.toString}"
}

case class CpdEditInfo(f: EditInfo.Field)
   extends CpdField(f.colDesc, f.canExport, f.canQuery) {
  override val toString = s"editInfo_${f.toString}"
}

case class CpdFil(f: FilField) extends CpdField(f.desc, false, f.canQuery) {
  override val toString = s"fil_${f.toString}"
  override def inColumn = false
}

object CpdField extends EnumHelper[CpdField] {
  val name = "cyby.dat.example.CpdField"
  lazy val values =
    Nel.of(CpdId, CpdName, CpdAbs, CpdCasNr, CpdProject, CpdCreated, CpdContainers) :::
    Mol.Field.values.map(CpdMol.apply) :::
    EditInfo.Field.values.map(CpdEditInfo.apply) :::
    FilField.values.map(CpdFil.apply)

  def encode(s: CpdField) = s match {
    case CpdMol(f)      ⇒ Mol.Field.encode(f)
    case CpdEditInfo(f) ⇒ EditInfo.Field.encode(f)
    case CpdFil(f)      ⇒ FilField encode f map ("fil_" ++ _)
    case sf             ⇒ lowerHeadDropEncode(sf, 3)
  }
}


/**
  * Enum type representing fields of containers. These
  * are used in queries, for sorting, for exporting, and
  * when defining the order and visibility of columns in
  * tabular views.
  */
sealed abstract class ConField(
  val desc: F.ColumnDesc = F.NoDesc,
  val canExport: Boolean = true,
  val canQuery: Boolean = true,
)extends ZHAWField {
  def locName(l: Loc) = l conField this
  def ef: ExportField = ExportCon(this)
}

case object ConId            extends ConField(F.strDesc("id"))
case object ConLocation      extends ConField(F.strDesc("location"))
case object ConSupplier      extends ConField(F.strDesc("supplier"))
case object ConBatch         extends ConField(F.strDesc("batch"))
case object ConOrderNr       extends ConField(F.strDesc("orderNr"))
case object ConComment       extends ConField(F.strDesc("comment"))
case object ConLentTo        extends ConField(F.strDesc("lentTo"))
case object ConPurity        extends ConField(F.dblDesc("purity"))
case object ConPurityStr     extends ConField(F.strDesc("purityStr"))
case object ConDensity       extends ConField(F.dblDesc("density"))
case object ConConcentration extends ConField(F.dblDesc("concentration"))
case object ConAmount        extends ConField(F.dblDesc("amount"))
case object ConEmpty         extends ConField(F.strDesc("empty"))
case object ConProject       extends ConField(F.strDesc("project"))
case object ConCreated       extends ConField(F.strDesc("created"))

case class ConEditInfo(f: EditInfo.Field)
extends ConField(f.colDesc, f.canExport, f.canQuery) {
  override val toString = s"editInfo_${f.toString}"
}

case class ConFil(f: FilField) extends ConField(f.desc, false, f.canQuery) {
  override val toString = s"fil_${f.toString}"
}

object ConField extends EnumHelper[ConField] {
  val name = "cyby.dat.example.ConField"
  lazy val values = Nel.of(ConId, ConLocation, ConSupplier, ConBatch, ConOrderNr, ConComment, ConLentTo, ConPurity, ConPurityStr, ConDensity, ConConcentration, ConAmount, ConEmpty, ConProject, ConCreated) :::
               EditInfo.Field.values.map(ConEditInfo.apply) :::
               FilField.values.map(ConFil.apply)

  def encode(c: ConField) = c match {
    case ConEditInfo(f) ⇒ EditInfo.Field.encode(f)
    case ConFil(f)      ⇒ FilField encode f map ("fil_" ++ _)
    case cf             ⇒ lowerHeadDropEncode(cf, 3)
  }
}


/**
  * Enum type representing fields of bio entries. These
  * are mainly used in queries and for exporting.
  */
sealed abstract class BioField(
  val desc: F.ColumnDesc = F.NoDesc,
  val canExport: Boolean = true,
  val canQuery: Boolean = true,
)extends ZHAWField {
  def locName(l: Loc) = l bioField this
  def ef: ExportField = ExportBio(this)
}

case object BioId       extends BioField()
case object BioValue    extends BioField()
case object BioMethod   extends BioField()
case object BioSupplier extends BioField()
case object BioDate     extends BioField()
case object BioComment  extends BioField()
case object BioProject  extends BioField()
case object BioCreated  extends BioField()

case class BioEditInfo(f: EditInfo.Field)
extends BioField(F.NoDesc, f.canExport, f.canQuery) {
  override val toString = s"editInfo_${f.toString}"
}

case class BioFil(f: FilField) extends BioField(f.desc, false, f.canQuery) {
  override val toString = s"fil_${f.toString}"
}



object BioField extends EnumHelper[BioField] {
  val name = "cyby.dat.example.BioField"
  lazy val values = Nel.of(BioId, BioValue, BioMethod, BioSupplier, BioDate, BioComment, BioProject, BioCreated) :::
               EditInfo.Field.values.map(BioEditInfo.apply) :::
               FilField.values.map(BioFil.apply)

  def encode(b: BioField) = b match {
    case BioEditInfo(f) ⇒ EditInfo.Field.encode(f)
    case BioFil(f)      ⇒ FilField encode f map ("fil_" ++ _)
    case bf             ⇒ lowerHeadDropEncode(bf, 3)
  }
}

/**
  * Enum type representing fields of bio entries. These
  * are mainly used in queries.
  */
sealed abstract class FilField(val desc: F.ColumnDesc, val canQuery: Boolean) {
  def locName(l: Loc) = l filField this
}

case object FilId         extends FilField(F.strDesc("id"), true)
case object FilName       extends FilField(F.strDesc("name"), true)
case object FilComment    extends FilField(F.strDesc("comment"), true)
case object FilPath       extends FilField(F.strDesc("path"), true)
case object FilProject    extends FilField(F.strDesc("project"), true)
case object FilCreated    extends FilField(F.strDesc("created"), true)

case class FilEditInfo(f: EditInfo.Field) extends FilField(f.colDesc, f.canQuery) {
  override val toString = s"editInfo_${f.toString}"
}

object FilField extends EnumHelper[FilField] {
  val name = "cyby.dat.example.FilField"
  lazy val values =
    Nel.of(FilId, FilName, FilPath, FilComment, FilProject, FilCreated) :::
    EditInfo.Field.values.map(FilEditInfo.apply)

  def encode(s: FilField) = s match {
    case FilEditInfo(f) ⇒ EditInfo.Field.encode(f)
    case sf             ⇒ lowerHeadDropEncode(sf, 3)
  }
}


/**
  * Sum type representing different types of fields in CyBy.
  * These are used for instance in queries, for sorting columns, and for
  * exporting values.
  *
  * @TODO: Rename to something more reasonable
  */
sealed abstract class ExportField(val tag: DataType) {
  def f: ExportField = this
  def desc: F.ColumnDesc
}

case class ExportCpd(fld: CpdField) extends ExportField(CpdT){
  override lazy val toString = s"sub:${CpdField show fld}"
  def desc = fld.desc
}

case class ExportCon(fld: ConField) extends ExportField(ConT){
  override lazy val toString = s"con:${ConField show fld}"
  def desc = fld.desc
}

case class ExportBio(fld: BioField) extends ExportField(BioT){
  override lazy val toString = s"bio:${BioField show fld}"
  def desc = fld.desc
}

case class ExportStats(mid: Method.Id, stat: StatsType)
  extends ExportField(StatsT){
  override lazy val toString = s"stats:${mid}:${stat}"
  def desc = F.StatsDesc
}

object ExportField {
  implicit lazy val eqI: Eq[ExportField] = Eq.fromUniversalEquals
  implicit lazy val encI: Encoder[ExportField] = Encoder[String].contramap(_.toString)
  implicit lazy val readI: Read[ExportField] =
    Read.inst(s ⇒ s"Invalid ExportField: $s")(read)

  implicit lazy val decI: Decoder[ExportField] = readI.decoder

  implicit lazy val keyEncI: KeyEncoder[ExportField] =
    new KeyEncoder[ExportField]{ def apply(a: ExportField) = a.toString }

  implicit lazy val keyDecI: KeyDecoder[ExportField] =
    new KeyDecoder[ExportField]{ def apply(s: String) = readI read s }

  def read(s: String): Option[ExportField] = s split ":" toList match {
    case "sub"::f::Nil ⇒ Read[CpdField].read(f) map ExportCpd.apply
    case "con"::f::Nil ⇒ Read[ConField].read(f) map ExportCon.apply
    case "bio"::f::Nil ⇒ Read[BioField].read(f) map ExportBio.apply
    case "stats"::m::s::Nil ⇒
      (Read[Method.Id].read(m), Read[StatsType].read(s)).mapN(ExportStats.apply)
    case _             ⇒ None
  }
}
