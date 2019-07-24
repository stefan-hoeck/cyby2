/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

import shapeless.{ ::, HNil, LabelledGeneric }
import shapeless.ops.record._

import cyby.dat.{format ⇒ F}

/**
  * Datatype holding information about when and by whom a data
  * item was last modified.
  */
@io.circe.generic.JsonCodec case class EditInfo(
  timestamp: TimeStamp,
  id:        Long,
  name:      Option[Name],
)

object EditInfo {
  val lblG = LabelledGeneric[EditInfo]
  val lbls@(timestamp::id::name::HNil) = Keys[lblG.Repr].apply

  implicit val eqI: cats.Eq[EditInfo] = cats.Eq.fromUniversalEquals

  /**
    * Datatype representing "fields" in an EditInfo object.
    *
    * Some of these can be used for sorting, querying, or exporting
    * pieces of information.
    */
  sealed abstract class Field(
    val colDesc:   F.ColumnDesc,
    val canExport: Boolean,
    val canQuery:  Boolean,
  ){
    def locName(l: Loc) = l editInfoField this
  }

  case object Timestamp extends Field(F.strDesc("editinfo_timestamp"), true, true)
  case object Summary   extends Field(F.strDesc("editinfo_summary"), true, false)
  case object UserId    extends Field(F.strDesc("editinfo_id"), true, false)
  case object UserName  extends Field(F.strDesc("editinfo_name"), true, true)

  object Field extends EnumHelper[Field] {
    val name = "cyby.dat.EditInfo.Field"
    def encode(f: Field) = Nel.of(f.toString, lowerHead(f.toString))
    lazy val values = Nel.of(Summary, Timestamp, UserId, UserName)
  }
}

// vim: set ts=2 sw=2 et:
