/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

import cats.Eq
import cyby.dat.format.Format
import cyby.query.{Q, QuickSearch, Query}

import io.circe.{Encoder, Decoder, KeyEncoder, KeyDecoder}
import io.circe.generic.semiauto._

import shapeless.{ ::, HNil, LabelledGeneric }
import shapeless.ops.record._

/**
  * Class representing user settings to be persisted at
  * the server.
  *
  * @tpara C: column type used to sort queries
  * @tpara F: field type used in exports and queries
  * @tpara A: custom data type for additional user settings
  */
case class UserSettings[C,F,A](
  /** actual list of columns in the expandable table view */
  substanceColumns : List[C],

  /** actual list of columns in the statistics table view */
  methodColumns    : List[C],

  /** defined formatting rules for string types */
  stringFormats    : Map[C,Format[String]],

  /** defined formatting rules for floating point values */
  doubleFormats    : Map[C,Format[Double]],

  /** defined formatting rules for integers */
  integerFormats   : Map[C,Format[Long]],

  /** defined formatting rules for integers */
  booleanFormats   : Map[C,Format[Boolean]],

  /** latest list of exported fields */
  exportFieldsO    : Option[List[F]],

  /** format of last export */
  exportFormat     : Option[export.Format],

  /** whether last export was limited to selected entries */
  exportSelectionO : Option[Boolean],

  /** last combined query used */
  queryO           : Option[Q[F]],

  /** last quick search used */
  quickO           : Option[QuickSearch],

  /** last full query definition */
  lastQueryO       : Option[Query[C,F]],

  /** list of stored combined queries */
  queriesO         : Option[List[(Name,Q[F])]],

  /** custom settings */
  otherO           : Option[A],
){
  def exportFields: List[F] = exportFieldsO getOrElse Nil

  def exportSelection: Boolean = exportSelectionO getOrElse false

  def queryPairs: List[(Name,Q[F])] = queriesO map (_.sortBy(_._1.v)) getOrElse Nil

  def queries: List[Q[F]] = queryPairs map (_._2)
}

object UserSettings {
  implicit def eqI[C,F,A]: Eq[UserSettings[C,F,A]] = Eq.fromUniversalEquals

  def empty[C,F,A]: UserSettings[C,F,A] =
    UserSettings[C,F,A](Nil,Nil,Map.empty,Map.empty,Map.empty,Map.empty, None, None, None, None, None, None, None, None)

  val lblG = LabelledGeneric[UserSettings[String,String,String]]
  val lbls@(substanceColumns::methodColumns::stringFormats::doubleFormats::integerFormats::booleanFormats::exportFields::exportFormat::exportSelection::query::quick::lastQuery::queries::other::HNil) = Keys[lblG.Repr].apply

  implicit def encI[C:KeyEncoder:Encoder,F:Encoder,A:Encoder]
    : Encoder[UserSettings[C,F,A]] = deriveEncoder

  implicit def decI[C:KeyDecoder:Decoder,F:Decoder,A:Decoder]
    : Decoder[UserSettings[C,F,A]] = deriveDecoder
    
}
