/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

import cats.Eq
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

/**
  * Environment used to define nested types and traits at the server
  * and in the client. Specifies types used to represent users, server
  * results, errors and other things together with utility functions
  * and type class instances. See the example application for ways
  * to use this trait.
  */
trait CyByEnv {
  def version: String


  /**
    * User type.
    */
  type Use

  def level(u: Use): UserLevel

  def alias(u: Use): Name

  def userId(u: Use): Long

  def useEncoder: Encoder[Use]

  def useDecoder: Decoder[Use]

  def useEq: Eq[Use]



  /** Type of server results to be sent to the client **/
  type Result

  def resDecoder: Decoder[Result]

  def resEncoder: Encoder[Result]



  /** 
    *  Columns displayed in tabular views in the UI 
    *
    *  Invariant: colRead(c.toString) == Some(c)
    */
  type Column

  def colRead   : Read[Column]

  def colEncoder: Encoder[Column]

  def colDecoder: Decoder[Column]

  def colKeyEncoder: KeyEncoder[Column]

  def colKeyDecoder: KeyDecoder[Column]

  def colEq: Eq[Column]

  object Col {
    def unapply(s: String): Option[Column] = colRead read s
  }


  /** 
    *  Additional user settings stored at the server
    */
  type OtherSettings

  def settingsEncoder: Encoder[OtherSettings]

  def settingsDecoder: Decoder[OtherSettings]



  /** 
    *  Fields of data types
    */
  type Field

  def fldEncoder: Encoder[Field]

  def fldDecoder: Decoder[Field]

  def fldEq: Eq[Field]

  type Query = cyby.query.Query[Column,Field]



  /** 
    *  Enum representing types in the data tree
    *
    *  Note, that for any DataType d, d.toString must not
    *  contain any hyphens since these string representations will
    *  be used in element IDs (see als cyby.ui.UId).
    *
    *  Invariant: dataTypeRead(d.toString) == Some(d)
    */
  type DataType

  def dataTypeRead: Read[DataType]

  def subType: DataType

  def filType: DataType



  /** 
    *  Paths to data objects in heterogeneous data trees
    *
    *  Note, that for any Path p, p.toString must not
    *  contain any hyphens since these string representations will
    *  be used in element IDs (see als cyby.ui.UId).
    *
    *  Invariant: pathRead(p.toString) == Some(p)
    */
  type Path

  def rootPath: Path

  def pathRead: Read[Path]

  def subPath(id: Long): Path

  object Path {
    def unapply(s: String): Option[Path] = pathRead read s
  }

  type USettings   = cyby.dat.UserSettings[Column,Field,OtherSettings]

  type ExpSettings = cyby.export.Settings[Column,Field]


  /**
    * Type class instances provided for the types above.
    *
    * To bring these into implicit scope: import this.implicits._
    */
  object implicits {
    implicit lazy val resDecI: Decoder[Result] = resDecoder
    implicit lazy val colDecI: Decoder[Column] = colDecoder
    implicit lazy val useDecI: Decoder[Use]    = useDecoder
    implicit lazy val setDecI: Decoder[OtherSettings] = settingsDecoder
    implicit lazy val colKeyDecI: KeyDecoder[Column] = colKeyDecoder
    implicit lazy val fldDecI: Decoder[Field]  = fldDecoder

    implicit lazy val resEncI: Encoder[Result] = resEncoder
    implicit lazy val colEncI: Encoder[Column] = colEncoder
    implicit lazy val useEncI: Encoder[Use] = useEncoder
    implicit lazy val colKeyEncI: KeyEncoder[Column] = colKeyEncoder
    implicit lazy val fldEncI: Encoder[Field]  = fldEncoder
    implicit lazy val setEncI: Encoder[OtherSettings] = settingsEncoder

    implicit lazy val colEqI:  Eq[Column]      = colEq
    implicit lazy val useEqI:  Eq[Use]      = useEq
    implicit lazy val fldEqI:  Eq[Field]      = fldEq

    implicit lazy val dtReadI: Read[DataType] = dataTypeRead
    implicit lazy val pReadI: Read[Path]      = pathRead
  }
}
