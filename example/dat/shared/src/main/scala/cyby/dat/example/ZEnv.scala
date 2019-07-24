/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat
package example

/**
  * Wrapper trait providing type aliases and typeclass instances
  * for data types used throughout this example implementation.
  * Many classes and objects in the server and ui module inherit
  * from this trait.
  */
trait ZEnv extends CyByEnv {
  val version = "220"
  type Use      = cyby.dat.example.Use.Cli
  type Result   = cyby.dat.example.Result
  type Column   = cyby.dat.example.ExportField
  type Field    = cyby.dat.example.ExportField
  type Path     = cyby.dat.example.Path
  type DataType = cyby.dat.example.DataType
  type OtherSettings = Unit

  override def pathRead          = implicitly
  override def dataTypeRead      = implicitly
  override def rootPath          = RootP.path
  override def subPath(id: Long) = SubP(Id[Sub.type](id)::shapeless.HNil)
  override def subType: DataType = SubT
  override def filType: DataType = FilT
  override def useDecoder        = implicitly
  override def useEncoder        = implicitly
  override def useEq             = implicitly
  override def resDecoder        = implicitly
  override def resEncoder        = implicitly
  override def colEncoder        = implicitly
  override def colRead           = implicitly
  override def colDecoder        = implicitly
  override def colKeyEncoder     = implicitly
  override def colKeyDecoder     = implicitly
  override def colEq             = implicitly
  override def fldEq             = implicitly
  override def fldEncoder        = implicitly
  override def fldDecoder        = implicitly
  override def settingsEncoder   = implicitly
  override def settingsDecoder   = implicitly

  override def alias(u: Use) = u.alias.v
  override def userId(u: Use) = u.id.v
  override def level(u: Use) = u.level.v
}
