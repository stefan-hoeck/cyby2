/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

import cats.implicits._

import cyby.dat.{format ⇒ F}

import shapeless.{ ::, HNil, LabelledGeneric }
import shapeless.ops.record._

/**
  * Representation of molecules to be used at the client.
  */
@io.circe.generic.JsonCodec case class Mol(
  structure: MolFile,
  svg:       Svg,
  inchi:     String,
  mass:      Double,
  exactMass: Double,
  formula:   Formula,
  logP:      Option[Double],
  tpsa:      Option[Double],
  lipinski:  Option[Boolean],
  smiles:    Option[String],
) {
  lazy val formulaStr: String = formula map Mol.dispEntry mkString ""

  lazy val formulaHtml: String = formula map Mol.entryHtml mkString ""
}

object Mol {
  val exactStructure: String = "exactstructure"
  val subStructure: String = "substructure"

  val lblG = LabelledGeneric[Mol]
  val lbls@(structure::svg::inchi::mass::exactMass::formula::logP::tpsa::lipinski::smiles::HNil) = Keys[lblG.Repr].apply

  private def iso(mn: Int, sym: String, a: String, b: String): String = (mn,sym) match {
    case (0,s)   ⇒ s
    case (2,"H") ⇒ "D"
    case (3,"H") ⇒ "T"
    case (x,s)   ⇒ s"${a}${x}${b}${s}"
  }

  def dispIso(mn: Int, sym: String) = iso(mn, sym, "[", "]")

  def isoHtml(mn: Int, sym: String) = iso(mn, sym, "<sup>", "</sup>")

  lazy val dispEntry: FormulaEntry ⇒ String = {
    case (_,_,0) ⇒ ""
    case (x,s,1) ⇒ dispIso(x,s)
    case (x,s,n) ⇒ s"${dispIso(x,s)}${n}"
  }

  lazy val entryHtml: FormulaEntry ⇒ String = {
    case (_,_,0) ⇒ ""
    case (x,s,1) ⇒ isoHtml(x,s)
    case (x,s,n) ⇒ s"${isoHtml(x,s)}<sub>${n}</sub>"
  }

  val hillSorter: (FormulaEntry,FormulaEntry) ⇒ Boolean = (t1,t2) ⇒ {
    val (i1,h1,_) = t1
    val (i2,h2,_) = t2
    (h1,h2) match {
      case (a,b) if a === b ⇒ i1 < i2
      case ("C",_)          ⇒ true
      case (_,"C")          ⇒ false
      case ("H",_)          ⇒ true
      case (_,"H")          ⇒ false
      case (a,b)            ⇒ a < b
    }
  }

  sealed abstract class Field(
    val canExport: Boolean = true,
    val canQuery:  Boolean = true,
    val desc:      F.ColumnDesc = F.NoDesc
  ){
    def locName(l: Loc) = l molField this
  }

  case object Structure      extends Field(true, false, F.StructDesc)
  case object ExactStructure extends Field(false, true)
  case object SubStructure   extends Field(false, true)
  case object NoStructure    extends Field(false, true)
  case object Svg            extends Field(false, false)
  case object Smiles         extends Field()
  case object Inchi          extends Field()
  case object Mass           extends Field(desc = F.dblDesc("mass"))
  case object ExactMass      extends Field(desc = F.dblDesc("exactMass"))
  case object Formula        extends Field(desc = F.strDesc("formula"))
  case object LogP           extends Field(desc = F.dblDesc("logP"))
  case object Tpsa           extends Field(desc = F.dblDesc("tpsa"))
  case object Lipinski       extends Field(desc = F.strDesc("lipinski"))

  object Field extends EnumHelper[Field] {
    val name = "cyby.dat.Mol.Field"
    def encode(f: Field) = lowerHeadEncode(f)
    lazy val values = Nel.of(Structure, SubStructure, ExactStructure, NoStructure, Svg, Smiles, Inchi, Mass, ExactMass, Formula, LogP, Tpsa, Lipinski)
  }
}

// vim: set ts=2 sw=2 et:
