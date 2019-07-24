/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package chem

import cats.implicits.{none ⇒ _, _}

import cyby.dat.{Mol ⇒ DMol, Formula, MolFile, Svg}
import cyby.query.{ReadPred ⇒ RP}

import io.circe.{Decoder, Encoder, HCursor, DecodingFailure}
import io.circe.generic.JsonCodec

import scala.collection.immutable.BitSet

/**
  * Data type representing molecules as seen at the server.
  * Many properties are precalculated and stored on disk for
  * reasons of efficiency.
  */
case class Mol(
  structure:   MolFile,
  svg:         Svg,
  inchi:       String,
  fingerprint: BitSet,
  mol:         Molecule,
  logP:        Option[Double],
  tpsa:        Option[Double],
  lipinski:    Option[Boolean],
  smiles:      Option[String],
  formula:     Formula,
  mass:        Double,
  exactMass:   Double,
){
  lazy val formulaStr: String = formula map DMol.dispEntry mkString ""

  def toRepr: MolRepr = MolRepr(
    structure,
    svg,
    inchi,
    fingerprint.toList,
    logP, tpsa, lipinski, smiles
  )

  def isomorph(m: Molecule, fp2: BitSet): Boolean =
    fp2.subsetOf(fingerprint) && chem.iso.isIsomorph(mol, m)

  def hasSubgraph(m: Molecule, fp2: BitSet): Boolean =
    fp2.subsetOf(fingerprint) && isSubgraph(m)

  def isSubgraph(m: Molecule): Boolean = chem.iso.isSubgraph(mol, m)

  def toDatMol: DMol = DMol(structure, svg, inchi, mass, exactMass, formula, logP, tpsa, lipinski, smiles)

  override lazy val toString = structure.toString

  override def equals(v: Any): Boolean = v match {
    case m: Mol ⇒ inchi == m.inchi
    case _      ⇒ false
  }

  override def hashCode = inchi.hashCode
}

object Mol {
  implicit val eqI: cats.Eq[Mol] = cats.Eq.fromUniversalEquals

  implicit val readI: Read[Mol] = Read.inst(s ⇒ s"Not a Molecule: $s")(s ⇒ read(s))

  def read(s: String): Option[Mol] =
    readE(MolFile fromString MolFile.unescape(s)).fold[Option[Mol]](
      nel ⇒ {
        nel.toList foreach{ e ⇒  println(s"Error when reading molecule ${s}: ${e}") }
        none[Mol]
      }, some(_)
    )

  def readE(m: MolFile): ErrNel[Throwable,Mol] = for {
    p        <- readMol(m) <+> readSmiles(m)
    (s2,mol) = p
    svg      <- chem toSvg mol
    inchi    <- chem toInchi mol
    fo       <- chem formula mol
    m        <- chem molWeight mol
    em       <- chem exactMass mol
    _        <- chem aromatize mol
    _        <- chem addExplicitHydrogens mol
    fp       <- chem calcFingerprint mol
  } yield Mol(s2, svg, inchi, fp, mol, chem xlogP mol, chem tpsa mol, chem lipinski mol, chem.toSmiles(mol).toOption, fo, m, em)

  def toDecE[A](e: Either[Nel[Throwable],A]): Either[DecodingFailure,A] =
    e.leftMap(n ⇒ DecodingFailure.fromThrowable(n.head,Nil))

  private def readMol(m: MolFile) =
    chem readMol m.v map (m -> _)

  def readSmiles(m: MolFile) = for {
    mol <- chem readSmiles m.v
    _   <- chem generate2dCoords mol
    _   <- chem kekulize mol
    m2  <- chem writeMol mol
  } yield m2 -> mol

  implicit lazy val encI: Encoder[Mol] =
    Encoder[MolRepr] contramap (_.toRepr)

  implicit lazy val decI: Decoder[Mol] = new Decoder[Mol]{
    def apply(h: HCursor) =
      h.get[MolFile]("raw").flatMap(s ⇒ toDecE(readE(s))) <+>
      Decoder[MolRepr].apply(h).flatMap(_.toMol)
  }
}

@JsonCodec case class MolRepr(
  structure:   MolFile,
  svg:         Svg,
  inchi:       String,
  fingerprint: List[Int],
  logP:        Option[Double],
  tpsa:        Option[Double],
  lipinski:    Option[Boolean],
  smiles:      Option[String],
){
  def toMol: Either[DecodingFailure,Mol] = Mol.toDecE(
    for {
      mol <- chem readMol structure.v
      fo  <- chem formula mol
      m   <- chem molWeight mol
      em  <- chem exactMass mol
      _   <- chem aromatize mol
      _   <- chem addExplicitHydrogens mol
      smi = smiles orElse chem.toSmiles(mol).toOption
    } yield Mol(structure, svg, inchi, BitSet(fingerprint: _*), mol, logP, tpsa, lipinski, smi, fo, m, em)
  )
}

object MolRepr

// vim: set ts=2 sw=2 et:


