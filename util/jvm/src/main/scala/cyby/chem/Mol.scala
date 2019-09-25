/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package chem

import cats.data.EitherT
import cats.implicits.{none ⇒ _, _}

import cyby.dat.{Mol ⇒ DMol, Formula, MolFile, Svg}

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
  mol:         MolPure,
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

  def readE(m: MolFile): ErrNel[Throwable,Mol] = {
    def run[S]: MutableMol.ResST[S,Mol] = for {
      p        <- readMol[S](m) orElse readSmiles(m)
      (s2,mol) = p
      svg      <- MutableMol toSvg mol
      inchi    <- MutableMol toInchi mol
      fo       <- MutableMol formula mol
      m        <- MutableMol molWeight mol
      em       <- MutableMol exactMass mol
      _        <- MutableMol aromatize mol
      _        <- MutableMol addExplicitHydrogens mol
      fp       <- MutableMol calcFingerprint mol
      logp     <- MutableMol xlogP mol
      tpsa     <- MutableMol tpsa mol
      lip      <- MutableMol lipinski mol
      smi      <- MutableMol toSmiles mol
      mp       <- EitherT.liftF(mol.unsafeFreeze)
    } yield Mol(s2, svg, inchi, fp, mp, logp, tpsa, lip, smi, fo, m, em)

    ST.runST(new Forall[ErrNel[Throwable,Mol]]{def apply[S] = run[S].value})
  }

  def toDecE[A](e: Either[Nel[Throwable],A]): Either[DecodingFailure,A] =
    e.leftMap(n ⇒ DecodingFailure.fromThrowable(n.head,Nil))

  private def readMol[S](m: MolFile) =
    MutableMol.readMol[S](m.v).map (m -> _)

  def readSmiles[S](m: MolFile) = for {
    mol <- MutableMol.readSmiles[S](m.v)
    _   <- MutableMol.generate2dCoords(mol)
    _   <- MutableMol.kekulize(mol)
    m2  <- MutableMol.writeMol(mol)
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
  def toMol: Either[DecodingFailure,Mol] = {
    def run[S]: MutableMol.ResST[S,Mol] = for {
      mol      <- MutableMol.readMol[S](structure.v)
      fo       <- MutableMol formula mol
      m        <- MutableMol molWeight mol
      em       <- MutableMol exactMass mol
      _        <- MutableMol aromatize mol
      _        <- MutableMol addExplicitHydrogens mol
      mp       <- EitherT.liftF(mol.unsafeFreeze)
      smi      <- smiles.fold[MutableMol.ResST[S,Option[String]]](
                    MutableMol.toSmiles(mol)
                  )(s ⇒ MutableMol.pureRes[S,Option[String]](some(s)))
    } yield Mol(structure, svg, inchi, BitSet(fingerprint: _*), mp, logP, tpsa, lipinski, smiles, fo, m, em)

    Mol.toDecE(
      ST.runST(new Forall[ErrNel[Throwable,Mol]]{def apply[S] = run[S].value})
    )
  }
}

object MolRepr

// vim: set ts=2 sw=2 et:


