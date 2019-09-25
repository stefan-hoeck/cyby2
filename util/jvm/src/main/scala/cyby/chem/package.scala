/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby

import cats.implicits._

import org.openscience.cdk._
import org.openscience.cdk.config.Isotopes
import org.openscience.cdk.fingerprint.{Fingerprinter, ExtendedFingerprinter}
import org.openscience.cdk.inchi.InChIGeneratorFactory
import org.openscience.cdk.io.IChemObjectReader.Mode
import org.openscience.cdk.isomorphism.UniversalIsomorphismTester
import org.openscience.cdk.isomorphism.matchers.QueryAtomContainerCreator.createBasicQueryContainer
import org.openscience.cdk.layout.StructureDiagramGenerator
import org.openscience.cdk.qsar.descriptors.{molecular ⇒ MolDescs}
import org.openscience.cdk.qsar.{result ⇒ QSarResults}
import org.openscience.cdk.silent.SilentChemObjectBuilder
import org.openscience.cdk.smiles.{SmilesParser, SmilesGenerator, SmiFlavor}
import org.openscience.cdk.tools.manipulator.AtomContainerManipulator
import org.openscience.cdk.tools.manipulator.AtomContainerManipulator

import cyby.dat.{Formula,FormulaEntry,MolFile,Svg}
import java.io._

import scala.collection.JavaConverters._
import scala.collection.immutable.BitSet
import scala.util.control.NonFatal

/**
  * Types and utility functions for interacting with the algorithms
  * from the CDK.
  */
package object chem {
  type Atom = org.openscience.cdk.interfaces.IAtom

  type Element = org.openscience.cdk.interfaces.IElement

  type Isotope = org.openscience.cdk.interfaces.IIsotope

  type Molecule = org.openscience.cdk.interfaces.IAtomContainer

  /**
    * This ugly hack is necessary due to
    * https://github.com/scala/bug/issues/10501
    */
  private[chem] def cloneMol(m: Molecule): Molecule = {
    val met = m.getClass
               .getDeclaredMethods
               .toList
               .filter(_.getName === "clone")
               .head

    met.invoke(m).asInstanceOf[Molecule]
  }

//  @deprecated("use referential transparent versions", "0.2")
//  val builder = SilentChemObjectBuilder.getInstance
//
//  @deprecated("use referential transparent versions", "0.2")
//  val isotopes = Isotopes.getInstance()
//
//  @deprecated("use referential transparent versions", "0.2")
//  val FPSize = Fingerprinter.DEFAULT_SIZE
//
//  @deprecated("use referential transparent versions", "0.2")
//  val FPDepth = Fingerprinter.DEFAULT_SEARCH_DEPTH
//
//  @deprecated("use referential transparent versions", "0.2")
//  val hydrogen: Isotope = isotopes.getIsotope("H",1)
//  @deprecated("use referential transparent versions", "0.2")
//  val hMassExact: Double = hydrogen.getExactMass
//  @deprecated("use referential transparent versions", "0.2")
//  val hMassNatural: Double = isotopes getNaturalMass hydrogen
//
//  @deprecated("use referential transparent versions", "0.2")
//  def massNumber(a: Atom): Option[Integer] = Option(a.getMassNumber)
//
//  @deprecated("use referential transparent versions", "0.2")
//  def formulaEntries(a: Atom): List[FormulaEntry] = List(
//    massNumber(a).fold((0,a.getSymbol,1))(m ⇒ (m,a.getSymbol,1)),
//    (0,"H",a.getImplicitHydrogenCount)
//  )
//
//  @deprecated("use referential transparent versions", "0.2")
//  def formula(mol: Molecule): Res[Formula] = tryRes(
//    atoms(mol).flatMap(formulaEntries)
//              .groupBy{ case (i,s,_) ⇒ (i -> s)}
//              .toList
//              .map{ case ((i,s),ts) ⇒ (i,s,ts.map(_._3).sum) }
//              .sortWith(cyby.dat.Mol.hillSorter),
//    "formula"
//  )
//
//  @deprecated("use referential transparent versions", "0.2")
//  def atomMass(a: Atom): Double =
//    massNumber(a).fold(isotopes getNaturalMass a)(_ ⇒ a.getExactMass) +
//    a.getImplicitHydrogenCount * hMassNatural
//
//  @deprecated("use referential transparent versions", "0.2")
//  def exactMass(a: Atom): Double =
//    a.getExactMass + a.getImplicitHydrogenCount * hMassExact
//
//  @deprecated("use referential transparent versions", "0.2")
//  def configure(a: Atom): Res[Unit] = tryRes(
//    {
//      val sym = a.getSymbol
//      val iso = massNumber(a).fold(isotopes.getMajorIsotope(sym))(
//        isotopes.getIsotope(sym,_)
//      )
//      a.setExactMass(iso.getExactMass)
//      a.setAtomicNumber(iso.getAtomicNumber)
//    },
//    s"configure atom $a"
//  )
//
//  @deprecated("use referential transparent versions", "0.2")
//  def iso = new UniversalIsomorphismTester
//
//  @deprecated("use referential transparent versions", "0.2")
//  type Res[A] = ErrNel[Throwable,A]
//
//  @deprecated("use referential transparent versions", "0.2")
//  def adjustFromOldCyBy(s: String): Res[Molecule] = for {
//    mol <- readMol(s) <+> readSmiles(s)
//    _   <- if (hasNo2dCoords(mol)) generate2dCoords(mol).void else right(())
//    _   <- kekulize(mol)
//  } yield mol

//  @deprecated("use referential transparent versions", "0.2")
//  def readForQuery(s: String): Res[(Molecule, BitSet)] = for {
//    mol  <- readRawMol(s) <+> readRawSmiles(s)
//    mol2 <- readRawMol(s) <+> readRawSmiles(s)
//    _    <- detectAtomTypes(mol2)
//    _    <- detectAtomTypes(mol)
//    _    <- aromatize(mol2)
//    _    <- aromatize(mol)
//    fp   <- calcFingerprint(mol2)
//    q    <- tryRes(createBasicQueryContainer(mol), "createBasicQueryContainer")
//  } yield q -> fp
//
//  @deprecated("use referential transparent versions", "0.2")
//  def calcFingerprint(m: Molecule): Res[BitSet] = {
//    val fp = new ExtendedFingerprinter(FPSize, FPDepth)
//    tryRes(BitSet.fromBitMaskNoCopy(fp.getBitFingerprint(m).asBitSet.toLongArray), "fromBitMaskNoCopy")
//  }

//  /**
//    * Tries to read a string in .mol format. Afterwards,
//    * atom types and implicit hydrogens are calculated.
//    */
//  @deprecated("use referential transparent versions", "0.2")
//  def readMol(mol: String): Res[Molecule] = for {
//    m <- readRawMol(mol)
//    _ <- detectAtomTypes(m)
//    _ <- addImplicitHydrogens(m)
//    _ <- atoms(m) traverse_ configure
//  } yield m
//
//  /**
//    * Tries to read a string in .mol format. No further
//    * adjustments are made to the parsed molecules.
//    */
//  @deprecated("use referential transparent versions", "0.2")
//  def readRawMol(mol: String): Res[Molecule] = tryRes(
//    (new io.MDLV2000Reader(new StringReader(mol), Mode.RELAXED)).read(new AtomContainer),
//    "MDLV2000Reader.read"
//  )
//
//  /**
//    * Tries to read a string in SMILES format
//    */
//  @deprecated("use referential transparent versions", "0.2")
//  def readSmiles(mol: String): Res[Molecule] = for {
//    m <- readRawSmiles(mol)
//    _ <- detectAtomTypes(m)
//    _ <- addImplicitHydrogens(m)
//    _ <- atoms(m) traverse_ configure
//  } yield m
//
//  /**
//    * Tries to read a string in SMILES format
//    */
//  @deprecated("use referential transparent versions", "0.2")
//  def readRawSmiles(mol: String): Res[Molecule] = tryRes(
//    (new SmilesParser(SilentChemObjectBuilder.getInstance)).parseSmiles(mol),
//    "SmilesParser.parseSmiles"
//  )
//
//  /**
//    * Generates 2D coordinates for a molecule (which, for instances,
//    * comes froma SMILES string)
//    */
//  @deprecated("use referential transparent versions", "0.2")
//  def generate2dCoords(mol: Molecule): Res[Unit] = tryRes(
//    (new StructureDiagramGenerator).generateCoordinates(mol),
//    "StructureDiagramGenerator.generateCoordinates"
//  )
//
//  @deprecated("use referential transparent versions", "0.2")
//  def xlogP(mol: Molecule): Option[Double] = tryRes(
//    {
//      val desc = new MolDescs.XLogPDescriptor
//      val v = desc.calculate(mol).getValue
//      v.asInstanceOf[QSarResults.DoubleResult].doubleValue
//    },
//    "XLogPDescriptor.getValue"
//  ).toOption
//
//  @deprecated("use referential transparent versions", "0.2")
//  def tpsa(mol: Molecule): Option[Double] = tryRes(
//    {
//      val desc = new MolDescs.TPSADescriptor
//      val v = desc.calculate(mol).getValue
//      v.asInstanceOf[QSarResults.DoubleResult].doubleValue
//    },
//    "TPSADescriptor.getValue"
//  ).toOption
//
//  @deprecated("use referential transparent versions", "0.2")
//  def lipinski(mol: Molecule): Option[Boolean] = tryRes(
//    {
//      val desc = new MolDescs.RuleOfFiveDescriptor
//      val v = desc.calculate(mol).getValue
//      val failures = v.asInstanceOf[QSarResults.IntegerResult].intValue
//      failures === 0
//    }, "RuleOfFiveDescriptor.getValue"
//  ).toOption
//
//  @deprecated("use referential transparent versions", "0.2")
//  def molWeight(mol: Molecule): Res[Double] = 
//    tryRes(atoms(mol) map atomMass sum, "molWeight")
//
//  @deprecated("use referential transparent versions", "0.2")
//  def exactMass(mol: Molecule): Res[Double] = 
//    tryRes(atoms(mol) map exactMass sum, "exactMass")
//
//  @deprecated("use referential transparent versions", "0.2")
//  def addImplicitHydrogens(mol: Molecule): Res[Unit] = tryRes(
//    tools.CDKHydrogenAdder
//         .getInstance(SilentChemObjectBuilder.getInstance)
//         .addImplicitHydrogens(mol),
//    "CDKHydrogenAdder.addImplicitHydrogens"
//  )
//
//  @deprecated("use referential transparent versions", "0.2")
//  def addExplicitHydrogens(mol: Molecule): Res[Unit] =
//    addImplicitHydrogens(mol) *>
//    tryRes(AtomContainerManipulator.convertImplicitToExplicitHydrogens(mol), "AtomContainerManipulator.convertImplicitToExplicitHydrogens")
//
//  @deprecated("use referential transparent versions", "0.2")
//  def detectAtomTypes(mol: Molecule): Res[Unit] = tryRes(
//    {
//      val matcher = atomtype.CDKAtomTypeMatcher
//                            .getInstance(SilentChemObjectBuilder.getInstance)
//
//      atoms(mol) foreach { a ⇒ 
//        val tpe = matcher.findMatchingAtomType(mol, a)
//        tools.manipulator.AtomTypeManipulator.configure(a, tpe)
//      }
//    }, "CDKAtomTypeMatcher.findMatchingAtomType"
//  )
//
//  @deprecated("use referential transparent versions", "0.2")
//  def detectIsotopes(mol: Molecule): Res[Unit] = tryRes(
//    atoms(mol) foreach {a ⇒ isotopes.configure(a); ()},
//    "Isotopes.configure",
//  )
//
//  @deprecated("use referential transparent versions", "0.2")
//  def atoms(mol: Molecule): List[Atom] = mol.atoms.asScala.toList
//
//  @deprecated("use referential transparent versions", "0.2")
//  def hasNo2dCoords(mol: Molecule): Boolean =
//    tryRes(atoms(mol).exists(a ⇒ opt(a.getPoint2d.x).isEmpty), "getPoint2d") getOrElse false
//
//
//  @deprecated("use referential transparent versions", "0.2")
//  def writeMol(mol: Molecule): Res[MolFile] = tryRes(
//    {
//      val writer = new StringWriter
//      (new io.MDLV2000Writer(writer)).write(mol)
//      writer.flush()
//      val res = writer.toString.split("\n").toList map {
//        case l if l startsWith "  CDK" ⇒ "  CDK"
//        case l                         ⇒ l
//      }
//
//      MolFile.fromString(res mkString "\n")
//    }, "MDLV2000Writer.write"
//  )
//
//  @deprecated("use referential transparent versions", "0.2")
//  def kekulize(mol: Molecule): Res[Unit] =
//    tryRes(aromaticity.Kekulization.kekulize(mol), "Kekulization.kekulize")
//
//  @deprecated("use referential transparent versions", "0.2")
//  def aromatize(mol: Molecule): Res[Unit] =
//    tryRes({aromaticity.Aromaticity.cdkLegacy apply mol; () }, "Aromaticity.cdkLegacy")
//
//
//  @deprecated("use referential transparent versions", "0.2")
//  def toSvg(mol: Molecule): Res[Svg] = tryRes({
//    val gen = new org.openscience.cdk.depict.DepictionGenerator().withAtomColors()
//    val dep = gen.depict(mol)
//
//    Svg fromString dep.toSvgStr()
//  }, "DepictionGenerator.toSvgStr")
//
//  @deprecated("use referential transparent versions", "0.2")
//  def toInchi(mol: Molecule): Res[String] = tryRes(
//    InChIGeneratorFactory.getInstance.getInChIGenerator(mol).getInchi,
//    "InChIGeneratorFactory.getInchi"
//  )
//
//  @deprecated("use referential transparent versions", "0.2")
//  def toSmiles(mol: Molecule): Res[String] = tryRes({
//    val clone = AtomContainerManipulator.removeNonChiralHydrogens(mol)
//    (new SmilesGenerator(SmiFlavor.Absolute)).create(clone)
//  }, "SmilesGenerator.create")

  def tryRes[A](a: ⇒ A, str: String): ErrNel[Throwable,A] = try {
    val a2 = a
    if (a2 == null) left(new NullPointerException)
    else right(a2)
  } catch { case e@NonFatal(_) ⇒ {
      left(e)
  } }

  def opt[A](a: ⇒ A): Option[A] = try {
    if (a == null) None else Some(a)
  } catch { case NonFatal(_) ⇒ None }
}

// vim: set ts=2 sw=2 et:


