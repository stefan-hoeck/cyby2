/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package chem

import cats.data.EitherT
import cats.implicits.{none ⇒ _, _}

import cyby.dat.{Formula,FormulaEntry,MolFile,Svg}

import org.openscience.cdk._
import org.openscience.cdk.config.Isotopes
import org.openscience.cdk.fingerprint.{Fingerprinter, ExtendedFingerprinter, IBitFingerprint, MACCSFingerprinter, PubchemFingerprinter}
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

import java.io._

import scala.collection.JavaConverters._
import scala.collection.immutable.BitSet

case class MolPure(private val m: Molecule) {
  /**
    * Creates a deep copy of the enclosed molecule to
    * be used in further computations.
    */
  def thaw[S]: ST[S,MutableMol[S]] = ST returnST MutableMol[S](cloneMol(m))

  /**
    * Wraps the enclosed molecule in a MutableMol
    * without copying first. This method is to be used for
    * performance reasons only if you know what you are
    * doing as it might break referential transparency.
    */
  def unsafeThaw[S]: ST[S,MutableMol[S]] = ST returnST MutableMol[S](m)

  /**
    * Retruns the underlying molecule. This is for
    * testing reasons only as it breaks referential transparency.
    */
  private [chem] def unsafeGet: Molecule = m
}

final case class MutableMol[S](private val m: Molecule) {
  def cloneST: ST[S,MutableMol[S]] = ST returnST MutableMol[S](cloneMol(m))

  def freeze: ST[S,MolPure] = ST returnST MolPure(cloneMol(m))

  /**
    * Wraps the enclosed molecule in a MolPure
    * without copying first. This method is to be used for
    * performance reasons only if you know what you are
    * doing as it might break referential transparency.
    *
    * One use case, where this function is allowed:
    * When reading a molecule from a string representation
    * such as SMILES, making some adjustments to it
    * and finally wrapping it in a MolPure without using
    * the mutable reference in further computations,
    * calling this function is allowed. See for instance
    * the implementation of readForQuery.
    */
  def unsafeFreeze: ST[S,MolPure] = ST returnST MolPure(m)

  /**
    * Retruns the underlying molecule. This is for
    * testing reasons only as it breaks referential transparency.
    */
  def unsafeGet: Molecule = m
}

object MutableMol {
  
  type ResST[S,A] = EitherT[ST[S,?],Nel[Throwable],A]

  def pureRes[S,A](a: A): ResST[S,A] = EitherT(ST returnST Right(a))

  private val isotopes = Isotopes.getInstance()

  private val FPSize = Fingerprinter.DEFAULT_SIZE

  private val FPDepth = Fingerprinter.DEFAULT_SEARCH_DEPTH

  private val hydrogen: Isotope = isotopes.getIsotope("H",1)
  private val hMassExact: Double = hydrogen.getExactMass
  private val hMassNatural: Double = isotopes getNaturalMass hydrogen

  private def massNumber(a: Atom): Option[Integer] = Option(a.getMassNumber)

  private def formulaEntries(a: Atom): List[FormulaEntry] = List(
    massNumber(a).fold((0,a.getSymbol,1))(m ⇒ (m,a.getSymbol,1)),
    (0,"H",a.getImplicitHydrogenCount)
  )

  private def atomMass(a: Atom): Double =
    massNumber(a).fold(isotopes getNaturalMass a)(_ ⇒ a.getExactMass) +
    a.getImplicitHydrogenCount * hMassNatural

  private def exactMass(a: Atom): Double =
    a.getExactMass + a.getImplicitHydrogenCount * hMassExact

  def formula[S](mol: MutableMol[S]): ResST[S,Formula] = tryST(
    atoms(mol.m).flatMap(formulaEntries)
                .groupBy{ case (i,s,_) ⇒ (i -> s)}
                .toList
                .map{ case ((i,s),ts) ⇒ (i,s,ts.map(_._3).sum) }
                .sortWith(cyby.dat.Mol.hillSorter),
    "formula"
  )

  private def configure[S](a: Atom): ResST[S,Unit] = tryST(
    {
      val sym = a.getSymbol
      val iso = massNumber(a).fold(isotopes.getMajorIsotope(sym))(
        isotopes.getIsotope(sym,_)
      )
      a.setExactMass(iso.getExactMass)
      a.setAtomicNumber(iso.getAtomicNumber)
    },
    s"configure atom $a"
  )

  def adjustFromOldCyBy[S](s: String): ResST[S,MutableMol[S]] = for {
    mol <- readMol[S](s) orElse readSmiles(s)
    hc  <- hasNo2dCoords(mol)
    _   <- if (hc) generate2dCoords[S](mol).void else pureRes[S,Unit](())
    _   <- kekulize(mol)
  } yield mol

  def readForQuery[S](s: String): ResST[S,(MolPure, BitSet)] = for {
    mol  <- readRawMol[S](s) orElse readRawSmiles[S](s)
    _    <- detectAtomTypes[S](mol)
    _    <- aromatize(mol)
    fp   <- EitherT.liftF(mol.cloneST).flatMap(extendedFingerprint)
    q    <- tryST(MutableMol[S](createBasicQueryContainer(mol.m)), "createBasicQueryContainer")
    qr   <- EitherT liftF q.unsafeFreeze
  } yield qr -> fp

  /**
    * Prerequisite: atom types detected, aromatized
    */
  def extendedFingerprint[S](m: MutableMol[S]): ResST[S,BitSet] = {
    val fp = new ExtendedFingerprinter(FPSize, FPDepth)
    tryST(bitSet(fp.getBitFingerprint(m.m)), "extended fingerprinter")
  }

  /**
    * Generate MACCS fingerprint
    * Prerequisite: atom types detected, aromatized, implicit hydrogens
    */
  def maccs[S](m: MutableMol[S]): ResST[S,Option[BitSet]] = {
    val fp = new MACCSFingerprinter()
    optST(bitSet(fp.getBitFingerprint(m.m)), "maccs fingerprint")
  }
  /**
    * Generate Pubchem fingerprint
    * Prerequisite: atom types detected, aromatized, explicit hydrogens
    */
  def pubchem[S](m: MutableMol[S]): ResST[S,Option[BitSet]] = {
    val fp = new PubchemFingerprinter(null)
    optST(bitSet(fp.getBitFingerprint(m.m)), "pubchem fingerprint")
  }

  private def bitSet(fp: IBitFingerprint): BitSet =
    BitSet.fromBitMaskNoCopy(fp.asBitSet.toLongArray)

  /**
    * Tries to read a string in .mol format. Afterwards,
    * atom types and implicit hydrogens are calculated.
    */
  def readMol[S](mol: String): ResST[S,MutableMol[S]] = for {
    m <- readRawMol[S](mol)
    _ <- detectAtomTypes(m)
    _ <- addImplicitHydrogens(m)
    _ <- atoms(m.m) traverse_ (configure[S](_))
  } yield m

  /**
    * Tries to read a string in .mol format. No further
    * adjustments are made to the parsed molecules.
    */
  def readRawMol[S](mol: String): ResST[S,MutableMol[S]] = tryST(
    MutableMol((new io.MDLV2000Reader(new StringReader(mol), Mode.RELAXED)).read(new AtomContainer)),
    "MDLV2000Reader.read"
  )

  /**
    * Tries to read a string in SMILES format
    */
  def readSmiles[S](mol: String): ResST[S,MutableMol[S]] = for {
    m <- readRawSmiles(mol)
    _ <- detectAtomTypes(m)
    _ <- addImplicitHydrogens(m)
    _ <- atoms(m.m) traverse_ (configure[S](_))
  } yield m

  /**
    * Tries to read a string in SMILES format
    */
  def readRawSmiles[S](mol: String): ResST[S,MutableMol[S]] = tryST(
    MutableMol(new SmilesParser(SilentChemObjectBuilder.getInstance).parseSmiles(mol)),
    "SmilesParser.parseSmiles"
  )

  /**
    * Generates 2D coordinates for a molecule (which, for instances,
    * comes froma SMILES string)
    */
  def generate2dCoords[S](mol: MutableMol[S]): ResST[S,Unit] = tryST(
    (new StructureDiagramGenerator).generateCoordinates(mol.m),
    "StructureDiagramGenerator.generateCoordinates"
  )

  def xlogP[S](mol: MutableMol[S]): ResST[S,Option[Double]] = optST(
    {
      val desc = new MolDescs.XLogPDescriptor
      val v = desc.calculate(mol.m).getValue
      v.asInstanceOf[QSarResults.DoubleResult].doubleValue
    },
    "XLogPDescriptor.getValue"
  )

  def tpsa[S](mol: MutableMol[S]): ResST[S,Option[Double]] = optST(
    {
      val desc = new MolDescs.TPSADescriptor
      val v = desc.calculate(mol.m).getValue
      v.asInstanceOf[QSarResults.DoubleResult].doubleValue
    },
    "TPSADescriptor.getValue"
  )

  def lipinski[S](mol: MutableMol[S]): ResST[S,Option[Boolean]] = optST(
    {
      val desc = new MolDescs.RuleOfFiveDescriptor
      val v = desc.calculate(mol.m).getValue
      val failures = v.asInstanceOf[QSarResults.IntegerResult].intValue
      failures === 0
    }, "RuleOfFiveDescriptor.getValue"
  )

  def molWeight[S](mol: MutableMol[S]): ResST[S,Double] = 
    tryST(atoms(mol.m) map atomMass sum, "molWeight")

  def exactMass[S](mol: MutableMol[S]): ResST[S,Double] = 
    tryST(atoms(mol.m) map exactMass sum, "exactMass")

  def addImplicitHydrogens[S](mol: MutableMol[S]): ResST[S,Unit] = tryST(
    tools.CDKHydrogenAdder
         .getInstance(SilentChemObjectBuilder.getInstance)
         .addImplicitHydrogens(mol.m),
    "CDKHydrogenAdder.addImplicitHydrogens"
  )

  def addExplicitHydrogens[S](mol: MutableMol[S]): ResST[S,Unit] =
    addImplicitHydrogens(mol) *>
    tryST(AtomContainerManipulator.convertImplicitToExplicitHydrogens(mol.m), "AtomContainerManipulator.convertImplicitToExplicitHydrogens")

  def detectAtomTypes[S](mol: MutableMol[S]): ResST[S,Unit] = tryST(
    {
      val matcher = atomtype.CDKAtomTypeMatcher
                            .getInstance(SilentChemObjectBuilder.getInstance)

      atoms(mol.m) foreach { a ⇒ 
        val tpe = matcher.findMatchingAtomType(mol.m, a)
        tools.manipulator.AtomTypeManipulator.configure(a, tpe)
      }
    }, "CDKAtomTypeMatcher.findMatchingAtomType"
  )

  def detectIsotopes[S](mol: MutableMol[S]): ResST[S,Unit] = tryST(
    atoms(mol.m) foreach {a ⇒ isotopes.configure(a); ()},
    "Isotopes.configure",
  )

  private def atoms(mol: Molecule): List[Atom] = mol.atoms.asScala.toList

  def hasNo2dCoords[S](mol: MutableMol[S]): ResST[S,Boolean] =
    tryST(atoms(mol.m).exists(a ⇒ opt(a.getPoint2d.x).isEmpty), "getPoint2d")


  def writeMol[S](mol: MutableMol[S]): ResST[S,MolFile] = tryST(
    {
      val writer = new StringWriter
      (new io.MDLV2000Writer(writer)).write(mol.m)
      writer.flush()
      val res = writer.toString.split("\n").toList map {
        case l if l startsWith "  CDK" ⇒ "  CDK"
        case l                         ⇒ l
      }

      MolFile.fromString(res mkString "\n")
    }, "MDLV2000Writer.write"
  )

  def isSubgraph[S](mol: MutableMol[S], q: MutableMol[S]): ResST[S,Boolean] =
    tryST((new UniversalIsomorphismTester).isSubgraph(mol.m, q.m), "isSubgraph")

  def kekulize[S](mol: MutableMol[S]): ResST[S,Unit] =
    tryST(aromaticity.Kekulization.kekulize(mol.m), "Kekulization.kekulize")

  def aromatize[S](mol: MutableMol[S]): ResST[S,Unit] =
    tryST({aromaticity.Aromaticity.cdkLegacy apply mol.m; () }, "Aromaticity.cdkLegacy")

  def toSvg[S](mol: MutableMol[S]): ResST[S,Svg] = tryST({
    val gen = new org.openscience.cdk.depict.DepictionGenerator().withAtomColors()
    val dep = gen.depict(mol.m)

    Svg fromString dep.toSvgStr()
  }, "DepictionGenerator.toSvgStr")

  def toInchi[S](mol: MutableMol[S]): ResST[S,String] = tryST(
    InChIGeneratorFactory.getInstance.getInChIGenerator(mol.m).getInchi,
    "InChIGeneratorFactory.getInchi"
  )

  def toSmiles[S](mol: MutableMol[S]): ResST[S,Option[String]] = optST({
    val clone = AtomContainerManipulator.removeNonChiralHydrogens(mol.m)
    (new SmilesGenerator(SmiFlavor.Absolute)).create(clone)
  }, "SmilesGenerator.create")

  private def tryST[S,A](a: ⇒ A, str: String): ResST[S,A] = EitherT(
    ST(_ -> tryRes(a,str))
  )

  private def optST[S,A](a: ⇒ A, str: String): ResST[S,Option[A]] =
    tryST[S,A](a,str).map(some).orElse(pureRes[S,Option[A]](none))
}

