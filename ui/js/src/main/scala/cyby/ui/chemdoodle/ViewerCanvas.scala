/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui
package chemdoodle

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

/**
  * Compatibility trait for things with an "id" field
  */
@js.native
trait HasID extends js.Object {
  var id: String
}

object HasID {
  def get(o: js.Object): Option[String] =
    tryO(o.asInstanceOf[HasID].id)
}

/**
  * Wrapper trait for ChemDoodle.ViewerCanvas
  */
@js.native
@JSGlobal("ChemDoodle.ViewerCanvas")
class ViewerCanvas(id: String)
  extends js.Object {
  var specs: VisualSpecs = js.native

  def loadMolecule(m: Molecule): Unit = js.native
}

/**
  * Wrapper trait for ChemDoodle.SketcherCanvas
  */
@js.native
@JSGlobal("ChemDoodle.SketcherCanvas")
class SketcherCanvas(id: String, width: Int, height: Int, options: js.Object)
  extends js.Object {
  var specs: VisualSpecs = js.native

  def loadMolecule(m: Molecule): Unit = js.native

  def getMolecule(): Molecule = js.native

  def getMolecules(): js.Array[Molecule] = js.native

  def repaint(): Unit = js.native
}

/**
  * Wrapper trait for ChemDoodle.structures.VisualSpecifications
  */
@js.native
@JSGlobal("ChemDoodle.structures.VisualSpecifications")
class VisualSpecs() extends js.Object {
  var scale: Double = js.native
  var bonds_width_2D: Double = js.native
  var bonds_saturationWidthAbs_2D: Double = js.native
  var bonds_hashSpacing_2D: Double = js.native
  var atoms_font_size_2D: Int = js.native
  var atoms_displayTerminalCarbonLabels_2D: Boolean = js.native
  var atoms_useJMOLColors: Boolean = js.native
}

/**
  * Wrapper trait for the global ChemDoodle object
  */
@js.native
@JSGlobal("ChemDoodle")
object ChemDoodle extends js.Object {
  def readMOL(mol: String, mult: Double): Molecule = js.native
  def writeMOL(mol: Molecule): String = js.native
}

/**
  * Wrapper trait for ChemDoodle.structures.Molecule
  */
@js.native
@JSGlobal("ChemDoodle.structures.Molecule")
class Molecule extends js.Object {
  def scaleToAverageBondLength(v: Double): Unit = js.native
  def draw(context: js.Dynamic, specs: VisualSpecs): Unit = js.native
  var atoms: js.Array[Atom] = js.native
  var bonds: js.Array[Bond] = js.native
}

/**
  * Wrapper trait for ChemDoodle.structures.Atom
  */
@js.native
@JSGlobal("ChemDoodle.structures.Atom")
class Atom extends js.Object { }

/**
  * Wrapper trait for ChemDoodle.structures.Bon
  */
@js.native
@JSGlobal("ChemDoodle.structures.Bond")
class Bond extends js.Object { }

// vim: set ts=2 sw=2 et:
