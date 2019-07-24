/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat
package example

class LocalTest extends DataSuite {
  object loc extends LocEnUS

  property("bio localisation") { testLoc("bio", Bio.lbls.toList) }

  property("con localisation") { testLoc("con", Con.lbls.toList) }

  property("fil localisation") { testLoc("fil", Fil.lbls.toList) }

  property("met localisation") { testLoc("met", Met.lbls.toList) }

  property("mol localisation") { testLoc("mol", Mol.lbls.toList) }

  property("pro localisation") { testLoc("pro", Pro.lbls.toList) }

  property("sto localisation") { testLoc("sto", Sto.lbls.toList) }

  property("sub localisation") { testLoc("sub", Sub.lbls.toList) }

  property("sup localisation") { testLoc("sup", Sup.lbls.toList) }

  property("use localisation") { testLoc("use", Use.lbls.toList) }

  private def testLoc(typeName: String, symbols: List[Symbol]) =
    symbols foreach {s ⇒ assert(loc symbol s nonEmpty, s"${typeName} local: $s")}
}

// vim: set ts=2 sw=2 et:
