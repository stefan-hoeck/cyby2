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

  property("bio localisation") { testLoc("bio", BiodataEntry.lbls.toList) }

  property("con localisation") { testLoc("con", Container.lbls.toList) }

  property("fil localisation") { testLoc("fil", File.lbls.toList) }

  property("met localisation") { testLoc("met", Method.lbls.toList) }

  property("mol localisation") { testLoc("mol", Mol.lbls.toList) }

  property("pro localisation") { testLoc("pro", Project.lbls.toList) }

  property("sto localisation") { testLoc("sto", Location.lbls.toList) }

  property("sub localisation") { testLoc("sub", Compound.lbls.toList) }

  property("sup localisation") { testLoc("sup", Supplier.lbls.toList) }

  property("use localisation") { testLoc("use", User.lbls.toList) }

  private def testLoc(typeName: String, symbols: List[Symbol]) =
    symbols foreach {s ⇒ assert(loc symbol s nonEmpty, s"${typeName} local: $s")}
}

// vim: set ts=2 sw=2 et:
