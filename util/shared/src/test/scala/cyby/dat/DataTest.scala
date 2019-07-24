/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

import cats.kernel.laws.discipline.OrderTests
import cats.instances.all._

class DataTest extends DataSuite {
  checkLaws("Amount ord", OrderTests[Amount].order)
  checkLaws("Amount read", ReadLaws[Amount].fromTo)
  checkLaws("Amount json", JsonLaws[Amount].fromToStripped)

  checkLaws("CasNr ord", OrderTests[CasNr].order)
  checkLaws("CasNr read", ReadLaws[CasNr].fromTo)
  checkLaws("CasNr json", JsonLaws[CasNr].fromToStripped)

  checkLaws("Concentration ord", OrderTests[Concentration].order)
  checkLaws("Concentration read", ReadLaws[Concentration].fromTo)
  checkLaws("Concentration json", JsonLaws[Concentration].fromToStripped)

  checkLaws("Density ord", OrderTests[Density].order)
  checkLaws("Density read", ReadLaws[Density].fromTo)
  checkLaws("Density json", JsonLaws[Density].fromToStripped)

  checkLaws("Name ord", OrderTests[Name].order)
  checkLaws("Name read", ReadLaws[Name].fromTo)
  checkLaws("Name json", JsonLaws[Name].fromToStripped)

  checkLaws("Password eq", OrderTests[Password].eqv)
  checkLaws("Password read", ReadLaws[Password].fromTo)
  checkLaws("Password json", JsonLaws[Password].fromToStripped)

  checkLaws("Percent ord", OrderTests[Percent].order)
  checkLaws("Percent read", ReadLaws[Percent].fromTo)
  checkLaws("Percent json", JsonLaws[Percent].fromToStripped)

  checkLaws("Plain ord", OrderTests[Plain].order)
  checkLaws("Plain read", ReadLaws[Plain].fromTo)
  checkLaws("Plain json", JsonLaws[Plain].fromToStripped)

  checkLaws("UserLevel ord", OrderTests[UserLevel].order)
  checkLaws("UserLevel read", ReadLaws[UserLevel].fromTo)
  checkLaws("UserLevel json", JsonLaws[UserLevel].fromToStripped)

  checkLaws("Id ord", OrderTests[Id[Int]].order)
  checkLaws("Id read", ReadLaws[Id[Int]].fromTo)
  checkLaws("Id json", JsonLaws[Id[Int]].fromToStripped)

  checkLaws("MolFile read", ReadLaws[MolFile].fromTo)
  checkLaws("MolFile json", JsonLaws[MolFile].fromToStripped)

  checkLaws("Svg read", ReadLaws[Svg].fromTo)
  checkLaws("Svg json", JsonLaws[Svg].fromToStripped)

  checkLaws("Mol.Field eqv", EqLaws[Mol.Field].eqv)
  checkLaws("Mol.Field read", ReadLaws[Mol.Field].fromTo)
  checkLaws("Mol.Field json", JsonLaws[Mol.Field].fromToStripped)

  checkLaws("EditInfo.Field eqv", EqLaws[EditInfo.Field].eqv)
  checkLaws("EditInfo.Field read", ReadLaws[EditInfo.Field].fromTo)
  checkLaws("EditInfo.Field json", JsonLaws[EditInfo.Field].fromToStripped)

  checkLaws("StatsType eqv", EqLaws[StatsType].eqv)
  checkLaws("StatsType read", ReadLaws[StatsType].fromTo)
  checkLaws("StatsType json", JsonLaws[StatsType].fromToStripped)

  property("usersettings localisation") {
    testLoc("usersettings", UserSettings.lbls.toList)
  }

  object loc extends cyby.dat.LocEnUS {
    def localMap = coreLocalMap
  }

  private def testLoc(typeName: String, symbols: List[Symbol]) =
    symbols foreach {s ⇒ assert(loc symbol s nonEmpty, s"${typeName} local: $s")}
}

// vim: set ts=2 sw=2 et:
