/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat
package example

import tagInstances._

class DataTest extends DataSuite with Generators {
  checkLaws("Met eqv", EqLaws[Method.Cli].eqv)
  checkLaws("Met json", JsonLaws[Method.Cli].fromToStripped)

  checkLaws("Fil eqv", EqLaws[Fil.Cli].eqv)
  checkLaws("Fil json", JsonLaws[Fil.Cli].fromToStripped)

  checkLaws("Pro eqv", EqLaws[Project.Cli].eqv)
  checkLaws("Pro json", JsonLaws[Project.Cli].fromToStripped)

  checkLaws("Sto eqv", EqLaws[Location.Cli].eqv)
  checkLaws("Sto json", JsonLaws[Location.Cli].fromToStripped)

  checkLaws("Use eqv", EqLaws[User.Cli].eqv)
  checkLaws("Use json", JsonLaws[User.Cli].fromToStripped)

  checkLaws("Bio eqv", EqLaws[BiodataEntry.Cli].eqv)
  checkLaws("Bio json", JsonLaws[BiodataEntry.Cli].fromToStripped)

  checkLaws("Con eqv", EqLaws[Container.Cli].eqv)
  checkLaws("Con json", JsonLaws[Container.Cli].fromToStripped)

  checkLaws("Cpd eqv", EqLaws[Compound.Cli].eqv)
  checkLaws("Cpd json", JsonLaws[Compound.Cli].fromToStripped)

  checkLaws("Sup eqv", EqLaws[Supplier.Cli].eqv)
  checkLaws("Sup json", JsonLaws[Supplier.Cli].fromToStripped)

  checkLaws("FilField eqv", EqLaws[FilField].eqv)
  checkLaws("FilField json", JsonLaws[FilField].fromToStripped)

  checkLaws("CpdField eqv", EqLaws[CpdField].eqv)
  checkLaws("CpdField json", JsonLaws[CpdField].fromToStripped)

  checkLaws("ConField eqv", EqLaws[ConField].eqv)
  checkLaws("ConField json", JsonLaws[ConField].fromToStripped)

  checkLaws("BioField eqv", EqLaws[BioField].eqv)
  checkLaws("BioField json", JsonLaws[BioField].fromToStripped)

  checkLaws("ExportField eqv", EqLaws[ExportField].eqv)
  checkLaws("ExportField json", JsonLaws[ExportField].fromToStripped)
}

// vim: set ts=2 sw=2 et:
