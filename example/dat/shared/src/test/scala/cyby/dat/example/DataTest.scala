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
  checkLaws("Met eqv", EqLaws[Met.Cli].eqv)
  checkLaws("Met json", JsonLaws[Met.Cli].fromToStripped)

  checkLaws("Fil eqv", EqLaws[Fil.Cli].eqv)
  checkLaws("Fil json", JsonLaws[Fil.Cli].fromToStripped)

  checkLaws("Pro eqv", EqLaws[Pro.Cli].eqv)
  checkLaws("Pro json", JsonLaws[Pro.Cli].fromToStripped)

  checkLaws("Sto eqv", EqLaws[Sto.Cli].eqv)
  checkLaws("Sto json", JsonLaws[Sto.Cli].fromToStripped)

  checkLaws("Use eqv", EqLaws[Use.Cli].eqv)
  checkLaws("Use json", JsonLaws[Use.Cli].fromToStripped)

  checkLaws("Bio eqv", EqLaws[Bio.Cli].eqv)
  checkLaws("Bio json", JsonLaws[Bio.Cli].fromToStripped)

  checkLaws("Con eqv", EqLaws[Con.Cli].eqv)
  checkLaws("Con json", JsonLaws[Con.Cli].fromToStripped)

  checkLaws("Sub eqv", EqLaws[Sub.Cli].eqv)
  checkLaws("Sub json", JsonLaws[Sub.Cli].fromToStripped)

  checkLaws("Sup eqv", EqLaws[Sup.Cli].eqv)
  checkLaws("Sup json", JsonLaws[Sup.Cli].fromToStripped)

  checkLaws("FilField eqv", EqLaws[FilField].eqv)
  checkLaws("FilField json", JsonLaws[FilField].fromToStripped)

  checkLaws("SubField eqv", EqLaws[SubField].eqv)
  checkLaws("SubField json", JsonLaws[SubField].fromToStripped)

  checkLaws("ConField eqv", EqLaws[ConField].eqv)
  checkLaws("ConField json", JsonLaws[ConField].fromToStripped)

  checkLaws("BioField eqv", EqLaws[BioField].eqv)
  checkLaws("BioField json", JsonLaws[BioField].fromToStripped)

  checkLaws("ExportField eqv", EqLaws[ExportField].eqv)
  checkLaws("ExportField json", JsonLaws[ExportField].fromToStripped)
}

// vim: set ts=2 sw=2 et:
