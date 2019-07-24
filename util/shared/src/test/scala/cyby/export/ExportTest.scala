/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package export

class ExportTest extends ExportSuite {
  checkLaws("Format eq", EqLaws[Format].eqv)
  checkLaws("Format read", ReadLaws[Format].fromTo)
  checkLaws("Format json", JsonLaws[Format].fromToStripped)

  checkLaws("Settings eq", EqLaws[Settings[Long,Long]].eqv)
  checkLaws("Settings json", JsonLaws[Settings[Int,Long]].fromToStripped)
}

