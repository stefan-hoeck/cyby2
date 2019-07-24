/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package export

import org.scalacheck.Arbitrary
import org.scalacheck.Shapeless._

trait ExportImplicits extends TestImplicits {
  implicit def quryArb[C:Arbitrary,F:Arbitrary] = shapeless.the[Arbitrary[query.Query[C,F]]]
  implicit lazy val formatArb = shapeless.the[Arbitrary[Format]]
  implicit def settingsArb[C:Arbitrary,F:Arbitrary] =
    shapeless.the[Arbitrary[Settings[C,F]]]
}

object ExportImplicits extends ExportImplicits

// vim: set ts=2 sw=2 et:

