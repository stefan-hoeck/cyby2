/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat
package format

class ColorTest extends DataSuite {
  checkLaws("Color read", ReadLaws[Color].fromTo)
  checkLaws("Color json", JsonLaws[Color].fromTo)
}

// vim: set ts=2 sw=2 et:
