/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package msf

trait MsfSuite
  extends org.scalatest.PropSpec
  with org.scalatest.prop.PropertyChecks
  with org.scalatest.prop.Checkers
  with org.scalatest.Matchers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(
      minSize             = 0,
      sizeRange           = 50,
      minSuccessful       = 100,
      maxDiscardedFactor  = 5.0,
      workers             = 4
    )
}

// vim: set ts=2 sw=2 et:
