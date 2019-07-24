/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby

import cats.instances.all._

class ReadTest extends CyBySuite {
  checkLaws("Byte read", ReadLaws[Byte].fromTo)

  checkLaws("Short read", ReadLaws[Short].fromTo)

  checkLaws("Int read", ReadLaws[Int].fromTo)

  checkLaws("Long read", ReadLaws[Long].fromTo)

  checkLaws("BigInt read", ReadLaws[BigInt].fromTo)

  checkLaws("Boolean read", ReadLaws[Boolean].fromTo)

  checkLaws("Unit read", ReadLaws[Unit].fromTo)

  checkLaws("Char read", ReadLaws[Char].fromTo)

  checkLaws("String read", ReadLaws[Char].fromTo)
}

// vim: set ts=2 sw=2 et:
