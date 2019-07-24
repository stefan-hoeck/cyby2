/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server

trait types {
  type IO[+A] = cats.effect.IO[A]

  type IStream[+A] = fs2.Stream[IO,A]

  type Ref[A] = cats.effect.concurrent.Ref[IO,A]

  type Out[-A] = A ⇒ IO[Unit]

  type Atom = cyby.chem.Atom

  type Element = cyby.chem.Element

  type Isotope = cyby.chem.Isotope

  type Molecule = cyby.chem.Molecule

  type Timestamp = Long

  type HttpService = org.http4s.HttpRoutes[IO]

  type Request = org.http4s.Request[IO]

  type Response = org.http4s.Response[IO]
}

// vim: set ts=2 sw=2 et:
