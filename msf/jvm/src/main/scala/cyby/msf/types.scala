/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package msf

trait types {
  type IO[+A] = cats.effect.IO[A]

  def ioNow[A](a: A): IO[A] = cats.effect.IO pure a

  def delay[A](a: ⇒ A): IO[A] = cats.effect.IO(a)

  def runIO[A](a: IO[A]): A = a.unsafeRunSync
}

// vim: set ts=2 sw=2 et:

