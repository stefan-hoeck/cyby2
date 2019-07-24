/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package msf

import cats.Monad
import cats.data.Kleisli
import cats.implicits._

/**
  * Convert between the reader monad and signal functions with an input
  * of pairs.
  */
object reader {
  def apply[F[_]:Monad,E,A,B](msf: SF[F,(A,E),B])
    : SF[Kleisli[F,E,?],A,B] = SF[Kleisli[F,E,?],A,B] { a ⇒ 
      Kleisli( e ⇒ msf step (a -> e) map { case (b,m) ⇒ b -> apply(m) } )
    }

  def unReader[F[_]:Monad,E,A,B](msf: SF[Kleisli[F,E,?],A,B])
    : SF[F,(A,E),B] = SF[F,(A,E),B]{
        case (a,e) ⇒ msf step a run e map {
          case (b,m) ⇒ b -> unReader(m)
        }
      }

  def unReaderE[F[_]:Monad,E,A,B](mef: EF[Kleisli[F,E,?],A,B]): EF[F,(A,E),B] =
    EF(unReader(mef.sf))


  def unReader_[F[_]:Monad,E,B](msf: SF[Kleisli[F,E,?],Unit,B])
    : SF[F,E,B] = unReader(msf) contramap (() -> _)
}

// vim: set ts=2 sw=2 et:
