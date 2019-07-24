/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package msf

import cats.Monad
import cats.data.StateT
import cats.implicits._

/**
  * Convert between stateful effects and explicit state passing
  * through signal functions of pairs.
  */
object state {
  def apply[F[_]:Monad,S,A,B](msf: SF[F,(A,S),(B,S)]): SF[StateT[F,S,?],A,B] = {
    type Eff[X] = StateT[F,S,X]
    type Paired = SF[Eff,A,(B,S)]
    
    def fst(m1: SF[F,(A,S),(B,S)]): Paired = SF[Eff,A,(B,S)](a ⇒ 
      StateT(s ⇒ m1 step (a -> s) map {
        case ((b,s),m2) ⇒ s -> ((b,s) -> fst(m2))
      })
    )

    fst(msf) mapF { case (b,s) ⇒ StateT.set[F,S](s) map (_ ⇒ b) }
  }

  def unState[F[_]:Monad,S,A,B](msf: SF[StateT[F,S,?],A,B])
    : SF[F,(A,S),(B,S)] =
        SF[F,(A,S),(B,S)]{
          case (a,e) ⇒ msf step a run e map {
            case (s,(b,m)) ⇒ (b -> s) -> unState(m)
          }
        }

  def unStateS[F[_]:Monad,S,A,B](msf: SF[StateT[F,S,?],A,B])(ini: S)
    : SF[F,A,B] = SF.loop(unState(msf))(ini)

  def unStateBack[F[_]:Monad,S,A,B](msf: SF[StateT[F,S,?],A,B])
    : SF[F,(A,S),B] = SF.loopBack(unState(msf))
}

// vim: set ts=2 sw=2 et:
