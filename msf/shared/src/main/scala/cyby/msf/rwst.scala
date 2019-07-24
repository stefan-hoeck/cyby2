/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package msf

import cats.{Monad, Monoid}
import cats.data.{WriterT, RWST}
import cats.implicits._

/**
  * Combines the effects of reader, writer and state.
  */
object rwst {
  def unRWS[F[_]:Monad,E,L:Monoid,S,A,B](msf: SF[RWST[F,E,L,S,?],A,B])
    : SF[F,((A,E),S), ((B,L),S)] = SF[F,((A,E),S),((B,L),S)]{
      case ((a,e),s) ⇒ msf.step(a).run(e,s) map {
        case (l,s,(b,m)) ⇒ ((b,l),s) -> unRWS(m)
      }
    }

  def unRS[F[_]:Monad,E,L:Monoid,S,A,B](msf: SF[RWST[F,E,L,S,?],A,B])
    : SF[WriterT[F,L,?],((A,E),S),(B,S)] = SF[WriterT[F,L,?],((A,E),S),(B,S)]{
      case ((a,e),s) ⇒ WriterT(msf.step(a).run(e,s) map {
        case (l,s,(b,m)) ⇒ l -> ((b -> s) -> unRS(m))
      })
    }

  def unRWSS[F[_]:Monad,E,L:Monoid,S,A,B](msf: SF[RWST[F,E,L,S,?],A,B])(ini: S)
    : SF[F,(A,E),(B,L)] = SF.loop(unRWS(msf))(ini)

  def unRSS[F[_]:Monad,E,L:Monoid,S,A,B](msf: SF[RWST[F,E,L,S,?],A,B])(ini: S)
    : SF[WriterT[F,L,?],(A,E),B] = SF.loop(unRS(msf))(ini)
}

// vim: set ts=2 sw=2 et:
