/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package msf

import cats.{Monad, Monoid}
import cats.data.WriterT
import cats.implicits._

/**
  * Convert between the writer monad and signal functions with an output
  * of pairs.
  */
object writer {
  def unWriter[F[_]:Monad,L:Monoid,A,B](msf: SF[WriterT[F,L,?],A,B])
    : SF[F,A,(L,B)] = SF[F,A,(L,B)]{ a ⇒
        msf.step(a).run.map {
          case (l,(b,m)) ⇒ (l,b) -> unWriter(m)
        }
      }

  def unWriterE[F[_]:Monad,L:Monoid,A,B](mef: EF[WriterT[F,L,?],A,B]): EF[F,A,(L,B)] =
    EF(unWriter(mef.sf) map { case (l,o) ⇒ o map (l -> _) })
}

// vim: set ts=2 sw=2 et:
