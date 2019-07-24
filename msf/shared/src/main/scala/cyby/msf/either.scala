/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package msf

import cats.Monad
import cats.data.EitherT
import cats.implicits._

object either {
  def apply[F[_]:Monad,E,A,B](msf: SF[F,A,Either[E,B]])
    : SF[EitherT[F,E,?],A,B] = SF[EitherT[F,E,?],A,B]{ a ⇒ 
      EitherT(msf step a map { case (eb,m) ⇒ eb map (_ -> apply(m)) } )
    }

  def catchET[F[_],E,A,B](msf: SF[EitherT[F,E,?],A,B])
    (otherwise: E ⇒ SF[F,A,B])(implicit F: Monad[F]): SF[F,A,B] =
    SF[F,A,B]{ a ⇒ 
      (msf step a).value flatMap {
        case Right((b,newMsf)) ⇒ F pure (b -> catchET(newMsf)(otherwise))
        case Left(e)           ⇒ otherwise(e) step a
      }
    }

  def catchE[F[_],E,A,B](msf: SF[F,A,Either[E,B]])
    (otherwise: E ⇒ SF[F,A,B])(implicit F: Monad[F]): SF[F,A,B] =
    SF[F,A,B]{ a ⇒ 
      msf step a flatMap {
        case (Right(b),newMsf) ⇒ F pure (b -> catchE(newMsf)(otherwise))
        case (Left(e), _)      ⇒ otherwise(e) step a
      }
    }
}

// vim: set ts=2 sw=2 et:
