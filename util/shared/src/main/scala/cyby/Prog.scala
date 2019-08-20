/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby

import cats.{Functor,Monad,Eval}, cats.implicits._
import cats.arrow.FunctionK

/*
 * Effectful, stateful programs with logging and the possibility of
 * failure.
 *
 * F[_] is the monadic effect, in which the program runs (typically
 * something involving IO), E is the required environment
 * (dependency injection using the reader monad),
 * S is the state the program requires as input and returns as output,
 * Err is the type of error the program can yield, and A is the
 * program's return type if all goes well.
 * Finally, the program supports logging (type L). In case of an
 * error, logs are returned together with all errors.
 *
 * @tparam F: the effect the computation runs in
 * @tparam E: the required environment (read only)
 * @tparam L: the type of logs collected when running the program
 * @tparam S: the state mutated by the progrum
 * @tparam Err: type of errors the program can throw
 * @tparam A: the program's return type if all goes well
 */
case class ProgT[F[_],E,L,S,Err,A](
  run: (E,List[L],S) ⇒ F[Either[(Nel[Err],List[L]),(List[L],S,A)]]
){
  def trans[G[_]](f: FunctionK[F,G]): ProgT[G,E,L,S,Err,A] =
    ProgT((e,ls,s) ⇒ f apply run(e,ls,s))

  def map[B](f: A ⇒ B)(implicit F: Functor[F]): ProgT[F,E,L,S,Err,B] =
    ProgT((e,l,s) ⇒ run(e,l,s).map(_.map{ case (l2,s2,a) ⇒ (l2,s2,f(a))}))

  def flatMap[B](f: A ⇒ ProgT[F,E,L,S,Err,B])(implicit F: Monad[F])
    : ProgT[F,E,L,S,Err,B] = ProgT((e,l,s) ⇒ run(e,l,s).flatMap{
      case Left(p)          ⇒ F pure Left(p)
      case Right((l2,s2,a)) ⇒ f(a).run(e,l2,s2)
    })

  def onErr(f: Nel[Err] ⇒ ProgT[F,E,L,S,Err,A])(implicit F: Monad[F]): ProgT[F,E,L,S,Err,A] =
    ProgT((e,l,s) ⇒ run(e,l,s).flatMap{
      case Left((es,l1)) ⇒ f(es).run(e,l1,s)
      case Right(t)      ⇒ F pure Right(t)
    })

  def cmapEnv[E2](f: E2 ⇒ E): ProgT[F,E2,L,S,Err,A] = ProgT((e2,ls,s) ⇒ run(f(e2),ls,s))

  def mapSt[S2](f: S2 ⇒ S, g: (S2,S) ⇒ S2)(implicit F: Monad[F]): ProgT[F,E,L,S2,Err,A] =
    ProgT((e,ls,s2) ⇒ run(e,ls,f(s2)).map(_ map { case (ls2,s,a) ⇒ (ls2,g(s2,s),a) }))
    

  def or(p: ⇒ ProgT[F,E,L,S,Err,A])(implicit F: Monad[F]): ProgT[F,E,L,S,Err,A] =
    onErr(_ ⇒ p)

  def <|>(p: ⇒ ProgT[F,E,L,S,Err,A])(implicit F: Monad[F]): ProgT[F,E,L,S,Err,A] = or(p)
}

object ProgT {
  implicit def monadI[F[_],E,L,S,Err](implicit F: Monad[F]): Monad[ProgT[F,E,L,S,Err,?]] =
    new Monad[ProgT[F,E,L,S,Err,?]]{
      def pure[A](a: A): ProgT[F,E,L,S,Err,A] = ProgT((_,l,s) ⇒ F.pure(Right((l,s,a))))
      def flatMap[A,B](fa: ProgT[F,E,L,S,Err,A])(f: A ⇒ ProgT[F,E,L,S,Err,B]) = fa flatMap f
      def tailRecM[A, B](a: A)(f: A ⇒ ProgT[F,E,L,S,Err,Either[A,B]]): ProgT[F,E,L,S,Err,B] = ProgT{(c,l,s) ⇒
        F.tailRecM((l,s,a)){ case (ln,sn,an) ⇒
          F.map(f(an).run(c,ln,sn)) {
            case Left(l)                 ⇒ Right(Left(l))
            case Right((l1,s1,Left(a1))) ⇒ Left((l1,s1,a1))
            case Right((l1,s1,Right(b))) ⇒ Right(Right((l1,s1,b)))
          }
        }
      }
    }
}

abstract class ProgHelper[F[_],Conf,St,L,Err](implicit F: Monad[F]) {
  type Errs = Nel[Err]

  type Logs = List[L]

  type Prog[A] = ProgT[F,Conf,L,St,Err,A]

  lazy val monad: Monad[Prog] = Monad[Prog]

  // *** Wrapping and lifing related types into the Prog Monad *** //
  
  def pure[A](a: A): Prog[A] = monad pure a

  def wrap[A](f: Conf ⇒ St ⇒ F[Either[Errs,(St,A)]]): Prog[A] =
    ProgT((c,l,s) ⇒ f(c)(s) map {
      case Right((s1,a)) ⇒ Right((l,s1,a))
      case Left(es)      ⇒ Left((es,l))
    })

  def lift[A](f: F[A]): Prog[A] = ProgT((_,l,s) ⇒ f map (a ⇒ Right((l,s,a))))
  
  def wrapPure[A](f: Conf ⇒ St ⇒ Either[Errs,(St,A)]): Prog[A] =
    wrap[A](c ⇒ s ⇒ F pure f(c)(s))

  def wrapEval[A](f: Conf ⇒ St ⇒ Eval[Either[Errs,(St,A)]]): Prog[A] =
    wrap[A](c ⇒ s ⇒ F pure f(c)(s).value)

  def wrapEither[A](e: Either[Errs,A]): Prog[A] =
    wrapPure[A](_ ⇒ s ⇒ e map (s -> _))

  def wrapValidated[A](v: Validated[Errs,A]): Prog[A] = wrapEither(v.toEither)

  def chain[E,A](pr: Prog[E])(cp: ProgT[F,E,L,St,Err,A]): Prog[A] =
    ProgT((c,l,s) ⇒ pr.run(c,l,s).flatMap{
      case Left(x)          ⇒ F pure Left(x)
      case Right((l1,s1,e)) ⇒ cp.run(e,l1,s1)
    })
    

  // *** Working with errors and stuff that might fail *** //

  def raise[A](e: Err): Prog[A] = raiseErrors(Nel of e)
  
  def raiseErrors[A](e: Errs): Prog[A] = wrapPure[A](_ ⇒ _ ⇒ Left(e))

  def check[A](p: Boolean, e: ⇒ Errs, a: ⇒ A): Prog[A] =
    if (p) pure(a) else raiseErrors(e)
  
  def check_(p: Boolean, e: ⇒ Errs): Prog[Unit] = check(p, e, ())
  
  def opt[A](e: ⇒ Errs): Option[A] ⇒ Prog[A] = _.fold(raiseErrors[A](e))(pure)


  // *** Working with State *** //

  lazy val st: Lens[St,St] = lens[St]

  def get: Prog[St] = ProgT((_,l,s) ⇒ F pure Right((l,s,s)))

  def set(s: St): Prog[Unit] = modify(_ ⇒ s)
  
  def inspect[A](f: St ⇒ A): Prog[A] = get map f

  def modify(f: St ⇒ St): Prog[Unit] = ProgT((_,l,s) ⇒ F pure Right((l,f(s),unit)))

  def getL[A](l: Lens[St,A]): Prog[A] = inspect(l.get)

  def setL[A](l: Lens[St,A])(a: A): Prog[Unit] = modL(l)(_ ⇒ a)

  def modL[A](l: Lens[St,A])(f: A ⇒ A): Prog[Unit] = modify(l.modify(_)(f))

  def doLog(l: L): Prog[Unit] = ProgT((_,ls,s) ⇒ F pure Right((l::ls,s,unit)))


  // *** Dependency injection using the Reader Monad *** //

  def ask: Prog[Conf] = ProgT((c,l,s) ⇒ F pure Right((l,s,c)))

  def reader[A](f: Conf ⇒ A): Prog[A] = ask map f


  // *** Monadic Utility Functions *** //

  def between[A,B,C](p1: Prog[A], p2: Prog[B])(p: ⇒ Prog[C]): Prog[C] = for {
    _ <- p1
    r <- p
    _ <- p2
  } yield r

  def pmany[A](p: ⇒ Prog[A]): Prog[List[A]] =
    psome(p) map (_.toList) or pure(nil[A])

  def psome[A](p: ⇒ Prog[A]): Prog[Nel[A]] = for {
    h <- p
    t <- pmany(p)
  } yield Nel(h,t)

  def when(b: Boolean)(p: ⇒ Prog[Unit]): Prog[Unit] = if (b) p else pure(unit)

  def whenM(b: Prog[Boolean])(p: ⇒ Prog[Unit]): Prog[Unit] = b >>= { when(_)(p) }
}
