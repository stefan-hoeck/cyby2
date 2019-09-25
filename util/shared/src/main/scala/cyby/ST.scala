/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby

import cats.Monad

/**
  * ST Monad for encapsulating impure computations in
  * a safe manner.
  *
  * Reference: J. Launchbury and S. Peyton Jones: Lazy Functional State Threads
  */
case class ST[S,A](private val run: World[S] ⇒ (World[S],A)) {
  def map[B](f: A ⇒ B): ST[S,B] = ST{w ⇒ (w,f(run(w)._2)) }

  def flatMap[B](f: A ⇒ ST[S,B]): ST[S,B] = ST{w ⇒ f(run(w)._2) run w }
}

trait Forall[A] {
  def apply[S]: ST[S,A]
}

case class World[A]()

object ST {
  def runST[A](f: Forall[A]): A = f.apply.run(World())._2

  def returnST[S,A](a: A): ST[S,A] = ST(w ⇒ (w,a))

  implicit def monadI[S]: Monad[ST[S,?]] = new Monad[ST[S,?]] {
    override def map[A,B](fa: ST[S,A])(f: A ⇒ B) = fa map f
    override def pure[A](a: A): ST[S,A] = returnST(a)
    override def flatMap[A,B](fa: ST[S,A])(f: A ⇒ ST[S,B]) = fa flatMap f
    override def tailRecM[A, B](a: A)(f: A ⇒ ST[S,Either[A,B]]): ST[S,B] = {

      @scala.annotation.tailrec
      def loop(w: World[S], a2: A): (World[S],B) = f(a2) run w match {
        case (_,Right(b)) ⇒ (w,b)
        case (_,Left(a3)) ⇒ loop(w,a3)
      }

      ST{loop(_,a)}
    }
  }
}
