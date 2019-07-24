/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

import cats.{Foldable, Applicative, Functor, Traverse, Eval}, cats.implicits._
import io.circe.generic.JsonCodec

/**
  * Sent from server to client after successfully editing, querying
  * a list of data objects at the server.
  */
@JsonCodec sealed trait EditRes[A] {
  def r: EditRes[A] = this
  def map[B](f: A ⇒ B): EditRes[B] = this match {
    case Found(as,t,s) ⇒ Found(as map f, t, s)
    case Added(a)      ⇒ Added(f(a))
    case Updated(a)    ⇒ Updated(f(a))
    case Deleted(a)    ⇒ Deleted(f(a))
  }

  def vals: List[A] = this match {
    case Found(as,_,_) ⇒ as
    case Added(a)      ⇒ List(a)
    case Updated(a)    ⇒ List(a)
    case Deleted(a)    ⇒ List(a)
  }
}

/**
  * List of query results. This might be a partial list including
  * the total count of hits found together with the starting index
  * of the first item in the list. This is useful when it makes sense
  * to request hit sets incrementally, for instance when a user scrolls
  * down in the browsers to see more items.
  */
case class Found[A](as: List[A], total: Int, start: Int) extends EditRes[A]

case class Added[A](a: A) extends EditRes[A]

case class Updated[A](a: A) extends EditRes[A]

case class Deleted[A](a: A) extends EditRes[A]

object EditRes {
  implicit def eqI[A]: cats.Eq[EditRes[A]] = cats.Eq.fromUniversalEquals

  implicit val funI: Functor[EditRes] = new Functor[EditRes]{
    def map[A,B](e: EditRes[A])(f: A ⇒ B) = e map f
  }

  implicit val travI: Traverse[EditRes] = new Traverse[EditRes]{
    def foldLeft[A, B](fa: EditRes[A],b: B)(f: (B, A) ⇒ B): B =
      Foldable[List].foldLeft(fa.vals,b)(f)

    def foldRight[A, B](fa: EditRes[A],lb: Eval[B])(f: (A, Eval[B]) ⇒ Eval[B]): Eval[B] =
      Foldable[List].foldRight(fa.vals,lb)(f)

    def traverse[G[_]:Applicative, A, B](fa: EditRes[A])(f: A ⇒ G[B]): G[EditRes[B]] = fa match {
      case Found(as,x,y) ⇒ as traverse f map (Found(_,x,y).r)
      case Added(a)      ⇒ f(a) map (Added(_).r)
      case Updated(a)    ⇒ f(a) map (Updated(_).r)
      case Deleted(a)    ⇒ f(a) map (Deleted(_).r)
    }
  }
}

// vim: set ts=2 sw=2 et:
