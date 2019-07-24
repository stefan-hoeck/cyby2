/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package msf
package js

import cats.{ Apply, Functor }, cats.implicits._

trait AppSyntax {
  implicit class AppSyntax2T2[G[_],H[_],A,B](val v: (G[H[A]],G[H[B]])) {
    def mapN2[X](f: (A,B) ⇒ X)(
      implicit G: Apply[G], H: Apply[H]
    ): G[H[X]] = v.mapN((_,_).mapN(f))
  }

  implicit class AppSyntax2T3[G[_],H[_],A,B,C](
    val v: (G[H[A]],G[H[B]],G[H[C]])
  ) {
    def mapN2[X](f: (A,B,C) ⇒ X)(
      implicit G: Apply[G], H: Apply[H]
    ): G[H[X]] = v.mapN((_,_,_).mapN(f))
  }

  implicit class AppSyntax2T4[G[_],H[_],A,B,C,D](
    val v: (G[H[A]],G[H[B]],G[H[C]],G[H[D]])
  ) {
    def mapN2[X](f: (A,B,C,D) ⇒ X)(
      implicit G: Apply[G], H: Apply[H]
    ): G[H[X]] = v.mapN((_,_,_,_).mapN(f))
  }

  implicit class AppSyntax3T2[G[_],H[_],I[_],A,B](
    val v: (I[G[H[A]]],I[G[H[B]]])
  ) {
    def mapN2[X](f: (A,B) ⇒ X)(
      implicit G: Apply[G], H: Apply[H], I: Apply[I]
    ): I[G[H[X]]] = v.mapN((_,_).mapN2(f))
  }

  implicit class AppSyntax3T3[G[_],H[_],I[_],A,B,C](
    val v: (I[G[H[A]]],I[G[H[B]]],I[G[H[C]]])
  ) {
    def mapN2[X](f: (A,B,C) ⇒ X)(
      implicit G: Apply[G], H: Apply[H], I: Apply[I]
    ): I[G[H[X]]] = v.mapN((_,_,_).mapN2(f))
  }

  implicit class AppSyntax3T4[G[_],H[_],I[_],A,B,C,D](
    val v: (I[G[H[A]]],I[G[H[B]]],I[G[H[C]]],I[G[H[D]]])
  ) {
    def mapN2[X](f: (A,B,C,D) ⇒ X)(
      implicit G: Apply[G], H: Apply[H], I: Apply[I]
    ): I[G[H[X]]] = v.mapN((_,_,_,_).mapN2(f))
  }

  implicit class MapSyntax2[G[_],H[_],A](val v: G[H[A]]) {
    def map2[B](f: A ⇒ B)(
      implicit G: Functor[G], H: Functor[H]
    ): G[H[B]] = v map (_ map f)
  }

  implicit class MapSyntax3[G[_],H[_],I[_],A](val v: G[H[I[A]]]) {
    def map3[B](f: A ⇒ B)(
      implicit G: Functor[G], H: Functor[H], I: Functor[I]
    ): G[H[I[B]]] = v map2 (_ map f)
  }

  implicit class MapSyntax4[G[_],H[_],I[_],J[_],A](val v: G[H[I[J[A]]]]) {
    def map4[B](f: A ⇒ B)(
      implicit G: Functor[G], H: Functor[H], I: Functor[I], J: Functor[J]
    ): G[H[I[J[B]]]] = v map3 (_ map f)
  }

  implicit class MapSyntax5[G[_],H[_],I[_],J[_],K[_],A](val v: G[H[I[J[K[A]]]]]) {
    def map5[B](f: A ⇒ B)(
      implicit G: Functor[G], H: Functor[H], I: Functor[I], J: Functor[J], K: Functor[K]
    ): G[H[I[J[K[B]]]]] = v map4 (_ map f)
  }
}

// vim: set ts=2 sw=2 et:
