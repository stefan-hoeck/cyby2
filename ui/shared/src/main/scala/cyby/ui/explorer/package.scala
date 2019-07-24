/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui

import cats.Monoid

/**
  * Types, constants and functions used in the explorer
  */
package object explorer {
  /**
    * Number of pixels left to the bottom of the substance table,
    * before new compounds are displayed or requested from the server
    */
  final val DistanceToBottom: Int = 500

  /**
    * Number of compounds requested from the server.
    */
  final val LoadCount:        Int = 60

  /**
    * Number of compounds displayed at once
    */
  final val DispCount:        Int = 10

  /**
    * Number of pixels the row height is increased when pressing
    * the "enlarge" button
    */
  final val EnlargeStep:      Int = 25

  /**
    * Maximal row height
    */
  final val EnlargeMax:       Int = 500

  /**
    * Minimal row height
    */
  final val EnlargeMin:       Int = 50

  /**
    * Initial row height
    */
  final val EnlargeIni:       Int = 100

  /**
    * Total flex size of substance view plus side / bottom view
    */
  final val TotSize:          Int = 10 

  /**
    * Minimal flex size of substance view
    */
  final val MinSize:          Int = 1

  /**
    * Maximal flex size of substance view
    */
  final val MaxSize:          Int = TotSize - MinSize

  /**
    * Initial flex size of substance view
    */
  final val StartSize:        Int = 8

  implicit def endoMonoid[A]: Monoid[A ⇒ A] = new Monoid[A ⇒ A]{
    val empty = identity
    def combine(f1: A ⇒ A, f2: A ⇒ A) = f1 andThen f2
  }
}

// vim: set ts=2 sw=2 et:
