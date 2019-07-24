/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui
package select

/**
  * Sum type representing selection events
  */
sealed trait Event[+A]{ def e: Event[A] = this }

/** selection was cleared */
case object Clear extends Event[Nothing]

/** selection of a single item */
case class Single[A](v: A) extends Event[A]

/** toggles selection of a single item */
case class Toggle[A](v: A) extends Event[A]

/**
  * selection of a range of items
  */
case class Range[A](v: A) extends Event[A]
