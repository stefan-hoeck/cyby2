/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

/**
  * Core CyBy2 package providing utility functions and
  * classes useful in many contexts.
  */
package object cyby
  extends cyby.types
  with cyby.util {

  object syntax {
    implicit class MapLensSyntax[S,K,V](val l: Lens[S,Map[K,V]]) extends AnyVal {
      def at(k: K): Lens[S,Option[V]] = atLens(k) compose l
    }
  }
}

// vim: set ts=2 sw=2 et:
