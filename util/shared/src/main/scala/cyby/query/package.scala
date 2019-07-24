/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby

import cats.Functor, cats.implicits._

/**
  * Data types and utility functions used in combined queries
  */
package object query {
  /**
    * Predicate
    */
  type Pred[-A]    = A ⇒ Boolean

  /**
    * Function to parse a predicate from a string
    */
  type ReadPred[A] = String ⇒ Option[Pred[A]]

  /**
    * Parses a predicate from a list of tokens
    */
  type ParsePred[A] = Parser[Pred[A]]

  /**
    * Filters (and possibly transforms) a map of values
    */
  type MapFilter[I,A] = Map[I,A] ⇒ Map[I,A]

  val AndStr      = "and"
  val CIStr       = "(?i)"
  val Contains    = "contains"
  val ContainsCI  = "contains_ci"
  val EqStr       = "=="
  val GEqStr      = ">="
  val GtStr       = ">"
  val LEqStr      = "<="
  val LtStr       = "<"
  val NEqStr      = "!="
  val NotStr      = "not"
  val OrStr       = "or"
  val Undefined   = "undefined"
  val Values      = "values"

  val Negators        = List("", NotStr)
  val Comparators     = List(AndStr, OrStr)
  val QPrefixes = List(EqStr, NEqStr, GtStr, GEqStr, LtStr, LEqStr)
  val StringQPrefixes = ContainsCI :: Contains :: QPrefixes

  /**
    * Takes a parser for a predicate and creates a function
    * returning a MapFilter from a String and a Boolean. The Boolean
    * is used to possibly negate the parsed predicate.
    */
  def toMapFilter[I,A](p: ReadPred[A]): (Boolean,String) ⇒ Option[MapFilter[I,A]] =
    (neg,s) ⇒ {
      val pp: ReadPred[(I,A)] = ReadPred.cmap(p)(_._2)

      pp(s).map(pr ⇒ (db: Map[I,A]) ⇒ if (neg) db filterNot pr else db filter pr)
    }

  /**
    * Views a MapFilter through the lense of a parent type
    * returning a MapFilter for that parent type. This is used
    * to filter child elements from a parent element, and then
    * keeping the parent element only in the Map, if at least
    * one child element remains.
    *
    * @tparam IC : Child Key
    * @tparam C  : Child Type
    * @tparam IP : Parent Key
    * @tparam P  : Parent Type
    */
  def lensed[F[_]:Functor,IC,IP,C,P](m: F[MapFilter[IC,C]])
    (l: Lens[P,Map[IC,C]]): F[MapFilter[IP,P]] = m map {
    f ⇒ (m1: Map[IP,P]) ⇒ m1.map { case (i,p) ⇒ i -> l.modify(p)(f) }
                            .filterNot{ case (i,p) ⇒ l.get(p).isEmpty}
  }

  /**
    * Creates a new Map over the union of the key sets of both maps.
    * In case of a key with entries in both maps, the entries will
    * be merged using the given function.
    */
  def joinMaps[K,V](f: (V,V) ⇒ V): (Map[K,V],Map[K,V]) ⇒ Map[K,V] = (m1,m2) ⇒ {
    def un(k: K): (K,V) = (m1 get k, m2 get k) match {
      case (a,b) ⇒ k -> (a,b).mapN(f).orElse(a).orElse(b).get
    }

    (m1.keySet union m2.keySet).toList map un toMap
  }
}

// vim: set ts=2 sw=2 et:
