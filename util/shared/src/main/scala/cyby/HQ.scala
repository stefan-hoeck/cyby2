/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby

import shapeless.{::, HList, HNil}

/**
  * A view into a heterogeneous tree.
  *
  * This data type not only allows queries into
  * heterogeneous structures, it also keeps
  * track of all visited nodes and their types
  * as well as an optional path of keys.
  *
  * Furthermore, it provides a facility to
  * remove or update the last visited node,
  * resulting in a heterogeneous list
  * of updated nodes.
  *
  * @param setter: Updates or removes a node from the structure
  * @param path:   Heterogenuous list of keys that led to this view
  * @param nodes:  Heterogenuous list of nodes visited along the way
  */
case class HQ[N,KS<:HList,NS<:HList](
  setter:  Option[N] ⇒ NS,
  path:    KS,
  nodes:   N::NS,
){
  def node: N = nodes.head

  def delete: NS = setter(none)

  def set(n: N): N::NS = n :: setter(some(n))

  def mod(f: N ⇒ N): N::NS = set(f(node))

  def setL[A](l: Lens[N,A], a: A): N::NS = mod(l.set(_)(a))

  type Next[K,N2] = HQ[N2,K::KS,N::NS]

  type NextI[I,N2] = Next[Id[I],N2]

  def path[K](k: K): HQ[N,K::KS,NS] = HQ(setter, k::path,nodes)

  def lensed[N2](l: Lens[N,N2]): HQ[N2,KS,N::NS] = {
    val n2: N2 = l get node
    val set = (on2: Option[N2]) ⇒ {
      val newN = l.set(node)(on2 getOrElse n2)

      newN :: setter(some(newN))
    }

    HQ(set,path,n2::nodes)
  }

  def query[K,N2](mkLens: K ⇒ Lens[N,Option[N2]])(k: K)
    : Option[Next[K,N2]] = {
      val l = mkLens(k)

      val set = (on2: Option[N2]) ⇒ {
        val newN = l.set(node)(on2)

        newN :: setter(some(newN))
      }

      l get node map { n2 ⇒ HQ(set, k::path, n2::nodes) }
    }

  def queryE[K,N2,E](toLens: K ⇒ Lens[N,Option[N2]])
    (err: (K::KS) ⇒ E, k: K): Either[E,Next[K,N2]] =
    query(toLens)(k).toRight(err(k::path))

  def queryNel[K,N2,E](toLens: K ⇒ Lens[N,Option[N2]])
    (err: (K::KS) ⇒ E, k: K): Either[Nel[E],Next[K,N2]] =
    queryE(toLens)(p ⇒ Nel of err(p), k)
}

object HQ {
  def root[R](r: R): HQ[R,HNil,HNil] = HQ(_ ⇒ HNil, HNil, r::HNil)
}
