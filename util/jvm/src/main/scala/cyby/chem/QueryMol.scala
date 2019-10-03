/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package chem

import cats.data.EitherT

import scala.collection.immutable.BitSet

case class QueryMol(
  private val queryMol: MolPure,
  fingerPrint: BitSet
){
  def isSubgraphOf(m: Mol): Boolean = {
    fingerPrint.subsetOf(m.fingerprint) && isSubgraph(m)
  }

  private def isSubgraph(m: Mol): Boolean = {
    def run[S]: MutableMol.ResST[S,Boolean] = for {
      mm <- EitherT.liftF(m.mol.thaw[S])
      mq <- EitherT.liftF(queryMol.unsafeThaw[S])
      b  <- MutableMol.isSubgraph(mm,mq)
    } yield b

    ST.runST(new Forall[ErrNel[Throwable,Boolean]]{def apply[S] = run[S].value})
      .getOrElse(false)
  }
}

object QueryMol {
  def read(s: String): Option[QueryMol] = readE(s).toOption
 
  def readE(s: String): ErrNel[Throwable,QueryMol] = {
    def run[S]: MutableMol.ResST[S,QueryMol] =
      MutableMol.readForQuery[S](s).map{ case (m,fp) ⇒ QueryMol(m, fp) }

    ST.runST(new Forall[ErrNel[Throwable,QueryMol]]{def apply[S] = run[S].value})
  }
}

