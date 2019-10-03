/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package query

import scala.collection.immutable.BitSet

/**
  * Enum type representing types of fingerprints
  * used in similarity searches.
  */
sealed trait Fingerprint { def fp: Fingerprint = this }

object Fingerprint extends EnumHelper[Fingerprint] {
  def splitQuery(s: String): Option[(Fingerprint,Pred[Double],String,String)] =
    s.split(":",3) toList match {
      case List(x,c,s) ⇒ for {
        fp  <- Read[Fingerprint] read x
        cp  <- ReadPred.double_ apply c
      } yield (fp,cp,c,s)
      case _                     ⇒ None
    }

  def encodeSimilarity[A](f: Fingerprint, q: String, a: A): String =
    s"${f}:${q}:${a}"

  def similarity[A:Read](f: Fingerprint ⇒ A ⇒ BitSet): ReadPred[Option[A]] = s ⇒ for {
    (x,c,_,s)  <- splitQuery(s)
    fp         = f(x)
    a          <- Read[A] read s
  } yield { ao: Option[A] ⇒
    ao.exists(a2 ⇒ c(tanimoto(fp(a), fp(a2))))
  }

  def tanimoto(bs: BitSet, bs2: BitSet): Double = {
    val inter = bs.intersect(bs2).size
    val tot = bs.size + bs2.size - inter

    if (tot == 0) 0D else inter.toDouble / tot.toDouble
  }

  val name = "cyby.query.Fingerprint"

  def encode(fp: Fingerprint) = Nel.of(fp.toString)

  case object Default extends Fingerprint
  case object PubChem extends Fingerprint
  case object MACCS extends Fingerprint

  lazy val values = Nel.of(PubChem, Default, MACCS)
}
