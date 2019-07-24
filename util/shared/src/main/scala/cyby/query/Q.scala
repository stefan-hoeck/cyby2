/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package query

import cats.implicits._

import io.circe.generic.JsonCodec

/**
  * Enum type representing comparator between combined
  * queries.
  */
@JsonCodec sealed trait Comp { def c: Comp = this }

object Comp extends EnumHelper[Comp] {
  case object And extends Comp

  case object Or  extends Comp

  val name = "cyby.query.Comp"

  val values = Nel.of(And, Or)

  def encode(f: Comp) = lowerHeadEncode(f)
}

/**
  * Sub type representing combined queries.
  *
  * @tparam L: Label type representing the field to be queried in
  *            a primitive query.
  */
@JsonCodec sealed trait Q[L]

/**
  * Primitive query, used to inspect the given field. The query itself
  * is encoded as a string and has to be interpreted first. See
  * function Q.mapper.
  */
case class Prim[L](lbl: L, query: String, negate: Boolean) extends Q[L]

/**
  * Chain of combined queries.
  */
case class Chain[L](qs: List[(Comp, Q[L])]) extends Q[L]

object Q {
  def mapper[A,L](q: Q[L])(p: (L,Boolean,String) ⇒ Option[A ⇒ A])
    (join: (A,A) ⇒ A): ValNel[QueryErr[L],A ⇒ A] = q match {
      case Prim(l,s,n)  ⇒ optionToValNel(QueryErr(l, s))(p(l,n,s))
      case Chain(Nil)   ⇒ valid(identity[A])
      case Chain((_,h)::t)   ⇒ {
        val hq = mapper(h)(p)(join)
        val tq = t.traverse{ case (c,q) ⇒ mapper(q)(p)(join) map (c -> _) }

        (hq,tq).mapN((f,fs) ⇒
          fs.foldLeft(f){
            case (f, (Comp.And, g)) ⇒ f andThen g
            case (f, (Comp.Or,  g)) ⇒ (a: A) ⇒ join(f(a),g(a))
          }
        )
      }
    }
}
