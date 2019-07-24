/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

/**
  * Datatype to send editing (creating, mutating, and deleting data
  * objects) instructions from client to server.
  *
  * @tparam I: Id representing an object in a data tree
  * @tparam A: Data needed to create a new data object
  * @tparam M: Data needed to modify an existing data object
  */
@io.circe.generic.JsonCodec sealed trait Edit[I,A,M]{
  import cyby.dat.{EditRes,Added,Updated,Deleted}

  def e: Edit[I,A,M] = this

  def id(f: A ⇒ I): I = this match {
    case Add(a)   ⇒ f(a)
    case Mod(i,_) ⇒ i
    case Del(i)   ⇒ i
  }

  def editRes[S](s: S): EditRes[S] = this match {
    case Add(_)   ⇒ Added(s)
    case Mod(_,_) ⇒ Updated(s)
    case Del(_)   ⇒ Deleted(s)
  }
}

case class Add[I,A,M](ed: A) extends Edit[I,A,M]

case class Mod[I,A,M](id: I, ed: M) extends Edit[I,A,M]

case class Del[I,A,M](id: I) extends Edit[I,A,M]


object Edit {
  implicit def eqI[I,A,M]: cats.Eq[Edit[I,A,M]] = cats.Eq.fromUniversalEquals
}
