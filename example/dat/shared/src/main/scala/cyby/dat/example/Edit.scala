/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat
package example

import io.circe.{Json ⇒ J}
import io.circe.generic.JsonCodec
import io.circe.shapes._
import io.circe.syntax._

/**
  * Data type representing a modification of the
  * substance data tree. In this example implementation, substances
  * are the only deeply nested heterogeneous data structures.
  * In customized implementations, several such data structures might exist
  * and this data type suggests a way to deal with updates at such
  * data structures in a type-safe manner.
  *
  * Below is a schema of the substance tree's structure. In parentheses
  * are the types of IDs leading to elements of a given type.
  * In order to access a bio entry, for instance, we need its ID (of
  * type BiodataEntry.Id) together with the ID of its parent container (type Container.Id)
  * and the ID of the container's parent compound (type Compound.Id).
  *
  *   Cpd (Compound.Id)
  *   |
  *   |___ Con (Container.Id)
  *   |    |
  *   |    |___ Bio (BiodataEntry.Id)
  *   |    |    |
  *   |    |    |___ Fil (Fil.Id)
  *   |    |
  *   |    |___ Fil (Fil.Id)
  *   |
  *   |___ Fil (Fil.Id)
  *
  *  The four type parameters represent the types used
  *  to edit bio entries, containers, files, and substances.
  *
  *  @tparam B: represents bio entry
  *  @tparam C: represents container entry
  *  @tparam F: represents file entry
  *  @tparam S: represents substance entry
  */
@JsonCodec sealed trait CpdTreeEd[B,C,F,S] {
  def e: CpdTreeEd[B,C,F,S] = this
}

case class CpdEdit[B,C,F,S](ed: S) extends CpdTreeEd[B,C,F,S]

case class ConEdit[B,C,F,S](p: Compound.Path, ed: C) extends CpdTreeEd[B,C,F,S]

case class BioEdit[B,C,F,S](p: Container.Path, ed: B) extends CpdTreeEd[B,C,F,S]

case class CpdFilEdit[B,C,F,S](p: Compound.Path, ed: F) extends CpdTreeEd[B,C,F,S]

case class ConFilEdit[B,C,F,S](p: Container.Path, ed: F) extends CpdTreeEd[B,C,F,S]

case class BioFilEdit[B,C,F,S](p: BiodataEntry.Path, ed: F) extends CpdTreeEd[B,C,F,S]

object CpdTreeEd {
  def bioJson(p: Container.Path)(j: J): J = BioEdit[J,J,J,J](p,j).e.asJson

  def bioFilJson(p: BiodataEntry.Path)(j: J): J = BioFilEdit[J,J,J,J](p,j).e.asJson

  def conJson(p: Compound.Path)(j: J): J = ConEdit[J,J,J,J](p,j).e.asJson

  def conFilJson(p: Container.Path)(j: J): J = ConFilEdit[J,J,J,J](p,j).e.asJson

  def subJson(h: shapeless.HNil)(j: J): J = CpdEdit[J,J,J,J](j).e.asJson

  def subFilJson(p: Compound.Path)(j: J): J = CpdFilEdit[J,J,J,J](p,j).e.asJson

  implicit def eqI[B,C,F,S]: cats.Eq[CpdTreeEd[B,C,F,S]] = cats.Eq.fromUniversalEquals
}
