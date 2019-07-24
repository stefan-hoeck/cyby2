/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby

/**
  * Useful type aliases and aliases for often used
  * companion objects.
  */
trait types {
  type Nel[A] = cats.data.NonEmptyList[A]

  val Nel = cats.data.NonEmptyList

  type Validated[+E,+A] = cats.data.Validated[E,A]

  type Valid[A] = cats.data.Validated.Valid[A]

  type Invalid[+E] = cats.data.Validated.Invalid[E]

  type Undef = Option[Unit]

  val Validated = cats.data.Validated

  val Valid = cats.data.Validated.Valid

  val Invalid = cats.data.Validated.Invalid

  type ValNel[E,A] = Validated[Nel[E],A]

  type ErrNel[E,A] = Either[Nel[E],A]

  type Lens[A,B] = shapeless.Lens[A,B]

  type Parser[A] = ProgT[Pure,Unit,Unit,List[String],Unit,A]

  type @@[V,K] = shapeless.labelled.FieldType[K,V]
}


// vim: set ts=2 sw=2 et:
