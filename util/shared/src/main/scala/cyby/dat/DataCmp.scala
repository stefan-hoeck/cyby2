/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

/**
  * Utility trait to be extended by companion objects of data types.
  */
trait DataCmp {
  type Id = cyby.Id[this.type]

  type AccId = cyby.Id[this.type @@ HasAccess]

  type Cli

  /**
    * Alias for JSON decoders to reduce boilerplate
    */
  type D[A] = io.circe.Decoder[A]

  /**
    * Alias for higher-kinded JSON encoders to reduce boilerplate
    */
  type D1[F[_]] = cyby.Decoder1[F]

  /**
    * Alias for JSON encoders to reduce boilerplate
    */
  type E[A] = io.circe.Encoder[A]

  /**
    * Alias for higher-kinded JSON decoders to reduce boilerplate
    */
  type E1[F[_]] = cyby.Encoder1[F]

  val L: Lens[Cli,Cli] = lens
}

// vim: set ts=2 sw=2 et:
