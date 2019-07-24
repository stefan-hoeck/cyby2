/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby

import io.circe.{Decoder,Encoder}

/**
  * JSON decoder for higher-kinded types. See the data module in the example
  * implementation for typical use cases.
  */
trait Decoder1[F[_]] {
  def apply[A:Decoder]: Decoder[F[A]]
}

object Decoder1 {
  def apply[F[_]](implicit F: Decoder1[F]): Decoder1[F] = F

  implicit lazy val optI: Decoder1[Option] = new Decoder1[Option]{
    def apply[A:Decoder] = implicitly
  }

  implicit lazy val mayI: Decoder1[Maybe] = new Decoder1[Maybe]{
    def apply[A:Decoder] = implicitly
  }

  implicit lazy val listI: Decoder1[List] = new Decoder1[List]{
    def apply[A:Decoder] = implicitly
  }

  implicit lazy val nelI: Decoder1[Nel] = new Decoder1[Nel]{
    def apply[A:Decoder] = implicitly
  }

  implicit lazy val pureI: Decoder1[Pure] = new Decoder1[Pure]{
    def apply[A:Decoder] = implicitly
  }
}

/**
  * JSON encoder for higher-kinded types. See the data module in the example
  * implementation for typical use cases.
  */
trait Encoder1[F[_]] {
  def apply[A:Encoder]: Encoder[F[A]]
}

object Encoder1 {
  def apply[F[_]](implicit F: Encoder1[F]): Encoder1[F] = F

  implicit lazy val optI: Encoder1[Option] = new Encoder1[Option]{
    def apply[A:Encoder] = implicitly
  }

  implicit lazy val mayI: Encoder1[Maybe] = new Encoder1[Maybe]{
    def apply[A:Encoder] = implicitly
  }

  implicit lazy val listI: Encoder1[List] = new Encoder1[List]{
    def apply[A:Encoder] = implicitly
  }

  implicit lazy val nelI: Encoder1[Nel] = new Encoder1[Nel]{
    def apply[A:Encoder] = implicitly
  }

  implicit lazy val pureI: Encoder1[Pure] = new Encoder1[Pure]{
    def apply[A:Encoder] = implicitly
  }
}
