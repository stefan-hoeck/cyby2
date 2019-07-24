/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby

import cats.Eq, cats.implicits._

import scala.util.control.NonFatal

import io.circe.{Encoder, Decoder, Json, JsonObject}

/**
  * Utility functions. In most cases, the types
  * are specific enough and no further explanation
  * is necessary.
  */
trait util {
  val unit: Unit = ()

  val undef: Undef = None

  def read[A:Read](s: String): Option[A] = Read[A] read s

  def readId[A](s: String): Option[Id[A]] = read[Id[A]](s)

  def lens[A]: Lens[A,A] = shapeless.lens[A]

  def setL[A,B](l: Lens[A,B])(b: B): A ⇒ A = l.set(_)(b)

  def modL[A,B](l: Lens[A,B])(f: B ⇒ B): A ⇒ A = l.modify(_)(f)

  def nextId[A](s: Set[Id[A]]): Id[A] =
    if (s.isEmpty) Id(0L) else s.maxBy(_.v).inc

  def mapErr[E,F,A](v: ValNel[E,A])(f: E ⇒ F): ValNel[F,A] =
    v leftMap (_ map f)

  def mapErr[E,F,A](v: ErrNel[E,A])(f: E ⇒ F): ErrNel[F,A] =
    v.fold(nel ⇒ Left(nel map f), Right(_))

  def errNel[E,A](v: Either[E,A]): ErrNel[E,A] =
    v.fold(e ⇒ Left(Nel of e), Right(_))

  def optionToValNel[E,A](e: ⇒ E)(o: Option[A]): ValNel[E,A] =
    o.fold(fail[E,A](e))(valid)

  def optionToErrNel[E,A](e: ⇒ E)(o: Option[A]): ErrNel[E,A] =
    optionToValNel(e)(o).toEither

  def valid[E,A](a: A): ValNel[E,A] = Validated valid a

  def fail[E,A](e: E): ValNel[E,A] = Validated invalidNel e

  def right[E,A](a: A): ErrNel[E,A] = Right(a)

  def left[E,A](e: E): ErrNel[E,A] = Left(Nel(e, Nil))

  def none[A]: Option[A] = None

  def some[A](a: A): Option[A] = Some(a)

  def nil[A]: List[A] = Nil

  def tailOption[A](as: List[A]): Option[List[A]] = as match {
    case Nil  ⇒ None
    case h::t ⇒ Some(t)
  }

  def toMap[K,V](f: V ⇒ K)(vs: List[V]): Map[K,V] =
    vs map (v ⇒ f(v) -> v) toMap

  def dotag[A,K](a: A): A @@ K = shapeless.labelled.field[K].apply[A](a)

  def mapTagged[K,A,B](a: A @@ K)(f: A ⇒ B): B @@ K = dotag(f(a))

  def must[E](b: Boolean)(e: ⇒ E): List[E] = if (b) Nil else List(e)

  def mustNot[E](b: Boolean)(e: ⇒ E): List[E] = must(!b)(e)

  def stripEnc[A](a: A)(implicit E: Encoder[A]): Json = stripJson(E(a))

  /**
    * Recursively strips Null values from Json objects
    */
  def stripJson(j: Json): Json = {
    def obj(o: JsonObject): Json =
      Json fromJsonObject o.filter{ case (_,v) ⇒ !v.isNull}.mapValues(stripJson)

    j.fold(j, _ ⇒ j, _ ⇒ j, _ ⇒ j, vs ⇒ Json.arr(vs map stripJson: _*), obj)
  }

  /**
    * Implicits for tagged values
    */
  object tagInstances {
    implicit def encI[V:Encoder,K]: Encoder[V @@ K] =
      Encoder[V].contramap(v ⇒ v)

    implicit def decI[V:Decoder,K]: Decoder[V @@ K] =
      Decoder[V] map dotag[V,K]
  }

  /**
    * Implicits for working with JSON encoders and decoders for
    * higher-kinded types. See the data module in the example
    * implementation for typical use cases.
    */
  object decoder1Instances {
    implicit def enc1Inst[F[_],A](implicit F:Encoder1[F],A:Encoder[A])
      : Encoder[F[A]] = F apply A

    implicit def dec1Inst[F[_],A](implicit F:Decoder1[F],A:Decoder[A])
      : Decoder[F[A]] = F apply A
  }

  /**
    * Wraps a possibly failing computation in a Validated[Nel[E],A],
    * catching non-fatal errors (as in scala.util.control.NonFatal)
    * and passes those to onErr.
    */
  def tryO[A](a: ⇒ A): Option[A] = try {
    a match {
      case null ⇒ None
      case a2   ⇒ Some(a2)
    }
  } catch { case e@NonFatal(_) ⇒ none }

  /**
    * Wraps a possibly failing computation in a Validated[Nel[E],A],
    * catching non-fatal errors (as in scala.util.control.NonFatal)
    * and passes those to onErr.
    */
  def tryV[E,A](a: ⇒ A, onErr: Throwable ⇒ E): ValNel[E,A] = try {
    Validated valid a
  } catch {
    case e@NonFatal(_) ⇒ Validated invalidNel onErr(e)
  }

  /**
    * Like tryV but wraps the result in an Either
    */
  def tryE[E,A](a: ⇒ A, onErr: Throwable ⇒ E): ErrNel[E,A] =
    tryV(a, onErr).toEither

  def atLens[K,V](k: K): Lens[Map[K,V],Option[V]] = new Lens[Map[K,V],Option[V]] {
    def get(m: Map[K,V]) = m get k
    def set(m: Map[K,V])(o: Option[V]) = o match {
      case Some(v) ⇒ m + (k -> v)
      case None    ⇒ m - k
    }
  }

  object shapelessImplicits {
    import shapeless.{HNil, HList, ::}

    implicit lazy val hnilEq: Eq[HNil] = Eq.fromUniversalEquals
    implicit def hconsEq[H,T<:HList](
      implicit H: Eq[H], T: Eq[T]
    ): Eq[H::T] = new Eq[H::T]{
      def eqv(a: H::T, b: H::T) =
        H.eqv(a.head, b.head) && T.eqv(a.tail, b.tail)
    }
  }
}

// vim: set ts=2 sw=2 et:
