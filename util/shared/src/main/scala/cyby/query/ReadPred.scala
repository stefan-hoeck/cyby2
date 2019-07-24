/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package query

import cats.{Eq, Order}, cats.implicits.{none ⇒ _, _}
import cyby.{Parser ⇒ P}, P.{prefix, parse, psome}

import scala.util.matching.Regex

/**
  * Parsers for predicates.
  */
object ReadPred {
  def always[A]: ReadPred[A] = _ ⇒ some(_ ⇒ true)

  def bool[A](f: A ⇒ Boolean): ReadPred[A] = cmap(bool_)(f)

  def boolO[A](f: A ⇒ Option[Boolean]): ReadPred[A] = cmap(boolO_)(f)

  def int[A](f: A ⇒ Int): ReadPred[A] = orderC(f)(P.read)

  def long[A](f: A ⇒ Long): ReadPred[A] = orderC(f)(P.read)

  def id[A,B](f: A ⇒ Id[B]): ReadPred[A] = long(f(_).v)

  def ids[A,B](f: A ⇒ List[Id[B]]): ReadPred[A] =
    cmap(list(id[Id[B],B](identity)))(f)

  def idsNel[A,B](f: A ⇒ Nel[Id[B]]): ReadPred[A] = ids(f(_).toList)

  def float[A](f: A ⇒ Float): ReadPred[A] = orderC(f)(P.read)

  def double[A](f: A ⇒ Double): ReadPred[A] = orderC(f)(P.read)

  def doubleO[A](f: A ⇒ Option[Double]): ReadPred[A] = orderCO(f)(P.read)

  def string[A](f: A ⇒ String): ReadPred[A] = cmap(string)(f)

  def stringO[A](f: A ⇒ Option[String]): ReadPred[A] = cmap(option(string))(f)

  lazy val bool_ : ReadPred[Boolean] = Read[Boolean] read _ map (b ⇒ b === _)

  lazy val boolO_ : ReadPred[Option[Boolean]] = option(bool_)

  lazy val int_ : ReadPred[Int] = int(identity)

  lazy val long_ : ReadPred[Long] = long(identity)

  def id_ [A]: ReadPred[Id[A]] = id(identity)

  def ids_ [A]: ReadPred[List[Id[A]]] = ids(identity)

  def idsNel_ [A]: ReadPred[Nel[Id[A]]] =  idsNel(identity)

  lazy val float_ : ReadPred[Float] = float(identity)

  lazy val double_ : ReadPred[Double] = double(identity)

  lazy val doubleO_ : ReadPred[Option[Double]] = doubleO(identity)

  lazy val string_ : ReadPred[String] = string(identity)

  lazy val stringO_ : ReadPred[Option[String]] = stringO(identity)

  def list[A](p: ReadPred[A]): ReadPred[List[A]] =
    p(_) map (pr ⇒ (as: List[A]) ⇒ as exists pr)

  def nel[A](p: ReadPred[A]): ReadPred[Nel[A]] =
    cmap(list(p))(_.toList)

  def orderC[A:Order,B](f: B ⇒ A)(p: Parser[A]): ReadPred[B] =
    cmap[A,B](P.readTokens(comb(order[A](p))))(f)

  def orderCO[A:Order,B](f: B ⇒ Option[A])(p: Parser[A]): ReadPred[B] =
    cmap[Option[A],B](option(P.readTokens(comb(order[A](p)))))(f)

  def cmap[A,B](p: ReadPred[A])(f: B ⇒ A): ReadPred[B] = p(_) map f.andThen

  def option[A](p: ReadPred[A]): ReadPred[Option[A]] = _ match {
    case "" ⇒ some(_.isEmpty)
    case s  ⇒ p(s) map (pred ⇒ _ exists pred)
  }

  /**
    * query ::= p
    *         | query (and | or) query
    *         | not(query)
    *         | (query)
    */
  def comb[A](p: ParsePred[A]): ParsePred[A] = {
    lazy val parenthized: ParsePred[A] = P parens res

    lazy val not: ParsePred[A] = P.prefix(NotStr)(parenthized.map(p ⇒ (a: A) ⇒ !p(a)))

    lazy val default: ParsePred[A] = P.eoi.as((_: A) ⇒ true)

    lazy val res: ParsePred[A] = for {
      ini <- default <|> not <|> parenthized    <|> p
      q   <- P.prefix(AndStr)(res map (p ⇒ (a: A) ⇒ p(a) && ini(a))) <|>
             P.prefix(OrStr)(res map (p ⇒ (a: A) ⇒ p(a) || ini(a)))  <|>
             P.pure(ini)
    } yield q
 
    res
  }

  def commaDelimited[A](p: Parser[A]): Parser[List[A]] =
    parse(s ⇒ s.split(",").toList traverse (s ⇒ P.read(p)(List(s))))

  def equal[A](p: Parser[A])(implicit A: Eq[A]): ParsePred[A] =
    prefix(EqStr)(p).map[Pred[A]](a ⇒ A.eqv(a,_))                  <|>
    prefix(NEqStr)(p).map[Pred[A]](a ⇒ A.neqv(a,_))                <|>
    prefix(Values)(commaDelimited(p)).map(_.toSet)                 <|>
    psome(p).map[Pred[A]](as ⇒ (a: A) ⇒ as.exists(A.eqv(a,_)))

  def order[A:Order](p: Parser[A]): ParsePred[A] =
    prefix(GEqStr)(p).map[Pred[A]](a ⇒ _ >= a) <|>
    prefix(GtStr)(p).map[Pred[A]](a ⇒ _ >  a) <|>
    prefix(LEqStr)(p).map[Pred[A]](a ⇒ _ <= a) <|>
    prefix(LtStr)(p).map[Pred[A]](a ⇒ _ <  a) <|>
    equal(p)

  lazy val string: ReadPred[String] = _.split(" ", 2).toList match {
    case EqStr       ::s::Nil ⇒ some(s === _)
    case NEqStr      ::s::Nil ⇒ some(s =!= _)
    case GEqStr      ::s::Nil ⇒ some(s <= _)
    case GtStr       ::s::Nil ⇒ some(s < _)
    case LEqStr      ::s::Nil ⇒ some(s >= _)
    case LtStr       ::s::Nil ⇒ some(s > _)
    case Contains    ::s::Nil ⇒ regex(s)
    case ContainsCI  ::s::Nil ⇒ regexCI(s)
    case _                    ⇒ none
  }

  lazy val regexCI: ReadPred[String] = s ⇒ {
    val s2 = if (s startsWith CIStr) s else s"${CIStr}${s}"
    regex(s2)
  }

  lazy val regex: ReadPred[String] =
    Read[Regex] read _ map { r ⇒ (s: String) ⇒ r findFirstIn s nonEmpty }
}

// vim: set ts=2 sw=2 et:
