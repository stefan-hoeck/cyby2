/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby

import cats.implicits._

/**
  * Utility functions for working with parsers
  */
object Parser extends ProgHelper[Pure,Unit,List[String],Unit,Unit] {
  def run[A](p: Parser[A])(ts: List[String]): Option[(List[String],A)] =
    p.run(unit,Nil,ts).fold(_ ⇒ None, { case (_,ts2,a) ⇒ Some(ts2 -> a)})

  def read[A](p: Parser[A])(ts: List[String]): Option[A] = run(p)(ts) flatMap {
    case (Nil,a) ⇒ Some(a)
    case _       ⇒ None
  }

  def readTokens[A](p: Parser[A])(s: String): Option[A] =
    read(p)(tokenize(s))
 
  def readV[E,A](p: Parser[A])(ts: List[String], err: ⇒ E): ValNel[E,A] =
    optionToValNel(err)(read(p)(ts))

  def readVTokens[E,A](p: Parser[A])(s: String, err: ⇒ E): ValNel[E,A] =
    readV(p)(tokenize(s), err)
 
  def readE[E,A](p: Parser[A])(ts: List[String], err: ⇒ E): ErrNel[E,A] =
    optionToErrNel(err)(read(p)(ts))

  def readETokens[E,A](p: Parser[A])(s: String, err: ⇒ E): ErrNel[E,A] =
    readE(p)(tokenize(s), err)

  def read[A:Read]: Parser[A] = parse(Read[A].read)

  def parens[A](p: ⇒ Parser[A]): Parser[A] =
    between(single("("),single(")"))(p)

  def parse[A](p: String ⇒ Option[A]): Parser[A] =
    next >>= { p(_).fold[Parser[A]](fail)(pure)}

  lazy val next: Parser[String] = get flatMap {
    case Nil  ⇒ fail[String]
    case h::t ⇒ set(t) as h
  }

  lazy val eoi: Parser[Unit] = get flatMap {
    case Nil   ⇒ pure(unit)
    case h::_  ⇒ fail[Unit]
  }

  private def fail[A]: Parser[A] = raise[A](unit)

  def prefix[A](pre: String)(p: Parser[A]): Parser[A] = single(pre) *> p

  def single(s: String): Parser[String] = parse[String]{
    case x if x === s ⇒ Some(x)
    case _            ⇒ None
  }

  def all: Parser[List[String]] = get flatMap (set(Nil) as _)

  private type CS = List[Char]

  def tokenize(s: String): List[String] = {
    def next(cs: CS, res: List[String]): List[String] =
      if (cs.isEmpty) res else cs.reverse.mkString("") :: res

    def run(cs: CS, acc: CS, res: List[String]): List[String] = cs match {
      case Nil ⇒  next(acc,res).reverse
      case '('::t ⇒ run(t, Nil, "(" :: next(acc, res))
      case ')'::t ⇒ run(t, Nil, ")" :: next(acc, res))
      case ' '::t ⇒ run(t, Nil, next(acc, res))
      case c  ::t ⇒ run(t, c::acc, res)
    }

    run(s.toList, Nil, Nil)
  }
}

// vim: set ts=2 sw=2 et:
