/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package msf

import cats.implicits._
import cats.data.{ State }

class SFTest extends MsfSuite with SFHelper[State[Int,?]]{
  type Is = List[Int]
  type Us = List[Unit]

  type F[A] = State[Int,A]

  def run[A,B](sf: SF[A,B])(as: List[A]): (Int,List[B]) =
    sf embed as run 0 value

  def runV[A,B](sf: SF[A,B])(as: List[A]): List[B] = run(sf)(as)._2

  def runS[A,B](sf: SF[A,B])(as: List[A]): Int = run(sf)(as)._1

  def inc[A](a: A): F[A] = State(i ⇒ (i+1,a))

  def set10[A](a: A): F[A] = State(_ ⇒ 10 -> a)

  property("id always returns input") {
    forAll{ is: Is ⇒ runV(idS[Int])(is) shouldEqual is }
  }

  property("id is not effectful") {
    forAll{ is: Is ⇒ runS(idS[Int])(is) shouldEqual 0 }
  }

  property("const always returns given value") {
    forAll{ (is: Is, s: String) ⇒
      runV(const[Int,String](s))(is) shouldEqual is.as(s) }
  }

  property("const is not effectful") {
    forAll{ (is: Is, s: String) ⇒ runS(const[Int,String](s))(is) shouldEqual 0 }
  }

  property("const_ always returns given value") {
    forAll{ (is: Us, s: String) ⇒
      runV(const_[String](s))(is) shouldEqual is.as(s) }
  }

  property("const_ is not effectful") {
    forAll{ (is: Us, s: String) ⇒ runS(const_[String](s))(is) shouldEqual 0 }
  }

  property("constS always returns given value") {
    forAll{ (is: Is, s: String) ⇒
      runV(constS[Int,String](inc(s)))(is) shouldEqual is.as(s) }
  }

  property("constS is effectful") {
    forAll{ (is: Is, s: String) ⇒
      runS(constS[Int,String](inc(s)))(is) shouldEqual is.size }
  }

  property("constS_ always returns given value") {
    forAll{ (is: Us, s: String) ⇒
      runV(constS_[String](inc(s)))(is) shouldEqual is.as(s) }
  }

  property("constS_ is effectful") {
    forAll{ (is: Us, s: String) ⇒
      runS(constS_[String](inc(s)))(is) shouldEqual is.size }
  }

  property("arr maps input") {
    forAll{ (is: Is, f: Int ⇒ String) ⇒ runV(arr(f))(is) shouldEqual is.map(f) }
  }

  property("arr is not effectful") {
    forAll{ (is: Is, f: Int ⇒ String) ⇒ runS(arr(f))(is) shouldEqual 0 }
  }

  property("swap swaps input") {
    forAll{ (is: List[(Int,String)]) ⇒
      runV(swap[Int,String])(is) shouldEqual is.map(_.swap) }
  }

  property("swap is not effectful") {
    forAll{ (is: List[(Int,String)]) ⇒ runS(swap[Int,String])(is) shouldEqual 0 }
  }

  property("liftS maps input") {
    forAll{ (is: Is, f: Int ⇒ String) ⇒
      val ff = (i: Int) ⇒ inc(i) map f
      runV(liftS(ff))(is) shouldEqual is.map(f)
    }
  }

  property("liftS is effectful") {
    forAll{ (is: Is, f: Int ⇒ String) ⇒
      val ff = (i: Int) ⇒ inc(i) map f
      runS(liftS(ff))(is) shouldEqual is.size
    }
  }

  property("loop accumulates correctly") {
    forAll{ (is: Is, ini: Int) ⇒
      val sf = arr[(Int,Int),(Int,Int)]{ case (in,st) ⇒ (st,in+st) }
      runV(SF.loop(sf)(ini))(is) shouldEqual is.scanLeft(ini)(_ + _).init
    }
  }

  property("loop is not effectful") {
    forAll{ (is: Is, ini: Int) ⇒
      val sf = arr[(Int,Int),(Int,Int)]{ case (in,st) ⇒ st -> (in+st) }
      runS(SF.loop(sf)(ini))(is) shouldEqual 0
    }
  }

  property("loopF accumulates correctly") {
    forAll{ (is: Is, ini: Int) ⇒
      val sf = arr[(Int,Int),(Int,Int)]{ case (in,st) ⇒ st -> (in+st) }
      runV(SF.loopF(sf)(set10(ini)))(is) shouldEqual is.scanLeft(ini)(_ + _).init
    }
  }

  property("loopF is effectful and runs effect exactly once") {
    forAll{ (is: Is, ini: Int) ⇒
      val sf = arr[(Int,Int),(Int,Int)]{ case (in,st) ⇒ st -> (in+st) }
      val res = if (is.isEmpty) 0 else 1
      runS(SF.loopF(sf)(inc(ini)))(is) shouldEqual res
    }
  }
}

// vim: set ts=2 sw=2 et:
