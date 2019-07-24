/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package msf

import cats._, cats.implicits._
import cats.data.{NonEmptyList ⇒ Nel}

/**
  * Event stream transformers.
  *
  * Conceptually, event streams are signals of optional values.
  * They "fire" when they hold a "Some", otherwise they are
  * silent. We can perform operations on event streams like
  * filtering and merging them, which makes little sense for
  * continuous signals.
  *
  * Every signal can be transformed to an event stream (see [[toEF]])
  * and an event stream can be transformed back when given an
  * initial value (function [[stepper]] and its alias [[hold]]).
  *
  * When merging even streams we have to deal with the possibility
  * of simultaneous events. This can be done in several ways: Prefer
  * one of the two streams to be merged ([[unionR]] and [[unionL]])
  * or use a binary function to combine both values in case of
  * simultaneous events ([[union]]). Unifying values is trivial
  * for types with a Semigroup instance and for these we can
  * easily define a Monoid of event streams (empty being the stream
  * that never fires).
  *
  * Being a nesting of applicative functors (signal functions and
  * Options), event streams also come with an instance of Applicative.
  * Such an instance only makes sense if simultaneous events are possible.
  * This is the case for instance, when we obtain event streams from
  * filtering signals, which has an important application in validating
  * user input from several widgets in a GUI.
  *
  * This is just a wrapper for {{{ SF[F,A,Option[B]] }}} and all
  * functions are implemented using the primitives of [[msf.SF]]
  */
case class EF[F[_],A,B](sf: SF[F,A,Option[B]]) extends AnyVal {

  // ---------------------------------------------------------------
  //                    *** Arrow ***
  // ---------------------------------------------------------------
  

  /**
    * Alias for {{{ that compose this }}}
    */
  def andThen[C](that: EF[F,B,C]): EF[F,A,C] = that compose this


  /**
    * Alias for {{{ that composeS this }}}
    */
  def andThenS[C](that: SF[F,B,C]): EF[F,A,C] = that <-< this

  /**
    * Alias for {{{ that composeS this }}}
    */
  def andThenM[C:Monoid](that: SF[F,B,C]): SF[F,A,C] =
    (that <-< this).holdEmpty

  /**
    * Sinks all events through the given signal function.
    */
  def sink[C](snk: SF[F,B,Unit]): SF[F,A,Unit] = (snk <-< this) hold unit


  /**
    * Sequentially composes two event transformers.
    */
  def compose[C](that: EF[F,C,A]): EF[F,C,B] =
    EF(sf composeO that.sf map (_.flatten))


  /**
    * Build a sequence with a signal function.
    */
  def composeS[C](that: SF[F,C,A]): EF[F,C,B] = that >-> this


  /**
    * Symbolic alias for {{{ compose }}}
    */
  def <<<[C](that: EF[F,C,A]): EF[F,C,B] = compose(that)


  /**
    * Symbolic alias for {{{ andThen }}}
    */
  def >>>[C](that: EF[F,B,C]): EF[F,A,C] = andThen(that)


  /**
    * Symbolic alias for {{{ andThenS }}}
    */
  def >->[C](that: SF[F,B,C]): EF[F,A,C] = andThenS(that)


  /**
    * Symbolic alias for {{{ composeS }}}
    */
  def <-<[C](that: SF[F,C,A]): EF[F,C,B] = composeS(that)

  /**
    * Symbolic alias for {{{ sink }}}
    */
  def >>- (snk: SF[F,B,Unit]): SF[F,A,Unit] = sink(snk)

  /**
    * Symbolic alias for {{{ andThenM }}}
    */
  def >>|[C:Monoid] (that: SF[F,B,C]): SF[F,A,C] = andThenM(that)




  // ---------------------------------------------------------------
  //                    *** Merging Event Streams ***
  // ---------------------------------------------------------------
  

  /**
    * Like [[union]] using the given Semigroup's combine function
    * in case of simultaneous events.
    */
  def combine(that: EF[F,A,B])(implicit S: Semigroup[B]): EF[F,A,B] =
    union(that)(S.combine)


  /**
    * Merges two event streams. The resulting stream fires whenever
    * either of the input stream fires. In case of simultaneously
    * occuring events, the values are combined using the
    * given binary function.
    */
  def union(that: EF[F,A,B])(combine: (B,B) ⇒ B): EF[F,A,B] =
    EF(this.sf.zipWith(that.sf){
      case (Some(b1),Some(b2)) ⇒ Some(combine(b1, b2))
      case (ob1, ob2)          ⇒ ob1 orElse ob2
    })


  /**
    * Merges two event streams. The resulting stream fires whenever
    * either of the input stream fires. In case of simultaneously
    * occuring events, precedence is given to the left stream,
    * the value of the right stream being dropped.
    *
    * Therfore, in case of
    *
    *   {{{ s1 unionL s2 }}}
    *
    * if {{{ s1 }}} fires {{{ b1 }}} and {{{ s2 }}} fires {{{ b2 }}},
    * the only {{{ b1 }}} is passed on.
    */
  def unionL(that: EF[F,A,B]): EF[F,A,B] = union(that)((b,_) ⇒ b)

  /**
    * Merges two event streams. The resulting stream fires whenever
    * either of the input stream fires. In case of simultaneously
    * occuring events, precedence is given to the right stream,
    * the value of the left stream being dropped.
    *
    * Therfore, in case of
    *
    *   {{{ s1 unionL s2 }}}
    *
    * if {{{ s1 }}} fires {{{ b1 }}} and {{{ s2 }}} fires {{{ b2 }}},
    * the only {{{ b2 }}} is passed on.
    */
  def unionR(that: EF[F,A,B]): EF[F,A,B] = union(that)((_,b) ⇒ b)



  /**
    * Symbolic alias for {{{ combine }}}
    */
  def |+|(that: EF[F,A,B])(implicit S: Semigroup[B]): EF[F,A,B] =
    union(that)(S.combine)

  /**
    * Symbolic alias for {{{ unionL }}}
    */
  def <|>(that: EF[F,A,B]): EF[F,A,B] = unionL(that)




  // ---------------------------------------------------------------
  //                    *** Filtering ***
  // ---------------------------------------------------------------
  

  /**
    * A convenient combination of {{{ filter }}} and {{{ map }}}
    * using a partial function.
    */
  def collect[C](pf: PartialFunction[B,C]): EF[F,A,C] =
    EF(sf map { _ collect pf })


  /**
    * Like {{{ collect }}} but with an effectful computation.
    */
  def collectF[C](pf: PartialFunction[B,F[C]])
    (implicit F: Applicative[F]): EF[F,A,C] =
    EF(sf mapF { _ collect pf sequence })


  /**
    * A convenient combination of {{{ filter }}} and {{{ map }}}
    * using a function yielding an Option.
    */
  def collectOF[C](f: B ⇒ F[Option[C]])
    (implicit F: Applicative[F]): EF[F,A,C] =
    EF(sf mapF (_ traverse f map (_.flatten)))


  /**
    * A convenient combination of {{{ filter }}} and {{{ map }}}
    * using a function yielding an Option.
    */
  def collectO[C](f: B ⇒ Option[C]): EF[F,A,C] =
    EF(sf map (_ flatMap f))


  /**
    * Fires only events fulfilling the given predicate.
    */
  def filter(f: B ⇒ Boolean): EF[F,A,B] = EF(sf map { _ filter f })


  /**
    * Fires only events not fulfilling the given predicate.
    */
  def filterNot(f: B ⇒ Boolean): EF[F,A,B] = filter(b ⇒ !f(b))




  // ---------------------------------------------------------------
  //                    *** Applicative ***
  // ---------------------------------------------------------------
  

  /**
    * Applicative ap.
    */
  def ap[C](m: EF[F,A,B ⇒ C]): EF[F,A,C] =
    EF(sf.zipWith(m.sf)((ob,of) ⇒ Applicative[Option].ap(of)(ob)))


  /**
    * Alias for {{{ this map (_ ⇒ c) }}}
    */
  def as[C](c: C): EF[F,A,C] = map(_ ⇒ c)


  /**
    * Alias for {{{ this map (_ ⇒ c) }}}
    */
  def asF[C](c: F[C])(implicit F: Applicative[F]): EF[F,A,C] =
    mapF(_ ⇒ c)


  /**
    * Generates input values by applying the given function
    */
  def contramap[C](f: C ⇒ A): EF[F,C,B] = this <<< EF.arr(f)


  /**
    * Generates input values by applying the given function
    */
  def contramapF[C](f: C ⇒ F[A]): EF[F,C,B] =
    this <<< EF.liftS(f)


  /**
    * Functor map.
    */
  def map[C](f: B ⇒ C): EF[F,A,C] = EF(sf map (_ map f))


  /**
    * Maps an effectful computation over this signal function.
    */
  def mapF[C](f: B ⇒ F[C])(implicit F: Applicative[F]): EF[F,A,C] =
    EF(sf mapF (_ traverse f))


  /**
    * Evaluates the given effect, discarding its result
    */
  def effect[C](eff: F[C])(implicit F: Applicative[F]): EF[F,A,B] =
    mapF(eff as _)


  /**
    * Functor map.
    */
  def pair: EF[F,A,(B,B)] = map(b ⇒ (b,b))


  /**
    * Alias for {{{ this.map(()) }}}
    */
  def void: EF[F,A,Unit] = as(())


  /**
    * Pairs the output of two event streams. The resulting stream
    * fires only in case of simultaneous events from both
    * input streams
    */
  def zip[C](m: EF[F,A,C]): EF[F,A,(B,C)] =
    EF(sf.zipWith(m.sf)(Applicative[Option].map2(_,_)((_,_))))

  /**
    * Pairs the output of two event streams, applying the
    * given pure function afterwards. The resulting stream
    * fires only in case of simultaneous events from both
    * input streams
    */
  def zipWith[C,D](m: EF[F,A,C])(f: (B,C) ⇒ D): EF[F,A,D] =
    zip(m) map { case (b,c) ⇒ f(b,c) }


  /**
    * Symbolic alias for {{{ this mapF f }}}
    */
  def >- (f: B ⇒ F[Unit])(implicit F: Applicative[F]): EF[F,A,Unit] = mapF(f)


  /**
    * Symbolic alias for {{{ zip }}}
    */
  def &&& [C](that: EF[F,A,C]): EF[F,A,(B,C)] = zip(that)


  /**
    * Splits this event stream, passing values through two
    * transformers, combining the results afterwards.
    */
  def diamond[C,D,E](ef1: EF[F,B,C], ef2: EF[F,B,D])(f: (C,D) ⇒ E): EF[F,A,E] =
    this >>> (ef1 &&& ef2) >>> EF.arr{ case (c,d) ⇒ f(c,d) }


  /**
    * Passes the results through a second stream transformer,
    * while dropping the values it produces.
    *
    * Alias for {{{ diamond(that, id)((,_b) ⇒ b) }}}
    */
  def observe[C](that: SF[F,B,C]): EF[F,A,B] =
    diamond(that.toEF, EF.id)((_,b) ⇒ b)


  /**
    * Symbolic alias for {{{ observe }}}
    */
  def >>* [C](that: SF[F,B,C]): EF[F,A,B] = observe(that)




  // ---------------------------------------------------------------
  //           *** Interaction with Signal Functions ***
  // ---------------------------------------------------------------


  /**
    * Alias for {{{ this stepper b }}}
    */
  def hold(b: B): SF[F,A,B] = stepper(b)


  /**
    * Alias for {{{ this stepper b }}}
    */
  def holdEmpty(implicit M: Monoid[B]): SF[F,A,B] = stepper(M.empty)


  /**
    * Alias for {{{ this stepperF b }}}
    */
  def holdF(b: F[B])(implicit F: Functor[F]): SF[F,A,B] = stepperF(b)


  /**
    * Converts this event stream to a signal function, starting
    * with the given value and changing whenever this stream
    * fires.
    */
  def stepper(b: B): SF[F,A,B] =
    sf.scan(b){ case (bb,ob) ⇒ ob getOrElse bb }

  /**
    * Holds the given value whenever this event stream
    * does not hold a value
    */
  def onEmpty(b: B): SF[F,A,B] = sf map (_ getOrElse b)

  /**
    * Evaluates both signal functions, holding the given signals
    * value if this event stream does not hold a value
    */
  def onEmptyS(b: SF[F,A,B]): SF[F,A,B] = (sf,b).mapN(_ getOrElse _)


  /**
    * Like [[stepper]] but starts with a value from an
    * effectful computation.
    */
  def stepperF(b: F[B])(implicit F: Functor[F]): SF[F,A,B] =
    sf.scanF(b){ case (bb,ob) ⇒ ob getOrElse bb }




  // ---------------------------------------------------------------
  //               *** Value Accumulation ***
  // ---------------------------------------------------------------

  def zipWithIndex : EF[F,A,(B,Int)] = EF(
    sf.scan(none[B] -> 0){
      case ((_,n),o) ⇒ o -> (if (o.nonEmpty) n+1 else n)
    }.map{ case (ob,n) ⇒ ob map (_ -> n)}
  )

  def take(n: Int): EF[F,A,B] =
    zipWithIndex.collect{ case (b,x) if x <= n ⇒ b }

  def drop(n: Int): EF[F,A,B] =
    zipWithIndex.collect{ case (b,x) if x > n ⇒ b }

  def head: EF[F,A,B] = take(1)

  def tail: EF[F,A,B] = drop(1)

  /**
    * Counts the number of event occurences
    */
  def count: EF[F,A,Int] = scan(0)((n,_) ⇒ n+1)

  /**
    * Appends the given value thus delaying the output
    * of all later values by one.
    *
    * This is useful for instance to perform calculations
    * based on successive events.
    */
  def cons1(b: B): EF[F,A,B] = cons(Vector(b))

  /**
    * Prepends the given values thus delaying the output
    * of all later values.
    *
    * This is useful for instance to perform calculations
    * based on prior events.
    *
    * Note: For the time being the implementation is note
    * efficient (O(n) on the length of the list), since
    * the usefulness of this for large lists has yet
    * to be shown.
    */
  def cons(bs: Vector[B]): EF[F,A,B] = {
    def next(p: (Vector[B], Boolean), b: B): (Vector[B],Boolean) =
      if (p._2) (p._1 :+ b) -> false
      else (p._1.tail :+ b) -> false
    
    scan(bs -> true)(next) map (_._1.head)
  }

  /**
    * Repeats the first return value once.
    *
    * This is an alias for {{{ delayN(1) }}}.
    */
  def delay1: EF[F,A,B] = delayN(1)

  /**
    * Repeats the first return value of this signal function
    * an additional n-times
    * (resulting in a signal holding the initial value n+1 times).
    *
    * Law: delayN(0) == id
    */
  def delayN(n: Int): EF[F,A,B] = {
    def next(p: (Vector[B], Int), b: B): (Vector[B],Int) =
      if (p._2 > 0) (p._1 :+ b) -> (p._2 - 1)
      else (p._1.tail :+ b) -> 0

    scan(Vector[B]() -> n)(next) map (_._1.head)
  }

  /**
    * Accumulates the last n values
    */
  def accumN(n: Int): EF[F,A,List[B]] = {
    def next(p: List[B], b: B): List[B] = p match {
      case Nil ⇒ b::Nil
      case bs  if bs.size >= n ⇒ b::(bs.init)
      case bs                  ⇒ b::bs
    }

    scan(List[B]())(next)
  }


  /**
    * Accumulates values using a binary function and
    * an initial value as a seed.
    */
  def scan[C](ini: C)(f: (C,B) ⇒ C): EF[F,A,C] =
    scanO(ini)((c,b) ⇒ some(f(c,b)))

  /**
    * Accumulates values using a binary function and
    * an initial value as a seed.
    */
  def scanO[C](ini: C)(f: (C,B) ⇒ Option[C]): EF[F,A,C] = {
    val acc: ((Option[B],C)) ⇒ (Option[C],C) = {
      case (Some(b),c) ⇒ f(c,b) match {
        case Some(newC) ⇒ some(newC) -> newC
        case None       ⇒ none[C] -> c
      }
      case (None, c)   ⇒ none[C] -> c
    }

    EF(SF.loop(sf.first[C] map acc)(ini))
  }

  /**
    * Accumulates values using a binary function and
    * an initial value as a seed.
    */
  def scanEval[C](ini: C)(f: (C,B) ⇒ F[C])(implicit A: Applicative[F])
    : EF[F,A,C] = scanEvalO(ini)((c,b) ⇒ f(c,b) map some)

  /**
    * Accumulates values using a binary effectful function and
    * an initial value as a seed.
    */
  def scanEvalO[C](ini: C)
    (f: (C,B) ⇒ F[Option[C]])(implicit A: Applicative[F]): EF[F,A,C] = {
    val acc: ((Option[B],C)) ⇒ F[(Option[C],C)] = {
      case (Some(b),c) ⇒ f(c,b) map {
        case Some(newC) ⇒ some(newC) -> newC
        case None       ⇒ none[C] -> c
      }
      case (None, c)   ⇒ A pure (none[C] -> c)
    }

    EF(SF.loop(sf.first[C] mapF acc)(ini))
  }

  def distinct(implicit E: Eq[B]): EF[F,A,B] = {
    val acc: ((Option[B],Option[B])) ⇒ (Option[B],Option[B]) = {
      case (on@Some(n),Some(o)) if n =!= o ⇒ on -> on
      case (o,None)                        ⇒ o -> o
      case (_, o)                          ⇒ none[B] -> o
    }

    EF(SF.loop(sf.first[Option[B]] map acc)(None))
  }

  def flip[C](c1: C, c2: C): EF[F,A,C] = rotate[C](Nel.of(c1,c2)) 

  def rotate[C](c: C, cs: C*): EF[F,A,C] = rotate(Nel.of(c,cs: _*))

  def rotate[C](cs: Nel[C]): EF[F,A,C] =
    scan(cs){ case (Nel(h,t), _) ⇒ {
      val newl = t ::: List(h)
      Nel(newl.head, newl.tail)
    }} map (_.head)

  def toggle(ini: Boolean): SF[F,A,Boolean] = flip(ini, !ini) hold ini


  /**
    * Accumulates values using a binary function and
    * an initial value as a seed.
    */
  def scanF[C](ini: F[C])(f: (C,B) ⇒ C)(implicit F: Functor[F]): EF[F,A,C] = {
    val acc: ((Option[B],C)) ⇒ (Option[C],C) = {
      case (Some(b),c) ⇒ f(c,b) match {
        case newC      ⇒ some(newC) -> newC
      }
      case (None, c)   ⇒ none[C] -> c
    }

    EF(SF.loopF(sf.first[C] map acc)(ini))
  }


  /**
    * Accumulates values using a binary function.
    */
  def scan1(f: (B,B) ⇒ B): EF[F,A,B] = {
    val acc: ((Option[B],Option[B])) ⇒ (Option[B],Option[B]) = {
      case (Some(b1), Some(b2)) ⇒ f(b1,b2) match {
        case newB        ⇒ some(newB) -> some(newB)
      }
      case (o1, o2)     ⇒ o1 -> o1.orElse(o2)
    }

    EF(SF.loop(sf.first[Option[B]] map acc)(None))
  }




  // ---------------------------------------------------------------
  //                 *** Control Functions ***
  // ---------------------------------------------------------------


  /**
    * Performs the given effect only the first time
    * this event stream is invoked.
    *
    * This is useful for initialization purposes, for instance
    * when setting up event listeners in graphical user interfaces.
    * See also package [[ msf.ui ]]
    */
  def init(run: F[Unit])(implicit F: Applicative[F]): EF[F,A,B] =
    once(_ ⇒ run)


  /**
    * Performs an effect on the first input value.
    *
    * This is useful for initialization purposes, for instance
    * when setting up event listeners in graphical user interfaces.
    * See also package [[ msf.ui ]]
    */
  def once(run: A ⇒ F[Unit])(implicit F: Applicative[F]): EF[F,A,B] =
    EF(sf once run)


  /**
    * Performs an effect on every input value.
    */
  def always(run: A ⇒ F[Unit]): EF[F,A,B] = EF(sf always run)

  def translate[G[_]:Functor](f: F ~> G): EF[G,A,B] = EF(sf translate f)

}

object EF {

  def collect[F[_],A,B](pf: PartialFunction[A,B]): EF[F,A,B] =
    SF.id[F,A] collect pf

  def collectF[F[_]:Applicative,A,B](pf: PartialFunction[A,F[B]]): EF[F,A,B] =
    SF.id[F,A] collectF pf

  /**
    * The empty stream that never fires.
    */
  def never[F[_],A,B]: EF[F,A,B] =
    EF(SF.const[F,A,Option[B]](None))


  /**
    * Returns an event stream, which applies the given effectful
    * computation to its input values
    */
  def liftS[F[_],A,B](f: A ⇒ F[B]): EF[F,A,B] =
    EF(SF liftS f map some)


  /**
    * The event stream
    */
  def id[F[_],A]: EF[F,A,A] = arr(identity)


  /**
    * Returns an event stream transformer, which applies the given pure
    * function to every input value
    */
  def arr[F[_],A,B](f: A ⇒ B): EF[F,A,B] =
    EF(SF.arr[F,A,Option[B]](a ⇒ some(f(a))))


  /**
    * Alias for {{{ arr(_ ⇒ b) }}}
    */
  def const[F[_],A,B](b: B): EF[F,A,B] = arr(_ ⇒ b)


  /**
    * Alias for {{{ arr(_ ⇒ b) }}}
    */
  def const_[F[_],B](b: B): EF[F,Unit,B] = const(b)


  /**
    * Alias for {{{ liftS(_ ⇒ b) }}}
    */
  def constS[F[_],A,B](b: F[B]): EF[F,A,B] =
    liftS(_ ⇒ b)


  /**
    * Alias for {{{ liftS(_ ⇒ b) }}}
    */
  def constS_[F[_],B](b: F[B]): EF[F,Unit,B] =
    constS(b)

  def switch[F[_],A,B](ef: EF[F,A,EF[F,A,B]]): EF[F,A,B] =
    EF(never[F,A,B].sf switch ef.map(_.sf))

  def switch_[F[_],A,B](ef: EF[F,A,EF[F,Unit,B]]): EF[F,A,B] =
    EF(never[F,A,B].sf switch_ ef.map(_.sf))

  /**
    * Hides initialization in the F monad
    */
  def joinF[F[_],A,B](ef: F[EF[F,A,B]])(implicit F: Functor[F]): EF[F,A,B] =
    EF(SF.joinF(ef map (_.sf)))

  def helper[F[_]]: EFHelper[F] = new EFHelper[F]{}

  implicit def appI[F[_],A]: Applicative[EF[F,A,?]] =
    new Applicative[EF[F,A,?]] {
      def pure[B](b: B) = const(b)
      def ap[B,C](fa: EF[F,A,B ⇒ C])(f: EF[F,A,B]) = f ap fa
      override def map[B,C](fb: EF[F,A,B])(f: B ⇒ C) = fb map f
    }

  implicit def monoidI[F[_],A,B:Semigroup]: Monoid[EF[F,A,B]] =
    new Monoid[EF[F,A,B]] {
      def empty = never[F,A,B]
      def combine(e1: EF[F,A,B], e2: EF[F,A,B]) = e1 combine e2
    }
}

/**
  * Provides an environment together with utility functions for
  * a given effect [[F]].
  */
trait EFHelper[F[_]] {
  type EF[A,B] = msf.EF[F,A,B]

  def collect[A,B](pf: PartialFunction[A,B]): EF[A,B] =
    SF.id[F,A] collect pf

  def collectF[A,B](pf: PartialFunction[A,F[B]])(implicit A: Applicative[F]): EF[A,B] =
    SF.id[F,A] collectF pf

  /**
    * The empty stream that never fires.
    */
  def never[A,B]: EF[A,B] = EF.never

  def liftS[A,B](f: A ⇒ F[B]): EF[A,B] = EF liftS f

  def idE[A]: EF[A,A] = EF.id

  def arr[A,B](f: A ⇒ B): EF[A,B] = EF arr f

  def const[A,B](b: B): EF[A,B] = EF const b

  def const_[B](b: B): EF[Unit,B] = const(b)

  def constS[A,B](b: F[B]): EF[A,B] = liftS(_ ⇒ b)

  def constS_[B](b: F[B]): EF[Unit,B] = constS(b)

  def switch[A,B](ef: EF[A,EF[A,B]]): EF[A,B] = EF switch ef

  def switch_[A,B](ef: EF[A,EF[Unit,B]]): EF[A,B] = EF switch_ ef
    
  def joinF[A,B](ef: F[EF[A,B]])(implicit F: Functor[F]): EF[A,B] =
    EF(SF.joinF(ef map (_.sf)))
}

// vim: set ts=2 sw=2 et:

