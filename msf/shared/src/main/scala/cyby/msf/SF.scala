/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package msf

import cats._, cats.implicits._
import cats.arrow._

/**
  * Signal Function
  *
  * In the spirit of arrowized functional reactive programming,
  * this class can be used to describe reactive networks and data
  * transformations.
  *
  * The implementation is based on the paper on Monadic Stream
  * Functions by Ivan Perez, Manuel Bärenz, and Henrik Nilsson:
  *
  * @TODO: Add URL
  *
  * It allows abstracting not only over the input and output type
  * but also over the Monad in which the calculations should
  * be performed.
  *
  * Monadic Streaming Functions are instances of Arrow and
  * Arrow Choice.
  *
  * @tparam F : The Effect in which the signal function will run
  * @tparam A : Input type
  * @tparam B : Output type
  *
  */
sealed trait SF[F[_],A,B] {
  import SF.{arr, liftS, id}

  /**
    * The result of applying a signal function to an input of
    * type @tparam A is an output value of type @tparam B
    * paired with a continuation, both of which are returned
    * in the effect @tparam F.
    */
  type Res = F[(B, SF[F,A,B])]


  // ---------------------------------------------------------------
  //                    *** Primitives ***
  // ---------------------------------------------------------------


  /**
    * Sequentially composes two signal functions
    */
  def compose[C](that: SF[F,C,A]): SF[F,C,B] = Compose(this,that)


  /**
    * Threads a second value of type @tpara C through
    * this signal function, leaving it unmodified.
    */
  def first[C]: SF[F,(A, C),(B, C)] = First[F,A,B,C](this)


  /**
    * Choose between two input values, applying this signal function
    * in case of a Left and letting a Right pass unmodified.
    * This is the primitive function of [[ cats.arrow.ArrowChoice ]].
    */
  def left[C]: SF[F,Either[A,C],Either[B,C]] = LeftSF[F,A,B,C](this)


  /**
    * Switches to a new signal function whenever the given
    * event stream fires.
    */
  def switch(ef: EF[F,A,SF[F,A,B]]): SF[F,A,B] = Switch[F,A,B](this, ef.sf)

  def switch_(ef: EF[F,A,SF[F,Unit,B]]): SF[F,A,B] =
    switch(ef map (SF.const[F,A,Unit](()) >>> _))


  // ---------------------------------------------------------------
  //                    *** Arrow ***
  // ---------------------------------------------------------------


  /**
    * Alias for {{{ that compose this }}}
    */
  def andThen[C](that: SF[F,B,C]): SF[F,A,C] = that compose this


  /**
    * Chooses between this an another arrow function depending
    * on the input value unifying the output.
    */
  def choice[A2](that: SF[F,A2,B]): SF[F,Either[A,A2],B] =
    this either that map (_.fold(identity, identity))


  /**
    * Sequential composition with a signal function returning
    * optional values. This is typically used to compose
    * event streams.
    */
  def composeO[C](that: SF[F,C,Option[A]]): SF[F,C,Option[B]] =
    that >>> arr(_.toRight(())) >>> right >>> arr(_.toOption)


  /**
    * Splits this signal, passing values through two
    * signal functions, combining the results afterwards.
    */
  def diamond[C,D,E](sf1: SF[F,B,C], sf2: SF[F,B,D])(f: (C,D) ⇒ E): SF[F,A,E] =
    this >>> (sf1 &&& sf2) >>> arr{ case (c,d) ⇒ f(c,d) }


  /**
    * Chooses between this an another arrow function depending
    * on the input value.
    */
  def either[A2,B2](that: SF[F,A2,B2]): SF[F,Either[A,A2],Either[B,B2]] =
    this.left[A2] >>> that.right[B]


  /**
    * Passes the results through a second signal function,
    * while dropping the values it produces.
    *
    * Alias for {{{ diamond(that, id)((,_b) ⇒ b) }}}
    */
  def observe[C](that: SF[F,B,C]): SF[F,A,B] =
    diamond(that, id)((_,b) ⇒ b)

  /**
    * Splits this SF in two passing one branch through the
    * given signal function.
    */
  def branch[C](that : SF[F,B,C]): SF[F,A,(C,B)] = this >>> (that &&& SF.id)

  /**
    * Alias for {{{ branch(that) }}}
    */
  def --<[C](that: SF[F,B,C]): SF[F,A,(C,B)] = branch(that)

  /**
    * Choose between two input values, applying this signal function
    * in case of a Right and letting a Left pass unmodified.
    *
    * This is just a mirrored version of [[left]], and thus not
    * really a primitive though it is implemented as one for reasons
    * of symmetry.
    */
  def right[C]: SF[F,Either[C,A],Either[C,B]] = RightSF[F,A,B,C](this)


  /**
    * Threads a second value of type @tpara C through
    * this signal function
    *
    * Note: For reasons of efficiency, this is implemented
    * as a primitive. It is, however an alias for
    * {{{ SF.swap >>> first[C] >>> SF.swap }}}.
    */
  def second[C]: SF[F,(C, A),(C, B)] = Second[F,A,B,C](this)


  /**
    * Parallel composition of signal functions.
    */
  def split[C,D](that: SF[F,C,D]): SF[F,(A,C),(B,D)] =
    first[C] >>> that.second[B]


  /**
    * Symbolic alias for {{{ compose }}}
    */
  def <<< [C](that: SF[F,C,A]): SF[F,C,B] = compose(that)


  /**
    * Symbolic alias for {{{ observe }}}
    */
  def >>* [C](that: SF[F,B,C]): SF[F,A,B] = observe(that)


  /**
    * Symbolic alias for {{{ andThen }}}
    */
  def >>> [C](that: SF[F,B,C]): SF[F,A,C] = andThen(that)


  /**
    * Symbolic alias for {{{ split }}}
    */
  def *** [C,D](that: SF[F,C,D]): SF[F,(A,C),(B,D)] = split(that)


  /**
    * Symbolic alias for {{{ zip }}}
    */
  def &&& [C](that: SF[F,A,C]): SF[F,A,(B,C)] = zip(that)


  /**
    * Symbolic alias for {{{ choice }}}
    */
  def ||| [A2](that: SF[F,A2,B]): SF[F,Either[A,A2],B] = choice(that)


  /**
    * Symbolic alias for {{{ either }}}
    */
  def +++ [A2,B2](that: SF[F,A2,B2]): SF[F,Either[A,A2],Either[B,B2]] =
    either(that)




  // ---------------------------------------------------------------
  //                    *** Applicative ***
  // ---------------------------------------------------------------


  /**
    * Applicative ap.
    */
  def ap[C](m: SF[F,A,B ⇒ C]): SF[F,A,C] = zipWith(m)((a,f) ⇒ f(a))


  /**
    * Alias for {{{ this map (_ ⇒ c) }}}
    */
  def as[C](c: C): SF[F,A,C] = map(_ ⇒ c)


  /**
    * Generates input values by applying the given function
    */
  def contramap[C](f: C ⇒ A): SF[F,C,B] = this <<< arr(f)


  /**
    * Generates input values by applying the given function
    */
  def contramapF[C](f: C ⇒ F[A]): SF[F,C,B] =
    this <<< liftS(f)


  /**
    * Applies transformations both to input and output
    * values, thus combining the effect of map and contramap.
    */
  def dimap[C,D](f: C ⇒ A)(g: B ⇒ D): SF[F,C,D] = map(g) contramap f


  /**
    * Like dimap but with effectful computations.
    */
  def dimapF[C,D](f: C ⇒ F[A])(g: B ⇒ F[D]): SF[F,C,D] =
    mapF(g) contramapF f


  /**
    * Functor map.
    */
  def map[C](f: B ⇒ C): SF[F,A,C] = this >>> arr(f)


  /**
    * Maps an effectful computation over this signal function.
    */
  def mapF[C](f: B ⇒ F[C]): SF[F,A,C] =
    this >>> liftS(f)


  /**
    * Evaluates the given effect, discarding its result
    */
  def effect[C](eff: F[C])(implicit F: Functor[F]): SF[F,A,B] =
    mapF(eff as _)


  /**
    * Alias for {{{ map(b ⇒ (b,b)) }}}
    *
    * Note that for effectful signal functions this is is not the
    * same as {{{ this &&& this }}},
    * since in the latter case the effects involved are performed twice.
    */
  def pair: SF[F,A,(B,B)] = map(b ⇒ (b,b))


  /**
    * Alias for {{{ this.map(()) }}}
    */
  def void: SF[F,A,Unit] = as(())


  /**
    * Observers this signal through the given function.
    */
  def >- (f: B ⇒ F[Unit]): SF[F,A,Unit] = mapF(f)


  /**
    * Pairs the output of two signal functions
    */
  def zip[C](that: SF[F,A,C]): SF[F,A,(B,C)] =
    (this *** that) <<< id.pair


  /**
    * Pairs the output of two signal functions, applying the
    * given pure function afterwards.
    */
  def zipWith[C,D](m: SF[F,A,C])(f: (B,C) ⇒ D): SF[F,A,D] =
    zip(m) map { case (b,c) ⇒ f(b,c) }


  def |+|(that: SF[F,A,B])(implicit S: Semigroup[B]): SF[F,A,B] =
    zipWith(that)(S.combine)




  // ---------------------------------------------------------------
  //               *** Value Accumulation ***
  // ---------------------------------------------------------------


  /**
    * Counts the number of event occurences
    */
  def count: SF[F,A,Int] = scan(0)((n,_) ⇒ n+1)


  /**
    * Appends the given value thus delaying the output
    * of all later values by one.
    *
    * This is useful for instance to perform calculations
    * based on successive events.
    */
  def cons1(b: B): SF[F,A,B] = cons(Vector(b))

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
  def cons(bs: Vector[B]): SF[F,A,B] = {
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
  def delay1: SF[F,A,B] = delayN(1)

  /**
    * Repeats the first return value of this signal function
    * an additional n-times
    * (resulting in a signal holding the initial value n+1 times).
    *
    * Law: delayN(0) == id
    */
  def delayN(n: Int): SF[F,A,B] = {
    def next(p: (Vector[B], Int), b: B): (Vector[B],Int) =
      if (p._2 > 0) (p._1 :+ b) -> (p._2 - 1)
      else (p._1.tail :+ b) -> 0

    scan(Vector[B]() -> n)(next) map (_._1.head)
  }

  /**
    * Accumulates values using the given initial value
    * plus a binary function.
    */
  def scan[C](ini: C)(f: (C,B) ⇒ C): SF[F,A,C] =
    SF.loop(first[C] map { case (c,b) ⇒ f(b,c) } pair)(ini)


  /**
    * Accumulates values using the given initial effectful value
    * plus a binary function.
    */
  def scanF[C](ini: F[C])(f: (C,B) ⇒ C)(implicit F: Functor[F]): SF[F,A,C] =
    SF.loopF(first[C] map { case (c,b) ⇒ f(b,c) } pair)(ini)

  /**
    * Accumulates values using the given initial value
    * plus a binary effectful function.
    */
  def scanEval[C](ini: C)(f: (C,B) ⇒ F[C]): SF[F,A,C] =
    SF.loop(first[C] mapF { case (c,b) ⇒ f(b,c) } pair)(ini)


  /**
    * Accumulates values starting with the first value.
    */
  def scan1(f: (B,B) ⇒ B): SF[F,A,B] = {
    def acc: ((B,Option[B])) ⇒ (B,Option[B]) = {
      case (b1,ob) ⇒ {
        val newB = ob.fold(b1)(b2 ⇒ f(b1,b2))
        newB -> Some(newB)
      }
    }

    SF.loop(first[Option[B]] map acc)(None)
  }




  // ---------------------------------------------------------------
  //           *** Interaction with EventStreams ***
  // ---------------------------------------------------------------


  /**
    * Sequentially combines this signal function with the given event
    * stream transformer.
    */
  def andThenE[C](ef: EF[F,B,C]): EF[F,A,C] = EF(this >>> ef.sf)


  /**
    * A convenient combination of {{{ filter }}} and {{{ map }}}.
    */
  def collect[C](pf: PartialFunction[B,C]): EF[F,A,C] =
    toEF collect pf

  /**
    * A convenient combination of {{{ filter }}} and {{{ map }}}.
    */
  def collectO[C](f: B ⇒ Option[C]): EF[F,A,C] = toEF collectO f


  /**
    * Like {{{ collect }}} but with an effectful computation.
    */
  def collectF[C](pf: PartialFunction[B,F[C]])
    (implicit F: Applicative[F]): EF[F,A,C] =
    toEF collectF pf


  /**
    * Sequentially combines the given event stream transformer
    * with this signal function
    */
  def composeE[C](ef: EF[F,C,A]): EF[F,C,B] =
    EF(composeO(ef.sf))


  /**
    * Returns an event stream which fires whenever the result
    * of this signal function matches a given predicate.
    */
  def filter(p: B ⇒ Boolean): EF[F,A,B] = toEF filter p

  /**
    * Returns an event stream which fires whenever the result
    * of this signal function does not match a given predicate.
    */
  def filterNot(p: B ⇒ Boolean): EF[F,A,B] = filter(b ⇒ !p(b))


  /**
    * Returns an event stream that fires only when the input event
    * stream fires in which case the output is calculated
    * from the value of this signal function and the fired event
    * using the supplied binary function.
    */
  def onWith[C,D](ef: EF[F,A,C])(f: (B,C) ⇒ D): EF[F,A,D] =
    zipE(ef) map { case (b,c) ⇒ f(b,c) }

  /**
    * Returns an event stream that fires only when the input event
    * stream fires in which case the output is calculated
    * from the value of this signal function and the fired event
    * using the supplied binary function.
    */
  def onWithF[C,D](ef: EF[F,A,C])(f: (B,C) ⇒ F[D])(
    implicit A: Applicative[F]): EF[F,A,D] =
    onWith(ef)(f).mapF(identity)


  /**
    * Returns an event stream that fires this signal function's
    * value whenever the given event stream fires.
    */
  def on[C](ef: EF[F,A,C]): EF[F,A,B] = onWith(ef)((b,_) ⇒ b)


  /**
    * Converts this signal function to an event stream.
    *
    * Note, that conceptually, the two functions are still the
    * same since the resulting event stream fires on every input.
    * This function is merely the starting point for filtering
    * functions like {{{ filter }}} and {{{ collect }}}.
    */
  def toEF: EF[F,A,B] = EF(this map some)


  /**
    * Returns an event stream holding both the original
    * event stream's values plus this signal function's values
    * whenever the original event stream fires.
    */
  def zipE[C](ef: EF[F,A,C]): EF[F,A,(B,C)] =
    EF(zipWith(ef.sf){ case (b,oc) ⇒ oc map (b -> _) })


  /**
    * Alias for {{{ on }}}
    */
  def zipWithE[C,D](ef: EF[F,A,C])(f: (B,C) ⇒ D): EF[F,A,D] =
    onWith(ef)(f)


  /**
    * Alias for {{{ this andThenE ef }}}
    */
  def >->[C](ef: EF[F,B,C]): EF[F,A,C] = andThenE(ef)


  /**
    * Alias for {{{ this composeE ef }}}
    */
  def <-<[C](ef: EF[F,C,A]): EF[F,C,B] = composeE(ef)




  // ---------------------------------------------------------------
  //                 *** Control Functions ***
  // ---------------------------------------------------------------

  /**
    * Performs the given effect only the first time
    * this signal function is invoked.
    *
    * This is useful for initialization purposes, for instance
    * when setting up event listeners in graphical user interfaces.
    */
  def init(run: F[Unit])(implicit F: Monad[F]): SF[F,A,B] =
    once(_ ⇒ run)


  /**
    * Performs an effect on the first input value.
    *
    * This is useful for initialization purposes, for instance
    * when setting up event listeners in graphical user interfaces.
    * See also package [[ msf.ui ]]
    */
  def once(run: A ⇒ F[Unit])(implicit F: Applicative[F]): SF[F,A,B] =
    SF.once(run) >>> this


  /**
    * Performs an effect on every input value.
    */
  def always(run: A ⇒ F[Unit]): SF[F,A,B] =
    zipWith(liftS(run))((a,_) ⇒ a)


  def translate[G[_]:Functor](f: F ~> G): SF[G,A,B]



  // ---------------------------------------------------------------
  //           *** Running Signal Functions  ***
  // ---------------------------------------------------------------
 

  /**
    * Applies the signal function to the given list
    * of input values. Useful for testing.
    */
  def embed(as: List[A])(implicit F: Monad[F]): F[List[B]] = as match {
    case h::t ⇒ for {
      p    <- step(h)
      rest <- p._2 embed t 
    } yield p._1 :: rest

    case Nil  ⇒ F pure Nil
  }


  /**
    * Runs the signal function, dropping the continuation
    */
  def head(a: A)(implicit F: Monad[F]): F[B] = step(a) map (_._1)


  /**
    * Runs the signal function in effect @tparam F returning a
    * value of type @tparam B and a continuation.
    */
  def step(a: A)(implicit M: Monad[F]): Res


  /**
    * Runs the signal function, returning only the continuation
    */
  def tail(a: A)(implicit F: Monad[F]): F[SF[F,A,B]] = step(a) map (_._2)
}


object SF {
  // ---------------------------------------------------------------
  //                    *** Primitives ***
  // ---------------------------------------------------------------
  

  /**
    * Creates a signal function from the given effectful function.
    *
    * BEWARE: This should usually not be used in client code. It
    * is sometimes needed to create new signal function from
    * existing once by decomposing the effect F. See for instance
    * [[msf.reader]] and [[msf.either]]
    */
  def apply[F[_],A,B](st: A ⇒ F[(B,SF[F,A,B])]): SF[F,A,B] =
    Primitive(st)

  
  /**
    * Returns a signal function, which applies the given effectful
    * computation to its input values
    */
  def liftS[F[_],A,B](f: A ⇒ F[B]): SF[F,A,B] = Lifted(f)


  /**
    * Feedbacks one of the output signals back as the input
    * starting with a given effectful value.
    */
  def loopF[F[_]:Functor,A,B,C](sf: SF[F,(A,C),(B,C)])(c: F[C]): SF[F,A,B] =
    Loop2F[F,A,B,C](sf,c)

  /**
    * Feedbacks an output signal back as its input, producing
    * looped back value as a result
    */
  def loopPF[F[_]:Functor,A,B](sf: SF[F,(A,B),B])(b: F[B]): SF[F,A,B] =
    loopF(sf.pair)(b)

  /**
    * Feedbacks an output signal back as its input, producing
    * Unit as its result
    */
  def loopF_[F[_]:Functor,A,B](sf: SF[F,(A,B),B])(b: F[B]): SF[F,A,Unit] =
    loopF(sf map (() -> _))(b)

  /**
    * Hides initialization in the F monad
    */
  def joinF[F[_],A,B](sf: F[SF[F,A,B]]): SF[F,A,B] = JoinF[F,A,B](sf)

  /**
    * Dynamic lists of signal functions:
    *
    * Given a signal function yielding lists of signal functions together
    * with event streams for terminating them, we get a signal function
    * yielding lists of values.
    *
    * Example useage: Consider a user interface displaying a dynamic
    * list of entries together with widgets for new entries and
    * buttons for deleting entries (the xs below).
    *
    *                   -------------
    *   Adress Book     | New Entry |
    *                   -------------
    *
    *   Name       |  Address
    *   ================================
    *   John       |  ...            |x|
    *   --------------------------------
    *   Susan      |  ...            |x|
    *   --------------------------------
    *
    * Button "New Entry" generates interactive signal functions
    * of editable entries together with event streams for deleting
    * these entries (the "x" buttons"). The resulting list of editable
    * entries can be modeled as a signal function of a list of addresses:
    *
    * {{{
    *   val newEntry: SF[F,A,List[(SF[F,A,Address],EF[F,A,Unit])]] = ...
    *
    *   val initialEntries: List[(SF[F,A,Address],EF[F,A,Unit])] = ...
    *
    *   val addresses: SF[F,A,List[Address]] = dynlist(newEntry, initialEntries)
    * }}}
    */
  def dynlist[F[_],A,B](
    sf: SF[F,A,List[(SF[F,A,B],EF[F,A,Unit])]],
    sfs: List[(SF[F,A,B],EF[F,A,Unit])],
  ): SF[F,A,List[B]] = DynList(sf, sfs)


  // ---------------------------------------------------------------
  //                  *** Derived Constructors ***
  // ---------------------------------------------------------------
  

  /**
    * Returns a signal function, which applies the given pure function
    * to every input value
    *
    * This is implemented as a primitive, but for every F with
    * an instance of Applicative it is actually
    * {{{ liftS(a ⇒ pure(f(a))) }}}.
    */
  def arr[F[_],A,B](f: A ⇒ B): SF[F,A,B] = Pure(f)


  /**
    * Returns a constant signal function.
    */
  def const[F[_],A,B](b: B): SF[F,A,B] = arr(_ ⇒ b)


  /**
    * Returns a constant signal function with input type Unit.
    */
  def const_[F[_],B](b: B): SF[F,Unit,B] = const(b)


  /**
    * Returns a signal function which calculates its output
    * solely by evaluating the given effectful computation.
    */
  def constS[F[_],A,B](b: F[B]): SF[F,A,B] = liftS(_ ⇒ b)


  /**
    * Alias for constS[F,Unit,B](b).
    */
  def constS_[F[_],B](b: F[B]): SF[F,Unit,B] = constS(b)


  /**
    * The identity signal function
    */
  def id[F[_],A]: SF[F,A,A] = arr(identity)

   
  /**
    * Feedbacks one of the output signals back as the input
    * starting with a given initial value.
    *
    * This is actually not a primitive, as it can be implemented
    * trivially via {{{ loopF }}} for every F with an instance
    * of Applicative.
    */
  def loop[F[_],A,B,C](sf: SF[F,(A,C),(B,C)])(c: C): SF[F,A,B] =
    id[F,A].map(_ -> c) >>> Loop2[F,A,B,C](sf, Some(c))

  def loopBack[F[_],A,B,C](sf: SF[F,(A,C),(B,C)]): SF[F,(A,C),B] =
    Loop2(sf, None)

  def loopP[F[_],A,B](sf: SF[F,(A,B),B])(b: B): SF[F,A,B] =
    loop(sf.pair)(b)

  def loop_[F[_],A,B](sf: SF[F,(A,B),B])(b: B): SF[F,A,Unit] =
    loop(sf map (() -> _))(b)

    
  /**
    * Returns a signal function which runs the given effect
    * on the first input. Useful for initialization.
    */
  def once[F[_],A](run: A ⇒ F[Unit])(implicit F: Applicative[F]): SF[F,A,A] = {

    type Fun = A ⇒ F[Unit]
    type Pair = (A, Fun)

    val doNothing: Fun = _ ⇒ F pure (())

    def next: Pair ⇒ F[Pair] = { case (a,f) ⇒ f(a) as (a -> doNothing) }

    val sf = (id[F,A] *** id[F,Fun]) >>> liftS(next)

    loop(sf)(run)
  }


  /**
    * Swaps the pair of input values.
    */
  def swap[F[_],A,B]: SF[F,(A,B),(B,A)] = arr(_.swap)




  // ---------------------------------------------------------------
  //             *** Interaction with Applicative Ask ***
  // ---------------------------------------------------------------

  def helper[F[_]]: SFHelper[F] = new SFHelper[F]{}




  // ---------------------------------------------------------------
  //                *** Typeclass Instances ***
  // ---------------------------------------------------------------
  

  // Applicative instance
  implicit def appI[F[_],A]: Applicative[SF[F,A,?]] =
    new Applicative[SF[F,A,?]] {
      def pure[B](b: B) = const(b)
      def ap[B,C](fa: SF[F,A,B ⇒ C])(f: SF[F,A,B]) = f ap fa
      override def map[B,C](fb: SF[F,A,B])(f: B ⇒ C) = fb map f
    }


  // Arrow instance
  implicit def arrI[F[_]]: Arrow[SF[F,?,?]] =
    new Arrow[SF[F,?,?]] {
      type SF[A,B] = msf.SF[F,A,B]

      override def id[A] = SF.id[F,A]

      def compose[A, B, C](f: SF[B, C], g: SF[A, B]): SF[A, C] =
        f compose g

      def lift[A, B](f: A => B) = arr(f)

      override def dimap[A, B, C, D](fab: SF[A, B])(f: C => A)(g: B => D): SF[C, D] =
        fab.dimap(f)(g)

      override def first[A, B, C](fa: SF[A, B]): SF[(A, C), (B, C)] =
        fa.first[C]

      override def second[A, B, C](fa: SF[A, B]): SF[(C, A), (C, B)] =
        fa.second[C]

      override def split[A, B, C, D](f: SF[A, B], g: SF[C, D])
        : SF[(A, C), (B, D)] = f split g
    }


  // ---------------------------------------------------------------
  //                *** Syntactic Enrichements ***
  // ---------------------------------------------------------------

  implicit class LoopSyntax[F[_],A,B,C](val sf: SF[F,(A,C),(B,C)]) extends AnyVal {
    def loop(c: C): SF[F,A,B] = SF.loop(sf)(c)
      
    def loopF(c: F[C])(implicit F: Functor[F]): SF[F,A,B] = SF.loopF(sf)(c)
  }

  object syntax {
    implicit class FunSyntax[A,B](val f: A ⇒ B) extends AnyVal {
      def msf[F[_]]: SF[F,A,B] = SF.arr(f)
    }
  }
}

/**
  * Provides an environment together with utility functions for
  * a given effect [[F]].
  */
trait SFHelper[F[_]] {
  type SF[A,B] = msf.SF[F,A,B]

  def liftS[A,B](f: A ⇒ F[B]): SF[A,B] = SF liftS f

  def idS[A]: SF[A,A] = SF.id

  def arr[A,B](f: A ⇒ B): SF[A,B] = SF arr f

  def swap[A,B]: SF[(A,B),(B,A)] = SF.swap
    
  def loop[A,B,C](sf: SF[(A,C),(B,C)])(c: C): SF[A,B] = SF.loop(sf)(c)
    
  def loopP[A,B](sf: SF[(A,B),B])(b: B): SF[A,B] = SF.loopP(sf)(b)

  def loop_[A,B](sf: SF[(A,B),B])(b: B): SF[A,Unit] = SF.loop_(sf)(b)

  def loopF[A,B,C](sf: SF[(A,C),(B,C)])(c: F[C])(implicit F: Functor[F]): SF[A,B] =
    SF.loopF(sf)(c)

  def loopPF[A,B](sf: SF[(A,B),B])(b: F[B])(implicit F: Functor[F]): SF[A,B] = SF.loopPF(sf)(b)

  def loopF_[A,B](sf: SF[(A,B),B])(b: F[B])(implicit F: Functor[F]): SF[A,Unit] = SF.loopF_(sf)(b)

  def const[A,B](b: B): SF[A,B] = SF const b

  def const_[B](b: B): SF[Unit,B] = const(b)

  def constS[A,B](b: F[B]): SF[A,B] = liftS(_ ⇒ b)

  def constS_[B](b: F[B]): SF[Unit,B] = constS(b)

  def joinF[A,B](sf: F[SF[A,B]]): SF[A,B] = JoinF(sf)
 
  def dynlist[A,B](
    sf: SF[A,List[(SF[A,B],EF[F,A,Unit])]],
    sfs: List[(SF[A,B],EF[F,A,Unit])],
  ): SF[A,List[B]] = DynList(sf, sfs)
}




// ---------------------------------------------------------------
//             *** Primitives: Implementation  ***
// ---------------------------------------------------------------


private final case class Primitive[F[_],A,B](
  f: A ⇒ F[(B,SF[F,A,B])]
) extends SF[F,A,B] {
  def step(a: A)(implicit M: Monad[F]): Res = f(a)

  def translate[G[_]:Functor](t: F ~> G): SF[G,A,B] =
    Primitive(a ⇒ t(f(a)) map { case (b,sf) ⇒ b -> sf.translate(t) })
}

private final case class Const[F[_],A,B](b: B) extends SF[F,A,B] {
  def step(a: A)(implicit M: Monad[F]): Res = M.pure(b -> this)

  def translate[G[_]:Functor](t: F ~> G): SF[G,A,B] =
    Const[G,A,B](b)
}


private final case class ConstF[F[_],A,B](b: F[B]) extends SF[F,A,B] {
  def step(a: A)(implicit M: Monad[F]): Res = b map (_ -> this)

  def translate[G[_]:Functor](t: F ~> G): SF[G,A,B] =
    ConstF[G,A,B](t(b))
}


private final case class Pure[F[_],A,B](f: A ⇒ B) extends SF[F,A,B] {
  def step(a: A)(implicit M: Monad[F]): Res = M.pure(f(a) -> this)

  def translate[G[_]:Functor](t: F ~> G): SF[G,A,B] =
    Pure[G,A,B](f)
}


private final case class Lifted[F[_],A,B](f: A ⇒ F[B])
   extends SF[F,A,B] {
  def step(a: A)(implicit M: Monad[F]): Res = f(a) map (_ -> this)

  def translate[G[_]:Functor](t: F ~> G): SF[G,A,B] =
    Lifted[G,A,B](a ⇒ t(f(a)))
}


private final case class First[F[_],A,B,C](sf: SF[F,A,B])
  extends SF[F,(A,C),(B,C)] {
  def step(p: (A,C))(implicit M: Monad[F]): Res = p match {
    case (a,c) ⇒ sf.step(a) map { case (b,msf) ⇒ (b -> c) -> msf.first[C] }
  }

  def translate[G[_]:Functor](t: F ~> G): SF[G,(A,C),(B,C)] =
    First[G,A,B,C](sf translate t)
}


private final case class Second[F[_],A,B,C](sf: SF[F,A,B])
  extends SF[F,(C,A),(C,B)] {
  def step(p: (C,A))(implicit M: Monad[F]): Res = p match {
    case (c,a) ⇒ sf.step(a) map { case (b,msf) ⇒ (c -> b) -> msf.second[C] }
  }

  def translate[G[_]:Functor](t: F ~> G): SF[G,(C,A),(C,B)] =
    Second[G,A,B,C](sf translate t)
}


private final case class LeftSF[F[_],A,B,C](sf: SF[F,A,B])
  extends SF[F,Either[A,C],Either[B,C]] {
  def step(e: Either[A,C])(implicit M: Monad[F]): Res = e.fold(
    a ⇒ sf.step(a).map{ case (b,newSf) ⇒ Left(b) -> newSf.left[C] },
    c ⇒ M.pure(Right(c) -> this)
  )

  def translate[G[_]:Functor](t: F ~> G): SF[G,Either[A,C],Either[B,C]] =
    LeftSF[G,A,B,C](sf translate t)
}


private final case class RightSF[F[_],A,B,C](sf: SF[F,A,B])
  extends SF[F,Either[C,A],Either[C,B]] {
  def step(e: Either[C,A])(implicit M: Monad[F]): Res = e.fold(
    c ⇒ M.pure(Left(c) -> this),
    a ⇒ sf.step(a).map{ case (b,newSf) ⇒ Right(b) -> newSf.right[C] }
  )

  def translate[G[_]:Functor](t: F ~> G): SF[G,Either[C,A],Either[C,B]] =
    RightSF[G,A,B,C](sf translate t)
}


private final case class Compose[F[_],A,B,C](
  fst: SF[F,A,B],
  snd: SF[F,C,A]
) extends SF[F,C,B] {
  def step(c: C)(implicit M: Monad[F]): F[(B, SF[F,C,B])] = for {
    p1 <- snd step c // p1 : (A, SF[F,C,A])
    p2 <- fst step p1._1 // p2 : (B, SF[F,A,B])
  } yield p2._1 -> Compose(p2._2, p1._2)

  def translate[G[_]:Functor](t: F ~> G): SF[G,C,B] =
    Compose[G,A,B,C](fst translate t, snd translate t)
}


private final case class Loop2[F[_],A,B,C](
  sf: SF[F,(A,C),(B,C)],
  ini: Option[C]
) extends SF[F,(A,C),B] {
  def step(p: (A,C))(implicit M: Monad[F]): Res =
    sf.step(ini.fold(p)(p._1 -> _))
      .map { case ((b,c),newSf) ⇒ b -> Loop2(newSf, Some(c)) }

  def translate[G[_]:Functor](t: F ~> G): SF[G,(A,C),B] =
    Loop2[G,A,B,C](sf translate t, ini)
}

private final case class Loop2F[F[_],A,B,C](
  sf: SF[F,(A,C),(B,C)],
  ini: F[C]
) extends SF[F,A,B] {
  def step(a: A)(implicit M: Monad[F]): Res =
    ini.flatMap(c ⇒ sf step (a -> c))
       .map { case ((b,c),newSf) ⇒ b -> Loop2F(newSf, M pure c) }

  def translate[G[_]:Functor](t: F ~> G): SF[G,A,B] =
    Loop2F[G,A,B,C](sf translate t, t(ini))
}

private final case class Switch[F[_],A,B](
  sf: SF[F,A,B],
  sfo: SF[F,A,Option[SF[F,A,B]]]
) extends SF[F,A,B] {
  def step(a: A)(implicit M: Monad[F]): Res = for {
    p           <- sfo step a  // p: (Option[SF[F,A,B]],SF[F,A,Option[SF[F,A,B]]])
    p2          <- p._1 getOrElse sf step a // p2: (B,SF[F,A,B])
  } yield p2._1 -> Switch(p2._2, p._2)

  def translate[G[_]:Functor](t: F ~> G): SF[G,A,B] = Switch[G,A,B](
    sf translate t,
    sfo translate t map (_ map (_ translate t))
  )
}

private final case class DynList[F[_],A,B](
  sf: SF[F,A,List[(SF[F,A,B],EF[F,A,Unit])]],
  sfs: List[(SF[F,A,B],EF[F,A,Unit])]
) extends SF[F,A,List[B]] {
  def step(a: A)(implicit M: Monad[F]): Res = for {
    p1              <- sf step a
    (newSfs, newSf) = p1
    efRes           <- (sfs ::: newSfs) traverse {
                         case (s,e) ⇒ e.sf step a map {
                           case (o,ne) ⇒ o -> (s -> EF(ne))
                         }
                       }

    sfRes           <- efRes.collect{case (None,p) ⇒ p}
                            .traverse {
                              case (s,e) ⇒ s step a map {
                                case (b,ns) ⇒ b -> (ns -> e)
                              }
                            }
  } yield sfRes.map(_._1) -> DynList(newSf, sfRes.map(_._2))


  def translate[G[_]:Functor](t: F ~> G): SF[G,A,List[B]] = {
    def tr(ps: List[(SF[F,A,B],EF[F,A,Unit])]) = ps map {
      case (s,e) ⇒ s.translate(t) -> e.translate(t)
    }

    DynList[G,A,B](sf translate t map tr, tr(sfs))
  }
    
}

private final case class JoinF[F[_],A,B](mk: F[SF[F,A,B]]) extends SF[F,A,B] {
  def step(a: A)(implicit M: Monad[F]): Res = mk >>= (_ step a)

  def translate[G[_]:Functor](t: F ~> G): SF[G,A,B] =
    JoinF[G,A,B](t(mk) map (_ translate t))
}

// vim: set ts=2 sw=2 et:

