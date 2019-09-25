/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

import org.scalatest.Assertion

abstract class EditUtil
  extends ServerSuite
  with    ServerMatchers
  with    ArbGenerators {

  def extract[E,A](e: ErrNel[E,A]): A = e.toOption.get

  def extract[E,A](e: ValNel[E,A]): A = e.toOption.get

  def errs[E,A](e: ErrNel[E,A]): Nel[E] = e.left.toOption.get

  def errs[E,A](e: ValNel[E,A]): Nel[E] = errs(e.toEither)

  def testMod[A:cats.Eq](pn: Pure[A], po: Pure[A], o: Option[A]): Assertion =
    if (o.nonEmpty) pn.v shouldEq o.get
    else            pn.v shouldEq po.v

  def mkDB[I,A](a: A)(f: A ⇒ I): Map[I,A] = Map(f(a) -> a)

  def inDB[I,A,S](a: A, s: S)(f: A ⇒ I, l: Lens[S,S] ⇒ Lens[S,Map[I,A]]): S =
    l(lens[S]).set(s)(mkDB(a)(f))

  def proSt(p: ProS.Srv): St = inDB(p, St.empty)(_.id, _.pros)

  def subSt(s: SubS.Srv): St = inDB(s, St.empty)(_.id, _.subs)

  def conSt(c: ContainerS.Srv, s: SubS.Srv): St =
    subSt(inDB(c, s)(_.id, _.containers))

  def conFilSt(f: ConFilS.Srv, c: ContainerS.Srv, s: SubS.Srv): St =
    conSt(inDB(f, c)(_.id, _.files), s)

  def bioSt(b: BiodataEntryS.Srv, c: ContainerS.Srv, s: SubS.Srv): St =
    conSt(inDB(b, c)(_.id, _.bio), s)

  def stoSt(s: StoS.Srv): St = inDB(s, St.empty)(_.id, _.stos)

  def metSt(s: MetS.Srv): St = inDB(s, St.empty)(_.id, _.mets)
}

// vim: set ts=2 sw=2 et:
