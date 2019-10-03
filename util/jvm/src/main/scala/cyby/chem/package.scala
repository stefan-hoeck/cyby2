/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby

import cats.implicits._

import scala.util.control.NonFatal

/**
  * Types and utility functions for interacting with the algorithms
  * from the CDK.
  */
package object chem {
  type Atom = org.openscience.cdk.interfaces.IAtom

  type Element = org.openscience.cdk.interfaces.IElement

  type Isotope = org.openscience.cdk.interfaces.IIsotope

  type Molecule = org.openscience.cdk.interfaces.IAtomContainer

  /**
    * This ugly hack is necessary due to
    * https://github.com/scala/bug/issues/10501
    */
  private[chem] def cloneMol(m: Molecule): Molecule = {
    val met = m.getClass
               .getDeclaredMethods
               .toList
               .filter(_.getName === "clone")
               .head

    met.invoke(m).asInstanceOf[Molecule]
  }

  def tryRes[A](a: ⇒ A, str: String): ErrNel[Throwable,A] = try {
    val a2 = a
    if (a2 == null) left(new NullPointerException)
    else right(a2)
  } catch { case e@NonFatal(_) ⇒ {
      left(e)
  } }

  def opt[A](a: ⇒ A): Option[A] = try {
    if (a == null) None else Some(a)
  } catch { case NonFatal(_) ⇒ None }
}

// vim: set ts=2 sw=2 et:


