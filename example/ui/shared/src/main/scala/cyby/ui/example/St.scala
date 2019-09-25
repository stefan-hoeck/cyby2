/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui
package example

import cyby.dat.example._

case class St(
  pros:  List[Project.Cli],
  stos:  List[Location.Cli],
  sups:  List[Supplier.Cli],
  mets:  List[Method.Cli],
  uses:  List[User.Cli],
  subs:  List[Compound.Cli],
  bio:   List[BioStats],
)

object St {
  def ini: St = St(Nil, Nil, Nil, Nil, Nil, Nil, Nil)

  def subsChanged(or: Option[Result]) = or match {
    case Some(CpdRes(r)) ⇒ some(r)
    case _               ⇒ None
  }
}

// vim: set ts=2 sw=2 et:
