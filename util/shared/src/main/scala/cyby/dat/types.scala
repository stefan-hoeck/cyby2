/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

trait types {
  type Link[A] = (A, Name)

  type Links[A] = List[Link[A]]

  type LinksNel[A] = Nel[Link[A]]

  type FormulaEntry = (Int,String,Int)

  type Formula = List[FormulaEntry]
}

// vim: set ts=2 sw=2 et:
