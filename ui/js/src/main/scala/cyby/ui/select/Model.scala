/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui
package select

import cats.Eq, cats.implicits._

/**
  * State of selected items
  */
case class Model[A](
  /** set of selected values */
  selected: Set[A],

  /** 
    *  last selected value. this is used when selecting
    *  ranges of values
    */
  last:     Option[A],

  /** 
    *  backup of selected items when selecting a range
    *  of items. Use case: A user starts selecting a range
    *  by holding down SHIFT. So far, items 3 and 4 have
    *  been selected (stored also in lastSel).
    *  She clicks on item 8 (this goes into last, lastSel is
    *  unaffected) and then on 20 (last and lastSel are unaffected).
    *  Now items 3,4 and 8-20 are selected. If she's still holding down
    *  SHIFT and clicks now item 13, this is interpreted as undoing
    *  the former range selection and selecting a new range starting
    *  from 8. Now 3,4 and 8-13 are selected
    *  (last and lastSel are still unaffected). 3,4 came from lastSel, 8 from
    *  last and 9,10,11,12,13 from evaluating the selected range.
    */
  lastSel:  Set[A],

  /** 
    *  items recently added to the set of selected items 
    *  this will lead to
    */
  add:      List[A],

  /** items recently removed from the set of selected items */
  del:      List[A],
)

object Model {
  def empty[A]: Model[A] = Model[A](Set(), None, Set(), Nil, Nil)

  /**
    * Interprets a selection event, updating the given selection Model
    *
    *
    * @tparam A : type of selectable values
    * @tparam B : type (with an instance of cats.Eq) by which selectable
    *             values can be distinguished (for instance an ID)
    * @param vs : list of all values out of which parts have been selected
    * @param m  : actual selection model
    * @param get : returns identifier from selectable value
    * @param e   : latest selection event
    */
  def select[A,B:Eq](vs: List[A], m: Model[B])(get: A ⇒ B)(e: Event[B]): Model[B] = {
    // returns all values in the between the occurence
    // of b1 and b2 (order does not matter)
    def range(b1: B)(b2: B): List[B] = {
      val ib1 = vs.indexWhere(a ⇒ get(a) === b1)
      val ib2 = vs.indexWhere(a ⇒ get(a) === b2)
      val i1 = ib1 min ib2
      val i2 = ib1 max ib2

      if (i1 < 0) Nil
      else vs drop i1 take (i2 - i1 + 1) map get
    }

    e match {
      case Clear                     ⇒
        Model[B](Set(), None, Set(), Nil, m.selected.toList)
      case Single(b)                 ⇒
        Model[B](Set(b), some(b), Set(), List(b), m.selected.toList)
      case Toggle(b) if m.selected(b)⇒
        Model[B](m.selected - b, None, Set(), Nil, List(b))
      case Toggle(b)                 ⇒
        Model[B](m.selected + b, Some(b), m.selected, List(b), Nil)
      case Range(b)                  ⇒ m.last match {
        case None                    ⇒ select(vs, m)(get)(Toggle(b))
        case Some(b1)                ⇒ {
          val newSel = m.lastSel ++ range(b)(b1)
          Model[B](newSel, Some(b1), m.lastSel,
            newSel -- m.selected toList,
            m.selected -- newSel toList
          )
        }
      }
    }
  }
}
