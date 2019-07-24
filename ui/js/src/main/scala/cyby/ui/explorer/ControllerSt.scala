/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui
package explorer

import select.Model

/**
  * State of the explorer controller. This is mainly used to keep
  * track of selected compounds and number of compounds recieved
  * from the server, displayed in the UI, and still available at the server.
  */
case class ControllerSt[I](
  readyToLoad: Boolean,
  loaded:      Int,
  displayed:   Int,
  total:       Int,
  select:      Model[I],
)

// vim: set ts=2 sw=2 et:
