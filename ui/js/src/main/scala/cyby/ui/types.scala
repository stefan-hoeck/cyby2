/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui

import msf.js.{HttpEvent, UIEvent}

/**
  * Often used type aliases
  */
trait types extends cyby.ui.sharedTypes {
  /**
    * Event type from the UI causing the main signal function
    * to be reevaluated.
    */
  type In = Either[HttpEvent,UIEvent]

  type JSDate = scala.scalajs.js.Date
}

// vim: set ts=2 sw=2 et:
