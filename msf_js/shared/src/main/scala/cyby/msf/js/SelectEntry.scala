/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package msf
package js

case class SelectEntry(
  value:    Option[String],
  selected: Boolean,
  text:     String
)

// vim: set ts=2 sw=2 et:
