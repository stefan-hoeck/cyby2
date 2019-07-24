/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui

trait util extends cyby.ui.sharedUtil {
  def mkDate(n: Long): JSDate = new JSDate(n)

  def localeDateString(d: JSDate): String = d.toLocaleDateString

  def localeTimeString(d: JSDate): String = d.toLocaleTimeString
}

// vim: set ts=2 sw=2 et:
