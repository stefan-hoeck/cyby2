/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui

trait util extends cyby.ui.sharedUtil {
  def mkDate(n: Long): JSDate = n.toString

  def localeDateString(d: JSDate): String = d

  def localeTimeString(d: JSDate): String = d
}
