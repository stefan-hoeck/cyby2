/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui
 
/**
  * Sum type representing errors only occuring in the client
  */
sealed trait Err { def e: Err = this }

case class Serious(t: Throwable) extends Err
 
case class Timeout(url: String) extends Err
 
case class LoadErr(url: String) extends Err
 
// vim: set ts=2 sw=2 et:
