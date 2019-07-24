/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

/**
  * This package provides data types and utility functions used
  * in the conditional color formatting of table columns in the UI.
  */
package object format {
  def strDesc(name: String): ColumnDesc = StrDesc(name, NoFormat, true)

  def dblDesc(name: String): ColumnDesc = StrDesc(name, GradientFormat, true)
}
