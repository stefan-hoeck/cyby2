/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package query

/**
  * Quick search information as entered by users in the UI.
  */
@io.circe.generic.JsonCodec case class QuickSearch(
  includeEmpty:  Boolean,
  caseSensitive: Boolean,
  query:         String,
)

object QuickSearch
