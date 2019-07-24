/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package query

import io.circe.generic.JsonCodec

@JsonCodec case class QueryErr[L](lbl: L, query: String)

object QueryErr
