/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package export

import cats.Eq
import cyby.query.Query

/**
  * Export settings to be sent from the client to the server
  * where a file containing the specified data will be created.
  *
  * @tparam C: column type used for sorting the data
  * @tparam F: field type used to specify exported fields
  */
@io.circe.generic.JsonCodec case class Settings[C,F](
  /** query used to generate the data set to be exported */
  query:  Query[C,F],

  /** list of fields to be exported */
  fields: List[F],

  /** file format to export to */
  format: Format
)

object Settings {
  implicit def eqI[C,F]: Eq[Settings[C,F]] = Eq.fromUniversalEquals
}
