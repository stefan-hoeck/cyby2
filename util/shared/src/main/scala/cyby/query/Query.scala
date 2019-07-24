/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package query

import io.circe.generic.JsonCodec

/**
  * Fully specified query object.
  *
  * This datatype wraps a combined query together with information
  * about which part of the hitset should be returned by the server
  * and how the returned items should be sorted.
  *
  * @tparam C: column type used to sort hitsets
  * @tparam F: field type used to specify which field(s) to query
  */
@JsonCodec case class Query[C,F](
  /** the actual combined query */
  query:   Q[F],

  /** type of query **/
  qtype:   QueryType,

  /** column used to sort the hitset */
  sort:    C,

  /** flag signalling whether the hitset should be sorted in reverse order */
  reverse: Boolean,

  /** index of the first item in the hitset to be returned by the server */
  start:   Int,

  /** number of items to be returned by the server */
  count:   Int,
)

object Query {
  implicit def eqI[C,F]: cats.Eq[Query[C,F]] = cats.Eq.fromUniversalEquals
}

/**
  * Enum type representing types of queries.
  */
@JsonCodec sealed trait QueryType

/**
  * Signals that substances should be returned as heterogeneous
  * data trees (as displayed in the default substance view).
  */
case object SubstanceQuery extends QueryType

/**
  * Signals that statistics about biological activities
  * should be extracted (as displayed in the statistics view).
  */
case object StatisticsQuery extends QueryType

object QueryType

// vim: set ts=2 sw=2 et:
