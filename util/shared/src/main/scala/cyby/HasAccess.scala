/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby

/** 
  * Marker trait used to make sure that users have
  * be properly authorized before they get information
  * from the server.
  */
sealed trait HasAccess

/**
  * Marker trait used at the server during editing.
  */
sealed trait Adjusted

/**
  * Marker trait used at the server during editing.
  */
sealed trait IsValid

/**
  * Marker trait used at the server during editing.
  */
sealed trait Authorized
