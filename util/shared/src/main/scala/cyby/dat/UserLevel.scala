/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

import cats.implicits._

/**
  * There are two orthogonal concepts in CyBy2 defining
  * what a user of our software is allowed to do and
  * which information a user can access: UserLevels
  * and Projects.
  *
  * While Projects define in a fine-grained manner, which
  * pieces of information from the underlying database a
  * user can access, UserLevels
  * are used as a rough measure to group users into one of
  * several cathegories. In order to easily support additional
  * cathegories at a later stage, we opted for an internal Integer
  * representation.
  *
  *   Guests (UserLevel >= 1): Guests can only query the information
  *   which they have been granted access to. They are not allowed
  *   to modify the content of the database in any way.
  *
  *   CommonUser (UserLevel >= 10): Common users are allowed to
  *   modify content of projects to which they have been
  *   granted access. Data modified by a CommonUser has to be
  *   reviewed by another qualified user otherwise it is marked
  *   as unvalidated.
  *
  *   Superuser (UserLevel >= 100): SuperUsers are allowed to
  *   create new Projects and define ownership for these Projects.
  *   Superusers still do not have access to data from a given Project
  *   unless they are listed as one of the project's users.
  *
  *   Administrators (UserLevel >= 1000): Administrators have access
  *   to the whole content of the database and are the only ones
  *   allowed to register new users and define a user's level.
  *   They are also allowed to modify all content of the database,
  *   and are the only ones that can delete stuff from the database.
  *
  */
final class UserLevel private (val v: Int) extends AnyVal {
  override def toString = v.toString
}

object UserLevel extends Refined[Int,UserLevel]("UserLevel", _.v){
   val Guest      : UserLevel  = new UserLevel(1)
   val CommonUser : UserLevel  = new UserLevel(10)
   val Superuser  : UserLevel  = new UserLevel(100)
   val Admin      : UserLevel  = new UserLevel(1000)

   val values: List[UserLevel] =
     List(Guest, CommonUser, Superuser, Admin)

   def apply(v: Int): Option[UserLevel] = 
     if (v >= 0) Some(new UserLevel(v)) else None
}


// vim: set ts=2 sw=2 et:
