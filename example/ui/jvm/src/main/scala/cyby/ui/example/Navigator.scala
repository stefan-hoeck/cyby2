/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui
package example

import msf.js.{raw}

import cyby.ui.{DocType ⇒ DT}

object Navigator extends util {
  import Txt._

  def doc = section(id := UId.NavigatorDoc, cls := DT.Section.c)(
    h1(cls := DT.H1.c)(raw("Navigator: Organizing Data")),
    div(cls := DT.SectionContent.c)(
      div(cls := DT.Paragraph.c)(
        raw(s"""
        In the navigator, cathegories of peripheral data types like
        projects, suppliers, and tags are being managed, to which
        compounds and containers can be linked. In addition,
        projects are used to restrict access to confidential
        information stored in the data base. Users have only access
        to a subset of all projects and cannot look up information
        linked to other projects.
        """)
      ),
      example(navigator(dispEnv.creds, Nil, navSections(dispEnv))),
      div(cls := DT.Paragraph.c)(
        raw(s"""
        This part of the documentation not only describes the different
        data types in the navigator, but also gives some insight
        into the business rules and inner workings of the ${cyby}
        server.
        """)
      ),
      h2(id := UId.UsersDoc, cls := DT.H2.c)(raw("Users and their Roles")),
      div(cls := DT.Paragraph.c)(
        raw(s"""
        The list of users can be inspected by expanding the
        corresponding node. Most users (with the exception of
        Superusers and Administrators; see below), will only see
        a single entry. When expanding a user's node, more
        details will show up including a button for changing
        a user's password.
        """)
      ),
      example(navigator(dispEnv.creds, Nil, navSections(dispEnvUse))),
      div(cls := DT.Paragraph.c)(
        raw(s"""
        Users in ${cyby} are assigned one of four possible roles:
        """)
      ),
      div(cls := DT.DescList.c)(
        div(cls := DT.DescRow.c)(
          div(cls := DT.DescName.c)(raw("Guest")),
          div(cls := DT.DescContent.c)(
            raw(s"""
            Guest accounts are useful for allowing anonymous access to
            a limited subset of the data. Guests are not allowed
            to change the content of the database in any way. They
            are not even allowed to change their own passwords.
            """)
          ),
        ),
        div(cls := DT.DescRow.c)(
          div(cls := DT.DescName.c)(raw("User")),
          div(cls := DT.DescContent.c)(
            raw(s"""
            This is the role assigned to most personalized accounts.
            Users are allowed to view all data linked to projects
            to which they are granted access. They can change most
            content of the database with the following exceptions:
            They are only allowed to add compounds, containers, biodata, or
            files when they have access to all projects linked to the
            data object in question. They are not allowed to
            add new users or projects and can only modify parts of existing
            projects if they are registered as a project's reviewer.
            Additionally, they are not allowed to modify existing
            users with the exception of changing their own
            password, first name, last name, and email address.
            """)
          ),
        ),
        div(cls := DT.DescRow.c)(
          div(cls := DT.DescName.c)(raw("Superuser")),
          div(cls := DT.DescContent.c)(
            raw(s"""
            In addition to the rights granted to common users, Superusers
            are allowed to create new projects and can be set as
            a project's owner. They can view all user accounts but
            only make minor changes (like common users) to their own
            account. Only when registered as a project's owner can
            a Superuser grant another user access to that project.
            """)
          ),
        ),
        div(cls := DT.DescRow.c)(
          div(cls := DT.DescName.c)(raw("Administrator")),
          div(cls := DT.DescContent.c)(
            raw(s"""
            Administrators have unlimited access to all content
            stored in the database, independent to whether they
            have been granted access to certain projects or not.
            They are the only users allowed to create new
            users and modify all fields of existing accounts with
            the following limitations: Even Administrators cannot
            change their own alias nor their user role, nor can
            they delete their own account as all of these actions
            could lead to an inconsistent state in the client.
            However, if an Administrator needs to make changes as
            described above, she can always create an additional
            Administrator account and carry out those changes from
            there.
            Finally, Administrators are the only users allowed to
            delete objects from the database. Deleting data is
            a delicate task and should be avoided whenever possible.
            """)
          ),
        ),
      ),
      h2(id := UId.ProjectsDoc, cls := DT.H2.c)(raw("Projects")),
      div(cls := DT.Paragraph.c)(
        raw(s"""
        Projects are at the core of ${cyby}'s access restriction
        rules. Every compound, container, biodata entry, or file link
        in ${cyby} is linked to at least one project.
        With the exception of Administrators
        (see ${link(UId.UsersDoc, "users")}),
        users only have access to a subset of projects and they
        can only access data objects linked to projects to which
        they have been granted access.
        """)
      ),
      div(cls := DT.Paragraph.c)(
        raw(s"""
        New projects can only be created by Superusers. They are then
        assigned a project owner, who is responsible from granting other
        people access to the project. In addition, several people
        can be assigned the role of "project reviewer". They have special
        permissions when it comes to modifying a project definition
        or changing the list of projects to which existing data objects
        are linked.
        """)
      ),
      example(navigator(dispEnv.creds, Nil, navSections(dispEnvPro))),

      h2(id := UId.TagSupDoc, cls := DT.H2.c)(
        raw("Suppliers, Tags, and Storage Locations")
      ),
      div(cls := DT.Paragraph.c)(
        raw(s"""
        These data types are mostly self-explanatory. They can be
        created and modified by all users if necessary. Each container
        is linked to a supplier plus a storage location. Biodata entries
        are also each linked to a supplier. Tags on the other hand can
        be used as markers for compounds, containers, biodata entries, or files.
        """)
      ),
      example(navigator(dispEnv.creds, Nil, navSections(dispEnvTagSup))),
    )
  )
}

