/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui
package example

import cyby.ui.{DocType ⇒ DT}

object EditingDoc extends util {
  import Txt._

  def doc = section(id := UId.EditingDoc, cls := DT.Section.c)(
    h1(cls := DT.H1.c)("Editing Data"),
    div(cls := DT.SectionContent.c)(
      div(cls := DT.Paragraph.c)(
        s"""
        With the exception of ${link(UId.UsersDoc, "Guest")} accounts, all
        users in ${cyby} are allowed to modify the content of the database
        to a certain degree. Since modifying existing containers and
        compounds sometimes only consists of double clicking the right
        HTML element, all editing is disabled unless turned on by clicking
        the master editing button in the upper right corner of the
        explorer:
        """
      ),
      example(explorerDoc(NotEditing)("", "", "", "")),

      div(cls := DT.Paragraph.c)(
        s"""
        Turning editing on will result in some parts of the
        data being displayed and behaving differently than with
        editing turned off, so activate this mode only when needed.
        """
      ),
      h2(id := UId.CreateDoc, cls := DT.H2.c)("Adding Data"),
      div(cls := DT.Paragraph.c)(
        s"""
        Creating a new data objects always starts with clicking
        the blue plus icon next to the list of objects, to which
        the new item should be added. For adding new compounds,
        the corresponding icon appears in the explorer's title row,
        once editing has been enabled (see the example below).
        Some of these icons only appear, if the user has been assigned
        a certain ${link(UId.UsersDoc, "role")}:
        Only Superusers can create new projects and
        only Administrators can create new user accounts.
        """
      ),
      example(explorerDoc(Editing)("", "", "", "")),

      h2(id := UId.UpdateDoc, cls := DT.H2.c)("Updating Data"),
      div(cls := DT.Paragraph.c)(
        s"""
        Changing a field of an existing data object is in
        most cases started by double clicking the corresponding
        element when in editing mode. This will result
        in the appearance of a text field or selection box,
        where a new value can be entered. Pressing ${enter}
        sends the new value to the server, pressing Esc
        aborts editing the field in question.
        """
      ),
      div(cls := DT.Paragraph.c)(
        s"""
        There are two important exceptions to the general
        behavior described above: The structure of a compound
        can be changed by clicking the icon next to it when
        in editing mode (see example below), and containers
        can be set to empty
        by double clicking the full container icon.
        """
      ),
      example(
        explorerDoc(Editing)("", "", mkHeadRow(expEnvEdit),
          Explorer.dispSubs(Explorer.expEnvEdit))
      ),

      h2(id := UId.DeleteDoc, cls := DT.H2.c)("Deleting Data"),
      div(cls := DT.Paragraph.c)(
        s"""
        Deleting stuff from a database is a delicate task an in
        general strongly discouraged. Administrators will delete
        erroneous entries upon request. Just drop a quick email to
        hock@zhaw.ch.
        """
      ),

      h2(id := UId.ValidateDoc, cls := DT.H2.c)("Input Validation"),
      div(cls := DT.Paragraph.c)(
        s"""
        When editing fields or creating new data objects, most
        input is immediatly being validated. Text fields containing
        invalid input will be sourrounded by a red border to
        inform users that something is wrong. Some pieces of
        input can only be validated at the server, however. For
        instance, only at the server can we verify that a compound's
        structure or the name of a project are truly unique, and in order
        to keep the user interface responsive, this kind of
        validation only happens when editing is finished and the
        data is being sent to the server.
        If the server decides that the received data is invalid or
        that a user is not authorized to edit a certain piece of data,
        it will not accept the submitted changes and
        the user will be informed accordingly in the Log at the lower
        left of the screen. Always keep an eye out for warnings
        or error messages appearing in the Log when editing data.
        """
      ),
    )
  )
}

