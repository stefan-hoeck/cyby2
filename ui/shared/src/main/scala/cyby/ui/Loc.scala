/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui

trait LocEnv extends UIEnv {

  def loc: LocUtil

  /**
    * Localisation of text in the UI.
    */
  trait LocUtil extends cyby.dat.Loc {
    val loadCount: Int = 60

    def logResult(r: Result): Vector[Log]

    def logErr(e: Err): Log

    def editColumns: String

    def editFormat: String

    def navSearchTitle(m: NavSearchMode): String

    def plots: String
    def documentation: String
    def grid: String
    def table: String
    def stats: String
    def unknown: String
    def subTag: String
    def conTag: String
    def subPro: String
    def conPro: String
    def subFile: String
    def conFile: String
    def subCreate: String
    def subMod: String
    def conCreate: String
    def conMod: String
    def toggleEditing: String
    def deleteColumn: String
    def addColumn: String
    def addColumnTitle: String
    def editFormatting: String
    def negators(b: Boolean): String
    def formatHeader(c: Column, st: St): String
  }

  trait EnUSUtil extends LocUtil with cyby.dat.LocEnUS {
    def logErr(e: Err): Log = e match {
      case Timeout(url) ⇒ Log error s"The connection to the server at $url timed out. Please contact your CyBy admin."
      case LoadErr(url) ⇒ Log error s"An error occured when loading data from $url. Please contact your CyBy admin."
      case Serious(t)   ⇒ Log error s"A serious error occured in the user interface. This is a bug. Please inform your CyBy admin. The error message was: $t."
    }

    def plots =  "Plots"
    def documentation = "Documentation"
    def editColumns = "Set columns"
    def grid = "Grid View"
    def table = "Table View"
    def stats = "Statistics"
    def editFormat = "Conditional Formatting"
    def unknown = "Unknown"
    def subTag = "Tag (Substance)"
    def conTag = "Tag (Container)"
    def subPro = "Project (Substance)"
    def conPro = "Project (Container)"
    def subFile = "File Name (Substance)"
    def conFile = "File Name (Container)"
    def subCreate = "Created (Substance)"
    def subMod = "Last modified (Substance)"
    def conCreate = "Created (Container)"
    def conMod = "Last modified (Container)"
    def toggleEditing = "Enable Editing"
    def deleteColumn = "Remove column"
    def addColumn = "Add Column"
    def addColumnTitle = "Add column to the right"
    def editFormatting = "Conditional formatting"
    def negators(b: Boolean): String = if (b) "not" else ""
    def formatHeader(c: Column, st: St): String =
      s"Formatting Rules for ${columnLoc(st)(c)}"

    def navSearchTitle(m: NavSearchMode): String = m match {
      case NSBio         ⇒ "list biodata"
      case NSConAll      ⇒ "list all containers"
      case NSConNonEmpty ⇒ "list non-empty containers"
      case NSSub         ⇒ "list compounds"
    }
  }
}

// vim: set ts=2 sw=2 et:
