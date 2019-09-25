/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui

/**
  * Field Types in UI. F is field type here, In linked
  * input type, Out: linked output type.
  *
  * For instance: F = CpdField, In = Compound.Cli, Out = type of field in Compound.Cli
  *
  * Queries:   F ⇒ St ⇒ WidgetDesc[Unit,String,String]
  * Update:    F ⇒ Option[In] ⇒ St ⇒ Html[(Elem,Signal[Option[Json]])]
  *
  *
  * Desc:      F ⇒ ColumnDesc
  * Loc:       F ⇒ Localization
  * CanExport: F ⇒ Boolean
  * CanQuery:  F ⇒ Boolean
  * InColumn:  F ⇒ Boolean
  *
  * 
  * If using fields also as enumerations (not a selectors):
  *
  * Display: F ⇒ (In, DispEnv) ⇒ String
  *
  *
  * Server:
  *
  * Export-Odf:   F ⇒ In ⇒ String
  * Export-Txt:   F ⇒ In ⇒ String
  * Export-Sdf:   F ⇒ In ⇒ String
  * Query:        F ⇒ St ⇒ RP[In]
  * Sort:         F ⇒ List[In] ⇒ List[In]
  */
package object example
