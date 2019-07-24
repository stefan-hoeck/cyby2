/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui
package example

import cats.implicits._
import cyby.dat.example._

import UId.{Item ⇒ _, _}

trait DispZ extends cyby.ui.display.Disp with CoreZ with DispZShared {
  outer ⇒

  object Disp extends Display {
    def dispRes: Sink[Result] = liftS {
      case ProRes(_) ⇒ dispPros
      case StoRes(_) ⇒ dispStos
      case SupRes(_) ⇒ dispSups
      case MetRes(_) ⇒ dispMets
      case UseRes(_) ⇒ dispUses
      case _         ⇒ Eff.unit
    }

    lazy val dispPros = disp(ProT, _.pros)(pro)
    lazy val dispStos = disp(StoT, _.stos)(sto)
    lazy val dispSups = disp(SupT, _.sups)(sup)
    lazy val dispMets = disp(MetT, _.mets)(met)
    lazy val dispUses = disp(UseT, _.uses)(use)

    def dispSt: Eff[Unit] = (askE >>= {de ⇒
      at(NavView)(set innerHtml navSections(de)) *>
      at(ExplorerId)(set innerHtml Txt.explorer(de.mode)("","","",""))
    }) *> dispPros *> dispStos *> dispSups *> dispMets *> dispUses

    def disp[A](dt: DataType, as: St ⇒ List[A])
      (html: DispEnv ⇒ A ⇒ String): Eff[Unit] =
      askE.flatMap { de ⇒
        at(DataList(dt,RootP))(set innerHtml as(de.st).map(html(de)).mkString(""))
      }

    def htmlIni(d: DispEnv): String = outer htmlIni d
  }
}

// vim: set ts=2 sw=2 et:
