/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package ui
package display

import cats.implicits._

import msf.js.{UIEvent, Node}

trait Disp extends CoreEnv {

  /**
    * Provides the signal functions necessary to adjust
    * the UI to the EditMode and to display results returned
    * from the server
    */
  trait Display extends CyByH[DispEnv,Unit,Option[UIEvent]] {
    self ⇒ 
    import Src.click, UId.{Item ⇒ _, _}

    override def evToUIEvent(e: Ev) = e

    /**
      * HTML string used to display the environment
      * linked to this part of the UI
      */
    def htmlIni(e: DispEnv): Node

    /**
      * Sink for results (probably redisplaying parts of the UI,
      * depending on the last result sent from the server)
      */
    def dispRes: Sink[Result]

    /**
      * Displays part or all of the accumulated UI state
      */
    def dispSt: Eff[Unit]

    /**
      * Signal function used to display results from the server
      * and adjust the UI depending on the edit mode.
      */
    def run: LogSF[DisplayIn,Unit] = {
      val auth: Src[Creds] = env.map(_.creds).collect{ case cs@Creds(_,_,_,HasLoggedIn) ⇒ cs }

      val res: Src[Result] = env.map(_.res).collect { case Some(r) ⇒ r }

      val behavior: Sink[Ev] =
        auth.asF(askE map htmlIni)          >>- sink.innerHtml(Content)            |+|
        click(InfoBtn)                      >>- (hide(NavView) |+| show(InfoView)) |+|
        click(NavBtn)                       >>- (show(NavView) |+| hide(InfoView)) |+|
        res                                 >>- dispRes                            |+|
        env.map(_.cmd).filter(_.hasChanged) >>- constS(dispSt)

      unrss(behavior)(unit)
    }

  }
}

// vim: set ts=2 sw=2 et:
