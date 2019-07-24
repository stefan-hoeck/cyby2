/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package msf
package js

trait Attributes[ID,Cls] {
  type Attribute = (String,String)

  def idToString(i: ID): String

  def clsToString(c: Cls): String

  trait Mod[-V] { self ⇒ 
    def := (v: V): (String,String)

    def cmap[W](f: W ⇒ V): Mod[W] = new Mod[W] {
      def := (w: W) = self.:=(f(w))
    }
  }

  def mod[V](s: String, f: V ⇒ String): Mod[V] =
    new Mod[V]{ def := (v: V) = s -> f(v) }

  def str(n: String): Mod[String] = new Mod[String]{
    def := (v: String) = n -> v
  }

  def bool(n: String): Mod[Boolean] = new Mod[Boolean]{
    def := (b: Boolean) = if (b) n -> n else n -> ""
  }

  def data(n: String): Mod[String] = str(s"data-$n")

  def dataT[V](n: String, f: V ⇒ String): Mod[V] = mod(s"data-$n", f)

  def dataBool(n: String): Mod[Boolean] = bool(s"data-$n")


  val accept: Mod[String]       = str("accept")
  val accessKey: Mod[String]    = str("accesskey")
  val alt: Mod[String]          = str("alt")
  val autofocus: Mod[Boolean]   = bool("autofocus")
  val checked: Mod[Boolean]     = bool("checked")
  val cls: Mod[Cls]             = mod("class", clsToString)
  val disabled: Mod[Boolean]    = bool("disabled")
  val forId: Mod[ID]            = mod("for", idToString)
  val href: Mod[String]         = str("href")
  val list: Mod[ID]             = mod("list", idToString)
  val id: Mod[ID]               = mod("id", idToString)
  val name: Mod[String]         = str("name")
  val multiple: Mod[Boolean]    = bool("multiple")
  val optlabel: Mod[String]     = str("label")
  val placeholder: Mod[String]  = str("placeholder")
  val readOnly: Mod[Boolean]    = bool("readonly")
  val required: Mod[Boolean]    = bool("required")
  val selected: Mod[Boolean]    = bool("selected")
  val style: Mod[String]        = str("style")
  val src: Mod[String]          = str("src")
  val target: Mod[String]       = str("target")
  val title: Mod[String]        = str("title")
  val typ: Mod[InputType]       = mod("type", _.toString)
  val value: Mod[String]        = str("value")
  val cwidth: Mod[Int]          = mod("width", _.toString)
  val cheight: Mod[Int]         = mod("height", _.toString)
}

sealed abstract class InputType(override val toString: String)

object InputType {
  case object Button extends InputType("button")
  case object CheckBox extends InputType("checkbox")
  case object Color extends InputType("color")
  case object Date extends InputType("date")
  case object DateTime extends InputType("datetime-local")
  case object Email extends InputType("email")
  case object File extends InputType("file")
  case object Image extends InputType("image")
  case object Month extends InputType("month")
  case object Number extends InputType("number")
  case object Password extends InputType("password")
  case object Radio extends InputType("radio")
  case object Range extends InputType("range")
  case object Tel extends InputType("tel")
  case object Text extends InputType("text")
  case object Time extends InputType("time")
  case object URL extends InputType("url")
  case object Week extends InputType("week")
}

// vim: set ts=2 sw=2 et:
