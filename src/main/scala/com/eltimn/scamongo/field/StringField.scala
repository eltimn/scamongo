package com.eltimn.scamongo.field

/*
 * Copyright 2009 Tim Nelson
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */

import scala.xml._
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http.{S}
import net.liftweb.http.js._
import _root_.java.util.regex._
import S._
import Helpers._
import JE._

import net.liftweb.record.{Field, Record}

/**
 * A Field containing String content.
 */
class StringField[OwnerType <: Record[OwnerType]](rec: OwnerType, maxLength: Int) extends Field[String, OwnerType] {

  def this(rec: OwnerType, maxLength: Int, value: String) = {
    this(rec, maxLength)
    set(value)
  }

  def this(rec: OwnerType, value: String) = {
    this(rec, 100)
    set(value)
  }

  def owner = rec

  def setFromAny(in: Any): Box[String] = {
    in match {
      case seq: Seq[_] if !seq.isEmpty => seq.map(setFromAny)(0)
      case (s: String) :: _ => Full(set(s))
      case null => Full(set(null))
      case s: String => Full(set(s))
      case Some(s: String) => Full(set(s))
      case Full(s: String) => Full(set(s))
      case None | Empty | Failure(_, _, _) => Full(set(null))
      case o => Full(this.set(o.toString))
    }
  }

  def setFromString(s: String): Box[String] = Full(set(s))

  private def elem = S.fmapFunc(SFuncHolder(this.setFromAny(_))) {
    funcName =>
    <input type="text" maxlength={maxLength.toString}
      name={funcName}
      value={value match {case null => "" case s => s.toString}}
      tabindex={tabIndex toString}/>
  }

  def toForm = {
    uniqueFieldId match {
      case Full(id) =>
         <div id={id+"_holder"}><div><label for={id+"_field"}>{displayName}</label></div>{elem % ("id" -> (id+"_field"))}<lift:msg id={id}/></div>
      case _ => <div>{elem}</div>
    }

  }

  def asXHtml: NodeSeq = {
    var el = elem

    uniqueFieldId match {
      case Full(id) =>  el % ("id" -> (id+"_field"))
      case _ => el
    }
  }


  def defaultValue = ""
  
  /**
   * A validation helper.  Make sure the string is at least a particular
   * length and generate a validation issue if not
   */
  def valMinLen(len: Int, msg: => String)(value: String): Box[Node] =
    if ((value eq null) || value.length < len) Full(Text(msg))
    else Empty

  /**
   * A validation helper.  Make sure the string is no more than a particular
   * length and generate a validation issue if not
   */
  def valMaxLen(len: Int, msg: => String)(value: String): Box[Node] =
    if ((value ne null) && value.length > len) Full(Text(msg))
    else Empty
  
  // use maxLength
  def valMaxLen(msg: => String)(value: String): Box[Node] = valMaxLen(maxLength, msg)(value)

  /**
   * Make sure the field matches a regular expression
   */
  def valRegex(pat: Pattern, msg: => String)(value: String): Box[Node] = pat.matcher(value).matches match {
    case true => Empty
    case false => Full(Text(msg))
  }

  final def toUpper(in: String): String = in match {
    case null => null
    case s => s.toUpperCase
  }

  final def trim(in: String): String = in match {
    case null => null
    case s => s.trim
  }

  final def notNull(in: String): String = in match {
    case null => ""
    case s => s
  }

  def asJs = Str(value)

}
