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

import net.liftweb.common.{Box, Empty, Full}
import net.liftweb.record.{Field, Record}
import net.liftweb.record.field.{StringField => LStringField}

/**
 * A Field containing String content. This is identical to the lift version, 
 * it just adds a couple of validation methods
 */
 
class StringField[OwnerType <: Record[OwnerType]](rec: OwnerType, maxLength: Int) extends LStringField[OwnerType](rec, maxLength) {

	/**
   * A validation helper.  Make sure the string is at least a particular
   * length and generate a validation issue if not
   */
  def valMinLen(len: Int, msg: => String)(valBox: Box[String]): Box[Node] =
  	valBox.flatMap {
  		s =>
  			if ((s eq null) || s.length < len) Full(Text(msg))
    		else Empty
  	}

  /**
   * A validation helper.  Make sure the string is no more than a particular
   * length and generate a validation issue if not
   */
  def valMaxLen(len: Int, msg: => String)(valBox: Box[String]): Box[Node] =
  	valBox.flatMap {
  		s =>
				if ((value ne null) && value.length > len) Full(Text(msg))
				else Empty
		}
  
  // use maxLength
  def valMaxLen(msg: => String)(valBox: Box[String]): Box[Node] = valMaxLen(maxLength, msg)(valBox)
}
