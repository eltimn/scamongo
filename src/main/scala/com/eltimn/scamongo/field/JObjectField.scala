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

import net.liftweb.common.{Box, Empty, Failure, Full}
import net.liftweb.http.js.JE.Str
import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.JsonParser
import net.liftweb.record.{Field, Record}

class JObjectField[OwnerType <: Record[OwnerType]](rec: OwnerType) extends Field[JObject, OwnerType] {

	def asJs = Str(toString)

	def asXHtml = <div></div>

	def defaultValue = JObject(List())
	
	def setFromAny(in: Any): Box[JObject] = in match {
  	case jv: JObject => Full(set(jv))
  	case Some(jv: JObject) => Full(set(jv))
    case Full(jv: JObject) => Full(set(jv))
    case seq: Seq[_] if !seq.isEmpty => seq.map(setFromAny)(0)
    case (s: String) :: _ => setFromString(s)
    case null => Full(set(null))
    case s: String => setFromString(s)
    case None | Empty | Failure(_, _, _) => Full(set(null))
    case o => setFromString(o.toString)
  }

	// assume string is json
	def setFromString(in: String): Box[JObject] = {
		// use lift-json to parse string into a JObject
		Full(set(JsonParser.parse(in).asInstanceOf[JObject]))
	}

	def toForm = <div></div>

	def owner = rec
}

/*
class JObjectCaseField[OwnerType <: Record[OwnerType], CaseType](rec: OwnerType) extends Field[JObject, OwnerType] {

	//import MongoHelpers._

	def asJs = Str(toString)

	def asXHtml = <div></div>

	def defaultValue = JObject(List())

	def setFromAny(in: Any): Box[JObject] = in match {
  	case jv: JObject => Full(set(jv))
  	case Some(jv: JObject) => Full(set(jv))
    case Full(jv: JObject) => Full(set(jv))
    //case dbo: DBObject => Full(set(dbObjectToJObject(dbo)))
    case seq: Seq[_] if !seq.isEmpty => seq.map(setFromAny)(0)
    case (s: String) :: _ => setFromString(s)
    case null => Full(set(null))
    case s: String => setFromString(s)
    case None | Empty | Failure(_, _, _) => Full(set(null))
    case o => setFromString(o.toString)
  }

	def setFromString(in: String): Box[JObject] = {
		Full(set(JsonParser.parse(in).asInstanceOf[JObject])) // use lift-json to go directly to JObject
	}

	def toForm = <div></div>

	def owner = rec
}
*/