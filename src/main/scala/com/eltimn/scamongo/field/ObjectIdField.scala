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
import net.liftweb.record.{Field, Record}

import com.mongodb.{ObjectId, DBRef}

/*
* Field for storing an ObjectId
*/
class ObjectIdField[OwnerType <: MongoRecord[OwnerType]](rec: OwnerType)
	extends Field[ObjectId, OwnerType] {

	def asJs = Str(toString)

	def asXHtml = <div></div>

	def defaultValue = ObjectId.get

	def setFromAny(in: Any): Box[ObjectId] = in match {
  	case oid: ObjectId => Full(set(oid))
  	case Some(oid: ObjectId) => Full(set(oid))
    case Full(oid: ObjectId) => Full(set(oid))
    case seq: Seq[_] if !seq.isEmpty => seq.map(setFromAny)(0)
    case (s: String) :: _ => setFromString(s)
    case null => Full(set(null))
    case s: String => setFromString(s)
    case None | Empty | Failure(_, _, _) => Full(set(null))
    case o => setFromString(o.toString)
  }
  
	def setFromString(in: String): Box[ObjectId] = {
		ObjectId.isValid(in) match {
			case true => Full(set(new ObjectId(in)))
			case false => Empty 
		}
	}
	
	def toForm = <div></div>

	def owner = rec
}

