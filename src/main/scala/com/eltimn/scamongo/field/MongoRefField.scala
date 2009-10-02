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

import net.liftweb.http.js.JE.Str
import net.liftweb.record.{Field, Record}
import net.liftweb.util.{Box, Empty, Failure, Full}

import com.mongodb.{BasicDBObject, BasicDBObjectBuilder, DBObject, DBRef, ObjectId}

/*
* Field for storing a DBRef
*/
//abstract class MongoRefField[OwnerType <: MongoRecord[OwnerType]](rec: OwnerType)
//abstract class MongoRefField[OwnerType <: MongoRecord[OwnerType], RefType <: MongoMetaRecord[RefType]](rec: OwnerType, ref: RefType)
abstract class MongoRefField[OwnerType <: MongoRecord[OwnerType], RefType <: MongoRecord[RefType]](rec: OwnerType, ref: RefType)
	extends Field[DBRef, OwnerType] {

	/*
	* fetch the referenced object
	*/
	def fetch = ref.meta.findAny(value.getId)

	def asJs = Str(toString)

	def asXHtml = <div></div>

	def defaultValue = new DBRef(null, null, null)

	def setFromAny(in: Any): Box[DBRef] = in match {
  	case ref: DBRef => Full(set(ref))
  	case Some(ref: DBRef) => Full(set(ref))
    case Full(ref: DBRef) => Full(set(ref))
    case seq: Seq[_] if !seq.isEmpty => seq.map(setFromAny)(0)
    case (s: String) :: _ => setFromString(s)
    case null => Full(set(null))
    case s: String => setFromString(s)
    case None | Empty | Failure(_, _, _) => Full(set(null))
    case o => setFromString(o.toString)
  }

  // assume string is json
	def setFromString(in: String): Box[DBRef] = Empty

	def toForm = <div></div>

	def owner = rec

}

