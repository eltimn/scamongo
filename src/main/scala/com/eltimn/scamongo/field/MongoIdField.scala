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
import net.liftweb.record.field.StringField
import net.liftweb.util.{Box, Empty, Failure, Full}

import com.mongodb.{ObjectId, DBRefBase}

/*
* Field for storing an ObjectId as a string
*/
class MongoIdField[OwnerType <: MongoRecord[OwnerType]](rec: OwnerType)
	extends StringField[OwnerType](rec, 24) {
	
	override def defaultValue = ObjectId.get.toString
	
	override def setFromString(in: String): Box[String] = {
		ObjectId.isValid(in) match {
			case true => Full(set(in))
			case false => Empty 
		}
	}

	def getRef: MongoRef =
		MongoRef(owner.meta.collectionName, value)
}
