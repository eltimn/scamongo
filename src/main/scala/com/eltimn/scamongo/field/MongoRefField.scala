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

import com.mongodb.{BasicDBObject, BasicDBObjectBuilder, DBObject, DBRefBase}

/*
* Field for storing a DBRef
*/
//class MongoRefField[OwnerType <: MongoRecord[OwnerType], RefType <: MongoRecord[RefType]](rec: OwnerType, refMeta: RefType) extends Field[DBRef, OwnerType] {
//class MongoRefField[OwnerType <: MongoRecord[OwnerType], MongoMetaRecord](rec: OwnerType, refMeta: MongoMetaRecord) extends Field[DBRef, OwnerType] {
class MongoRefField[OwnerType <: MongoRecord[OwnerType]](rec: OwnerType)
	extends Field[DBRef, OwnerType] with MongoFieldFlavor[DBRef] {

	def asJs = Str(toString)

	def asXHtml = <div></div>

	def defaultValue = null

	def setFromAny(in: Any): Box[DBRef] = in match {
  	case ref: DBRef => Full(set(ref))
  	case Some(ref: DBRef) => Full(set(ref))
    case Full(ref: DBRef) => Full(set(ref))
    case dbo: DBObject => setFromDBObject(dbo)
    case seq: Seq[_] if !seq.isEmpty => seq.map(setFromAny)(0)
    case (s: String) :: _ => setFromString(s)
    case null => Full(set(null))
    case s: String => setFromString(s)
    case None | Empty | Failure(_, _, _) => Full(set(null))
    case o => setFromString(o.toString)
  }

	def toForm = <div></div>

	def owner = rec
	
	/*
	* fetch the referenced Record 
	*/
	/*def fetch: Box[Any] = {
		refMeta.find(new ObjectId(value.id))
	}*/
	
	/*
	* Convert this field's value into a DBObject so it can be stored in Mongo.
	*/
  def asDBObject: DBObject = {
  	/*
  	val dbob = BasicDBObjectBuilder.start
  	dbob.add("$ref", value.ref)
  	dbob.add("$id", value.id)
  	dbob.get
  	*/
  	new BasicDBObject("ref", value.ref).append("id", value.id)
  }

	// set this field's value using a DBObject returned from Mongo.
	def setFromDBObject(dbo: DBObject): Box[DBRef] = {
  	(dbo.containsField("ref"), dbo.containsField("id")) match {
  		case (true, true) => Full(set(DBRef(dbo.get("ref").toString, dbo.get("id").toString)))
  		case _ => Empty //Full(set(null))
  	}
  }

}

/*
* Field for storing a DBRefBase

class MongoRefBaseField[OwnerType <: Record[OwnerType]](rec: OwnerType)
	extends Field[DBRefBase, OwnerType] with MongoFieldFlavor[DBRefBase] {

	def asJs = Str(toString)

	def asXHtml = <div></div>

	def defaultValue = null

	def setFromAny(in: Any): Box[DBRefBase] = in match {
  	case ref: DBRefBase => Full(set(ref))
  	case Some(ref: DBRefBase) => Full(set(ref))
    case Full(ref: DBRefBase) => Full(set(ref))
    case dbo: DBObject => setFromDBObject(dbo)
    case seq: Seq[_] if !seq.isEmpty => seq.map(setFromAny)(0)
    case (s: String) :: _ => setFromString(s)
    case null => Full(set(null))
    case s: String => setFromString(s)
    case None | Empty | Failure(_, _, _) => Full(set(null))
    case o => setFromString(o.toString)
  }

	def toForm = <div></div>

	def owner = rec
	
	/*
	* Convert this field's value into a DBObject so it can be stored in Mongo.
	*/
  def asDBObject: DBObject = {
  	new BasicDBObject("ref", value.ref).append("id", value.id)
  }

	// set this field's value using a DBObject returned from Mongo.
	def setFromDBObject(dbo: DBObject): Box[DBRef] = {
  	(dbo.containsField("ref"), dbo.containsField("id")) match {
  		case (true, true) => Full(set(DBRef(dbo.get("ref").toString, dbo.get("id").toString)))
  		case _ => Full(set(null))
  	}
  }

}
*/
