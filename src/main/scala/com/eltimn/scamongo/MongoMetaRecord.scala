package com.eltimn.scamongo

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

import java.util.Calendar
import java.util.regex.Pattern

import scala.collection.mutable.ListBuffer

import net.liftweb.json.JsonAST.JObject
import net.liftweb.record.{MetaRecord, Record}
import net.liftweb.record.field._
import net.liftweb.util.{Box, Empty, Full}

import com.mongodb._
import com.mongodb.util.JSON

trait MongoMetaRecord[BaseRecord <: MongoRecord[BaseRecord]]
	extends MetaRecord[BaseRecord] with MongoMeta[BaseRecord] {

	self: BaseRecord =>

	//def afterCommit: List[BaseRecord => Unit] = Nil

	/**
   * Creates a new, empty record

  override def createRecord: BaseRecord = {
    super.createRecord
  }*/

  /**
   * Creates a new record from a JSON construct
   *
   * @param json - the stringified JSON stucture

  override def createRecord(json: String): Box[BaseRecord] = {
    super.createRecord(json)
  }*/

	/**
	* Creates a new record from a DBObject. For internal use only.
	*
	* @param obj - the DBObject
	*/
	private def createRecord(dbo: DBObject): Box[BaseRecord] = {
		val rec: BaseRecord = rootClass.newInstance.asInstanceOf[BaseRecord]
		rec.runSafe {
			introspect(rec, rec.getClass.getMethods) {case (v, mf) =>}
		}
		// convert to json using mongo's parser, then populate record via fromJson method
		//rec.fromJSON(JSON.serialize(dbo))
		rec.fromDBObject(dbo) // convert to this using fromDBObject method
	}

	/**
	* Delete the instance from backing store
	*/
	def delete_!(inst: BaseRecord): Boolean = {
		foreachCallback(inst, _.beforeDelete)
		try {
			delete("_id", inst.id)
			/*
			MongoDB.useCollection(mongoIdentifier, collectionName) ( coll =>
				coll.remove(new BasicDBObject("_id", inst.id))
			)
			*/
			true
		}
		catch {
			case me: MongoException => false
		}
		finally {
			foreachCallback(inst, _.afterDelete)
		}
	}

	/**
	* Find a single row by a qry, using a DBObject. For internal use only.
	*/
	private def find(qry: DBObject): Box[BaseRecord] = {
		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll =>
			coll.findOne(qry) match {
				case null => Empty
				case dbo => createRecord(dbo)
			}
		)
	}

	/**
	* Find a single row by id
	*/
	//def find(a: Any): Box[BaseRecord] = find(new BasicDBObject("_id", a))

	/**
	* Find a single row by a qry, using a Map

	def find(qrymap: Map[String,String]): Box[BaseRecord] = {
		// convert map to DBObject
		val qry = new BasicDBObject()

		for (k <- qrymap.keys) {
			qry.put(k.toString, qrymap(k.toString))
		}

		find(qry)
	}*/

	/**
	* Find a single row by a qry using String key and Any value
	*/
	def find(k: String, o: Any): Box[BaseRecord] = find(new BasicDBObject(k, o))

	/**
	* Find a single row by a DBRefBase

	def find(oid: DBRefBase): Box[BaseRecord]= {
		oid.fetch match {
			case null => Empty
			case obj => createRecord(obj)
		}
	}
*/
	/**
	* Find all rows in this collection
	*/
	def findAll: List[BaseRecord] = {
		var ret = new ListBuffer[BaseRecord]

		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll => {
			val cur = coll.find
			while (cur.hasNext) {
				createRecord(cur.next) match {
					case Full(obj) => ret += obj
					case _ =>
				}
			}
		})
		ret.toList
	}

	/**
	* Find all rows using a DBObject query. Internal use only.
	*/
	private def findAll(qry: DBObject): List[BaseRecord] = {
		var ret = new ListBuffer[BaseRecord]

		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll => {
			val cur = coll.find(qry)
			while (cur.hasNext) {
				createRecord(cur.next) match {
					case Full(obj) => ret += obj
					case _ =>
				}
			}
		})
		ret.toList
	}

	/**
	* Find all rows using a "query" via a Map

	def findAll(qrymap: Map[String,String]): List[BaseRecord] = {
		// convert the map to a DBObject
		val qry = new BasicDBObject
		for (k <- qrymap.keys) {
			qry.put(k.toString, qrymap(k.toString))
		}

		// do the query
		findAll(qry)
	}*/

	/**
	* Find all rows using a "query" via k, Any
	*/
	def findAll(k: String, o: Any): List[BaseRecord] = findAll(new BasicDBObject(k, o))

	/**
	* Save the instance in the appropriate backing store
	*/
	def save(inst: BaseRecord): Boolean = {
		foreachCallback(inst, _.beforeSave)
		try {
			MongoDB.useCollection(mongoIdentifier, collectionName) ( coll =>
				//coll.save(JSON.parse(asJSON(inst).toString)) // convert to DBObject via json
				coll.save(toDBObject(inst))
			)
			true
		}
		catch {
			case me: MongoException => false
		}
		finally {
			foreachCallback(inst, _.afterSave)
		}
	}

	/*
	* Save a document to the db using the given Mongo instance
	*/
	def save(inst: BaseRecord, db: DBBase): Boolean = {
		foreachCallback(inst, _.beforeSave)
		try {
			db.getCollection(collectionName).save(toDBObject(inst))
			true
		}
		catch {
			case me: MongoException => false
		}
		finally {
			foreachCallback(inst, _.afterSave)
		}
	}

	/**
	* Was this instance saved in backing store?
	*/
	def saved_?(inst: BaseRecord): Boolean = true

	/**
	* Populate the inst's fields with the values from a DBObject. Values are set
	* using setFromAny passing it the DBObject returned from Mongo.
	*
	* @param inst - the record that will be populated
	* @param obj - The DBObject
	* @return Box[BaseRecord]
	*/
	def fromDBObject(inst: BaseRecord, obj: DBObject): Box[BaseRecord] = {
		// loop thru each field name
		val keys = obj.keySet
		keys.size match {
			case 0 => Empty
			case _ => {
				for (k <- keys.toArray) {
					// mongo-java-driver returns json objects as string instead of DBObjects
					inst.fieldByName(k.toString).map(field => 
						field.setFromAny(obj.get(k.toString))
					)
				}
				Full(inst)
			}
		}
	}

	/**
	* Create a BasicDBObject from the field names and values.
	* - Dates are stored using lift-json formatter in UTC time, eg:
	*   yyyy-MM-dd'T'HH:mm:ss'Z'
	* - MongoFieldFlavor types (List) are converted to DBObjects
	*   using toDBObject
	*/
	private def toDBObject(inst: BaseRecord): DBObject = {

		import MongoHelpers._

		val dbo = new BasicDBObject

		for (f <- fields) {
			fieldByName(f.name, inst) match {
				/* FIXME: Doesn't work
				case Full(field) if field.isInstanceOf[CountryField[Any]] =>
					dbo.put(f.name, field.asInstanceOf[CountryField[Any]].value)
				*/
				case Full(field) => field.value match {
					case b: Boolean => dbo.put(f.name, b)
					case c: Calendar => dbo.put(f.name, MongoFormats.dateFormat.format(c.getTime))
					case d: Double => dbo.put(f.name, d)
					case n: Int => dbo.put(f.name, n)
					case l: Long => dbo.put(f.name, l)
					case s: String => dbo.put(f.name, s)
					case jv: JObject => dbo.put(f.name, jObjectToDBObject(jv))
					case _ => dbo.put(f.name, field.toString)
				}

				/*case Full(field) if field.isInstanceOf[MongoFieldFlavor[Any]] =>
					obj.put(f.name, field.asInstanceOf[MongoFieldFlavor[Any]].toDBObject)
				*/

				case _ => dbo.markAsPartialObject // so we know it's only partial
			}
		}
		dbo
	}

}
