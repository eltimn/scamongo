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

import java.util.regex.Pattern

import scala.collection.mutable.ListBuffer

import net.liftweb.record.{MetaRecord, Record}
import net.liftweb.util.{Box, Empty, Full}

import com.mongodb.{BasicDBObjectBuilder, BasicDBObject, DBObject, MongoException}
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
	private def createRecord(obj: DBObject): Box[BaseRecord] = {
		val rec: BaseRecord = rootClass.newInstance.asInstanceOf[BaseRecord]
		rec.runSafe {
			introspect(rec, rec.getClass.getMethods) {case (v, mf) =>}
		}
		// convert to json using mongo's parser, then populate record via fromJson method
		rec.fromJSON(JSON.serialize(obj))
	}

	/**
	* Delete the instance from backing store
	*/
	def delete_!(inst: BaseRecord): Boolean = {
		foreachCallback(inst, _.beforeDelete)
		try {
			MongoDB.useCollection(mongoIdentifier, collectionName) ( coll =>
				coll.remove(new BasicDBObject("_id", inst.id))
			)
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
	* Save the instance in the appropriate backing store
	*/
	def save(inst: BaseRecord): Boolean = {
		foreachCallback(inst, _.beforeSave)
		try {
			MongoDB.useCollection(mongoIdentifier, collectionName) ( coll =>
				coll.save(JSON.parse(asJSON(inst).toString)) // convert to DBObject via json
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

	/**
	* Was this instance saved in backing store?
	*/
	def saved_?(inst: BaseRecord): Boolean = true

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
	def find(a: Any): Box[BaseRecord] = find(new BasicDBObject("_id", a))

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
	* Find a single row by a qry using String, Any inputs
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
	* Find all rows using a regex query
	
	def findAllRegex(k: String, regex: String): List[BaseRecord] = {
		var ret = new ListBuffer[BaseRecord]

		MongoDB.use(dbMongoIdentifier, collectionName) ( coll => {
			val cur = coll.find(
				BasicDBObjectBuilder.start().add(k, Pattern.compile(regex)).get())
			while (cur.hasNext) {
				createRecord(cur.next) match {
					case Full(obj) => ret += obj
					case _ =>
				}
			}
		})
		ret.toList
	}
*/
}
