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

import scala.collection.mutable.ListBuffer

import com.mongodb._

/*
* extend case class with this trait
*/
trait MongoDocument[BaseDocument] {
	self: BaseDocument =>

	def _id: Any

	def meta: MongoDocumentMeta[BaseDocument]

	def delete {
		meta.delete(this._id.toString)
	}

	def save {
		meta.save(this)
	}
}

/*
* extend case class companion objects with this trait
*/
trait MongoDocumentMeta[BaseDocument] extends MongoMeta[BaseDocument] {

	
	import net.liftweb.json.JsonAST.JValue
	import MongoHelpers._

	// convert json value to a class of BaseDocument
	def fromJson(in: JValue): BaseDocument/* = {
		implicit val formats = MongoFormats
		in.extract[BaseDocument]
	}
	Throws:  no implicit argument matching parameter type scala.reflect.Manifest[BaseDocument] was found.
	*/

	// convert class to a json value
	def toJson(in: BaseDocument): JValue /* = {
		compact(render({
			("_id" -> per._id) ~ ("name" -> per.name) ~ ("age" -> per.age)
		}))
	}
	*/

	// delete a document
	def delete(in: String) {
		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll =>
			coll.remove(new BasicDBObject("_id", in))
		)
	}

	/*
	* Delete documents by a json value query
	*/
	def delete(json: JValue) {
		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll =>
			coll.remove(jValueToDBObject(json))
		)
	}

	/**
	* Find a single document by _id object.
	*/
	def find(a: String): Option[BaseDocument] = find(new BasicDBObject("_id", a))
	/*
	def find(a: Any): Option[BaseDocument] = a match {
		case oid: ObjectId => find(new BasicDBObject("_id", oid))
		case s: String => find(new BasicDBObject("_id", s))
	}
	*/

	/**
	* Find a single row by a qry, using a DBObject.
	*/
	def find(qry: DBObject): Option[BaseDocument] = {
		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll =>
			coll.findOne(qry) match {
				case null => None
				case dbo => {
					Some(fromJson(dbObjectToJValue(dbo)))
				}
			}
		)
	}
	
	/**
	* Find a single document by a qry using String, Any inputs
	*/
	def find(k: String, o: Any): Option[BaseDocument] = find(new BasicDBObject(k, o))

	/**
	* Find a single document by a qry using a json value
	*/
	def find(json: JValue): Option[BaseDocument] = find(jValueToDBObject(json))
	
	//def find(o: ObjectId): Option[BaseDocument] = find(new BasicDBObject("_id", o.toString))

	/**
	* Find all documents in this collection
	*/
	def findAll: List[BaseDocument] = {
		var ret = new ListBuffer[BaseDocument]

		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll => {
			val cur = coll.find
			while (cur.hasNext) {
				ret += fromJson(dbObjectToJValue(cur.next))
			}
		})
		ret.toList
	}

	/**
	* Find all documents using a DBObject query.
	*/
	def findAll(qry: DBObject, limit: Int, skip: Int, sort: Option[DBObject]): List[BaseDocument] = {
		var ret = new ListBuffer[BaseDocument]

		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll => {
			val cur = coll.find(qry).limit(limit).skip(skip)
			sort.foreach( s => cur.sort(s))
			while (cur.hasNext) {
				ret += fromJson(dbObjectToJValue(cur.next))
			}
		})
		ret.toList
	}

	/**
	* Find all documents using a "query" via a json document with a limit and a skip
	*/
	def findAll(json: JValue, limit: Int, skip: Int): List[BaseDocument] = findAll(jValueToDBObject(json), limit, skip, None)

	/**
	* Find all documents using a "query" via a json document with a limit
	*/
	def findAll(json: JValue, limit: Int): List[BaseDocument] = findAll(json, limit, 0)

	/**
	* Find all documents using a "query" via a json document
	*/
	def findAll(json: JValue): List[BaseDocument] = findAll(json, 0, 0)

	/**
	* Find all documents using a "query" via a json document with a limit, a skip, and a sort
	*/
	def findAll(json: JValue, limit: Int, skip: Int, sort: JValue): List[BaseDocument] = {
		findAll(jValueToDBObject(json), limit, skip, Some(jValueToDBObject(sort)))
	}

	/**
	* Find all documents using a "query" via a json value with a limit and a sort
	*/
	def findAll(json: JValue, limit: Int, sort: JValue): List[BaseDocument] = findAll(json, limit, 0, sort)

	/**
	* Find all documents using a "query" via a json value with a sort
	*/
	def findAll(json: JValue, sort: JValue): List[BaseDocument] = findAll(json, 0, 0, sort)

	/**
	* Find all documents using a "query" via k, Any
	*/
	def findAll(k: String, o: Any): List[BaseDocument] = findAll(new BasicDBObject(k, o), 0, 0, None)

	/*
	* Save a document to the db
	*/
	def save(in: BaseDocument) {
		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll => {
			coll.save(jValueToDBObject(toJson(in)))
		})
	}

	/*
	* Save a document to the db using the given Mongo instance
	*/
	def save(in: BaseDocument, db: DBBase) {
		db.getCollection(collectionName).save(jValueToDBObject(toJson(in)))
	}

	/*
	* Update document
	*/
	def update(qry: JValue, newobj: JValue, opts: (String, Boolean)*) {
		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll => {
			val dboOpts = toUpdOpts(opts.toList)
			coll.update(jValueToDBObject(qry), jValueToDBObject(newobj), dboOpts._1, dboOpts._2)
		})
	}

	/*
	* Update document using the given Mongo instance
	*/
	def update(qry: JValue, newobj: JValue, db: DBBase, opts: (String, Boolean)*) {
		val dboOpts = toUpdOpts(opts.toList)
		db.getCollection(collectionName).update(jValueToDBObject(qry), jValueToDBObject(newobj), dboOpts._1, dboOpts._2)
	}

	/*
	* Convert the update varargs to an ordered tuple
	*/
	private def toUpdOpts(opts: List[(String, Boolean)]): (Boolean, Boolean) = opts match {
		case ("upsert", u) :: ("apply", a) :: _ => (u, a)
		case ("apply", a) :: ("upsert", u) :: _ => (u, a)
		case ("upsert", u) :: _ => (u, false)
		case ("apply", a) :: _ => (false, a)
		case _ => (false, false)
	}
}
