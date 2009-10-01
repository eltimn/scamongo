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

import net.liftweb.json.JsonAST.JObject

import com.mongodb._

/*
* extend case class with this trait
*/
trait MongoDocument[BaseDocument] extends JsonObject[BaseDocument] {
	self: BaseDocument =>

	def _id: Any

	def meta: MongoDocumentMeta[BaseDocument]

	def delete {
		meta.delete("_id", _id)
	}

	def save:BaseDocument = meta.save(this)
	
	def getRef: MongoRef = MongoRef(meta.collectionName, _id.toString)
}

/*
* extend case class companion objects with this trait
*/
trait MongoDocumentMeta[BaseDocument] extends JsonObjectMeta[BaseDocument] with MongoMeta[BaseDocument] {

	def create(dbo: DBObject): BaseDocument = {
		create(JObjectParser.serialize(dbo).asInstanceOf[JObject])
	}
	
	/**
	* Find a single document by _id object.
	def find(a: Any): Option[BaseDocument] = a match {
		case oid: ObjectId => find(new BasicDBObject("_id", oid))
		case s: String => find(new BasicDBObject("_id", s))
	}
	*/

	/**
	* Find a single row by a qry, using a DBObject.
	*/
	private def find(qry: DBObject): Option[BaseDocument] = {
		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll =>
			coll.findOne(qry) match {
				case null => None
				case dbo => {
					Some(create(dbo))
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
	def find(json: JObject): Option[BaseDocument] = find(JObjectParser.parse(json))

	/**
	* Find all documents in this collection
	*/
	def findAll: List[BaseDocument] = {
		val ret = new ListBuffer[BaseDocument]

		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll => {
			val cur = coll.find
			while (cur.hasNext) {
				ret += create(cur.next)
			}
		})
		ret.toList
	}

	/**
	* Find all documents using a DBObject query
	*/
	def findAll(qry: DBObject, sort: Option[DBObject], opts: FindOption*): List[BaseDocument] = {
		val ret = new ListBuffer[BaseDocument]
		val cur = getCursor(qry, sort, opts :_*)

		while (cur.hasNext) {
			ret += create(cur.next)
		}
		ret.toList
	}

	/**
	* Find all documents using a DBObject query. These are for passing in regex queries.
	*/
	def findAll(qry: DBObject, opts: FindOption*): List[BaseDocument] =
		findAll(qry, None, opts :_*)

	/**
	* Find all documents using a DBObject query with sort
	*/
	def findAll(qry: DBObject, sort: DBObject, opts: FindOption*): List[BaseDocument] =
		findAll(qry, Some(sort), opts :_*)

	/**
	* Find all documents using a JObject query
	*/
	def findAll(qry: JObject, opts: FindOption*): List[BaseDocument] =
		findAll(JObjectParser.parse(qry), None, opts :_*)

	/**
	* Find all documents using a JObject query with sort
	*/
	def findAll(qry: JObject, sort: JObject, opts: FindOption*): List[BaseDocument] =
		findAll(JObjectParser.parse(qry), Some(JObjectParser.parse(sort)), opts :_*)
		
	/**
	* Find all documents using a Map query
	*/
	def findAll(qry: Map[String, Any], opts: FindOption*): List[BaseDocument] =
		findAll(MapParser.parse(qry), None, opts :_*)

	/**
	* Find all documents using a Map query with sort
	*/
	def findAll(qry: Map[String, Any], sort: Map[String, Any], opts: FindOption*): List[BaseDocument] =
		findAll(MapParser.parse(qry), Some(MapParser.parse(sort)), opts :_*)

	/**
	* Find all documents using a k, v query
	*/
	def findAll(k: String, o: Any, opts: FindOption*): List[BaseDocument] = 
		findAll(new BasicDBObject(k, o), None, opts :_*)

	/**
	* Find all documents using a k, v query with JObject sort
	*/
	def findAll(k: String, o: Any, sort: JObject, opts: FindOption*): List[BaseDocument] = 
		findAll(new BasicDBObject(k, o), Some(JObjectParser.parse(sort)), opts :_*)
		
	/**
	* Find all documents using a k, v query with Map sort
	*/
	def findAll(k: String, o: Any, sort: Map[String, Any], opts: FindOption*): List[BaseDocument] = 
		findAll(new BasicDBObject(k, o), Some(MapParser.parse(sort)), opts :_*)

	/*
	* Save a document to the db
	*/
	def save(in: BaseDocument): BaseDocument = {
		MongoDB.use(mongoIdentifier) ( db => {
			save(in, db)
		})
	}

	/*
	* Save a document to the db using the given Mongo instance
	*/
	def save(in: BaseDocument, db: DB): BaseDocument = {
		create(db.getCollection(collectionName).save(JObjectParser.parse(toJObject(in))))
	}

	/*
	* Update document with a JObject query using the given Mongo instance
	*/
	def update(qry: JObject, newbd: BaseDocument, db: DB, opts: UpdateOption*): BaseDocument = {
		create(update(qry, toJObject(newbd), db, opts :_*))
	}

	/*
	* Update document with a JObject query
	*/
	def update(qry: JObject, newbd: BaseDocument, opts: UpdateOption*): BaseDocument = {
		MongoDB.use(mongoIdentifier) ( db => {
			update(qry, newbd, db, opts :_*)
		})
	}

}
