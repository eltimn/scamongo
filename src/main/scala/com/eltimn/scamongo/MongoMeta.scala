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

import net.liftweb.json.JsonAST.JValue
import com.mongodb._
import MongoHelpers._

/*
* This is used by both MongoDocumentMeta and MongoMetaRecord
*/
trait MongoMeta[BaseDocument] {

	// class name has a $ at the end. because it's an object(?)
	private lazy val _collectionName = {
		getClass.getName.split("\\.").toList.last.replace("$", "")+"s"
	}

	/*
	* Collection names should begin with letters or an underscore and may include
	* numbers; $ is reserved. Collections can be organized in namespaces; these
	* are named groups of collections defined using a dot notation. For example,
	* you could define collections blog.posts and blog.authors, both reside under
	* "blog". Note that this is simply an organizational mechanism for the user
	* -- the collection namespace is flat from the database's perspective.
	* From: http://www.mongodb.org/display/DOCS/Collections
	*/
	private def fixCollectionName(name: String) = name.toLowerCase match {
		case name if (name.contains("$")) => name.replace("$", "_d_")
		case name => name
	}

	/**
	* The name of the database collection.  Override this method if you
	* want to change the collection to something other than the name of
	* the MongoDocument case class with an 's' appended to the end.
	*/
	def collectionName = fixCollectionName(_collectionName)

	// override this to specify a MongoIdentifier for this MongoDocumnet type
  def mongoIdentifier: MongoIdentifier = DefaultMongoIdentifier
  
  /*
	* Count all documents
	*/
	def count: Long = {
		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll =>
			coll.getCount
		)
	}

	/*
	* Count documents by DBObject query
	*/
	private def count(qry: DBObject):Long = {
		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll =>
			coll.getCount(qry)
		)
	}

	/*
	* Count documents by JValue query
	*/
	def count(qry: JValue):Long = count(jValueToDBObject(qry))
	
	/* drop this document collection
	def drop {
		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll =>
			coll.drop
		)
	}
	*/
	
	/*
	* Ensure an index exists
	*/
	def ensureIndex(keys: JValue, opts: (String, Boolean)*) {
		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll => {
			val ixOpts = toIxOpts(opts.toList)
			coll.ensureIndex(jValueToDBObject(keys), ixOpts._1, ixOpts._2)
		})
	}

	/*
	* Ensure an index exists, giving it a name
	*/
	def ensureIndex(keys: JValue, name: String, opts: (String, Boolean)*) {
		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll => {
			val ixOpts = toIxOpts(opts.toList)
			coll.ensureIndex(jValueToDBObject(keys), name, ixOpts._2)
		})
	}

	/*
	* Convert the index varargs to an ordered tuple
	*/
	private def toIxOpts(opts: List[(String, Boolean)]): (Boolean, Boolean) = opts match {
		case ("force", f) :: ("unique", u) :: _ => (f, u)
		case ("unique", u) :: ("force", f) :: _ => (f, u)
		case ("force", f) :: _ => (f, false)
		case ("unique", u) :: _ => (false, u)
		case _ => (false, false)
	}	

}