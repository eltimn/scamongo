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

/*
* TODO: MongoField - setFromString
* TODO: rewrite jValueToDBObject and dbObjectToJValue to skip mongo parser/serializer
* and go directly to/from JValues.
* TOD: Go directly from Case Class to DBObject and vice versa (requires copying 
* code from lift-json)  Is this worth it if jValueToDBObject is updated?
* TODO: ObjectId and DBRef compatiblity
* TODO: Test all data types
* TODO: DefaultFormats -> MongoFormats
* TODO: Multiple mongos examples
*/

import scala.collection.immutable.HashSet
import scala.collection.mutable.{HashMap => MutableHashMap, ListBuffer}
import scala.reflect.Manifest

import net.liftweb.json.{DateFormat, DefaultFormats}
import net.liftweb.json.JsonAST.JObject

import com.mongodb._

/*
* The default MongoIdentifier
*/
case object DefaultMongoIdentifier extends MongoIdentifier {
  val jndiName = "mongo"
}

/*
* Wrapper for DBAddress
*/
class MongoAddress(host: String, port: Int, name: String) {

	def toDBAddress = new DBAddress(host, port, name)

	override def toString = host+":"+port+"/"+name
}

/*
* Main Mongo object
*/
object MongoDB {

	/*
	* HashMap of Mongo instances, keyed by MongoIdentifier
	*/
	private val mongos = new MutableHashMap[MongoIdentifier, Mongo]

	/*
	* Define a Mongo instance
	*/
	def defineMongo(name: MongoIdentifier, address: MongoAddress) {
    mongos(name) = new Mongo(address.toDBAddress)
  }

  /*
	* Define a Mongo instance with authorization
	*/
	def defineMongoAuth(name: MongoIdentifier, address: MongoAddress, username: String, password: String) {

		val newMongo = new Mongo(address.toDBAddress)

		if (!newMongo.authenticate(username, password))
			throw new MongoException("Authorization failed: "+address.toString)

    mongos(name) = newMongo
  }

	/*
	* Get a Mongo instance based on a MongoIdentifier
	*/
  private def getMongo(name: MongoIdentifier): Option[DBBase] = mongos.get(name)

  /*
  * Get a Mongo collection. Gets a Mongo first.
	*/
  private def getCollection(name: MongoIdentifier, collectionName: String): Option[DBCollection] = getMongo(name) match {
  	case Some(mongo) if mongo != null => Some(mongo.getCollection(collectionName))
  	case _ => None
  }

  /**
  * Executes function {@code f} with the mongo named {@code name}.
  */
  def use[T](name: MongoIdentifier)(f: (DBBase) => T): T = {

  	val db = getMongo(name) match {
			case Some(mongo) => mongo
			case _ => throw new MongoException("Mongo not found: "+name.toString)
		}

		f(db)
  }

  /**
  * Executes function {@code f} with the mongo named {@code name}. Uses the default mongoIdentifier
  */
  def use[T] (f: (DBBase) => T): T = {

  	val db = getMongo(DefaultMongoIdentifier) match {
			case Some(mongo) => mongo
			case _ => throw new MongoException("Mongo not found: "+DefaultMongoIdentifier.toString)
		}

		f(db)
  }

  /**
  * Executes function {@code f} with the mongo named {@code name} and collection names {@code collectionName}.
  * Gets a collection for you.
	*/
  def useCollection[T](name: MongoIdentifier, collectionName: String)(f: (DBCollection) => T): T = {
    val coll = getCollection(name, collectionName) match {
			case Some(collection) => collection
			case _ => throw new MongoException("Collection not found: "+collectionName+". MongoIdentifier: "+name)
		}

		f(coll)
  }

  /**
  * Executes function {@code f} with the mongo named {@code name}. Uses the same socket
  * for the entire function block. Allows multiple operations on the same thread and the
  * use of getLastError.
  * See: http://www.mongodb.org/display/DOCS/Java+Driver+Concurrency
  */
  def useSession[T](name: MongoIdentifier)(f: (DBBase) => T): T = {

  	val db = getMongo(name) match {
			case Some(mongo) => mongo
			case _ => throw new MongoException("Mongo not found: "+name.toString)
		}

		// start the request
		db.requestStart
		try {
      f(db)
    }
    finally {
    	// end the request
      db.requestDone
    }
  }
  
  //
  def close {
  	mongos.clear
  }
}

object MongoFormats extends DefaultFormats

/*
* Helper methods
*/
object MongoHelpers {

	import java.util.UUID

	import com.mongodb.util.JSON // Mongo parser/serializer
	import net.liftweb.json.JsonAST
	import net.liftweb.json.JsonAST.JObject
	import net.liftweb.json.JsonDSL
	import net.liftweb.json.JsonParser

	/* ID helper methods */
  def newMongoId = ObjectId.get.toString
  def newUUID = UUID.randomUUID.toString

  /*
  * Convert a JObject to a DBObject
  *
  * Mongo specific types
  * ObjectId --> ObjectId("kdhlkdh8769879")
  * DBRef -->
  *
  */
  def jObjectToDBObject(in: JObject): DBObject = {
  	//println(JsonDSL.compact(JsonAST.render(in)))
  	JSON.parse(JsonDSL.compact(JsonAST.render(in)))
  }

  /*
  * Convert a DBObject to a JObject
  */
  def dbObjectToJObject(in: DBObject): JObject = {
  	JsonParser.parse(JSON.serialize(in)).asInstanceOf[JObject]
  }

  /*
  * Get a BasicDBObjectBuilder. Use this for doing regex queries.
  */
  def dbObjectBuilder: BasicDBObjectBuilder = BasicDBObjectBuilder.start
}

/*
* A trait for identfying Mongo instances
*/
trait MongoIdentifier {
  def jndiName: String
  override def toString() = "MongoIdentifier("+jndiName+")"
  override def hashCode() = jndiName.hashCode()
  override def equals(other: Any): Boolean = other match {
    case mi: MongoIdentifier => mi.jndiName == this.jndiName
    case _ => false
  }
}

/*
* This is used by both MongoDocumentMeta and MongoMetaRecord
*/
trait MongoMeta[BaseDocument] {

	import MongoHelpers._

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
	def count(qry: JObject):Long = count(jObjectToDBObject(qry))
	
	// delete a document
	def delete(k: String, v: Any) {
		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll =>
			coll.remove(new BasicDBObject(k, v))
		)
	}

	/*
	* Delete documents by a json object query
	*/
	def delete(json: JObject) {
		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll =>
			coll.remove(jObjectToDBObject(json))
		)
	}

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
	def ensureIndex(keys: JObject, opts: IndexOption*) {
		val ixOpts = opts.toList
		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll => {
			coll.ensureIndex(jObjectToDBObject(keys),
				ixOpts.find(_ == Force).map(x => true).getOrElse(false),
				ixOpts.find(_ == Unique).map(x => true).getOrElse(false)
			)
		})
	}

	/*
	* Ensure an index exists, giving it a name
	*/
	def ensureIndex(keys: JObject, name: String, opts: IndexOption*) {
		val ixOpts = opts.toList
		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll => {
			coll.ensureIndex(jObjectToDBObject(keys), name,
				ixOpts.find(_ == Unique).map(x => true).getOrElse(false)
			)
		})
	}
}

/*
* For passing in options to the ensureIndex function
*/
abstract sealed class IndexOption
case object Force extends IndexOption
case object Unique extends IndexOption

/*
* For passing in options to the find function
*/
abstract sealed class FindOption {
	def value: Int
}
case class Limit(value: Int) extends FindOption
case class Skip(value: Int) extends FindOption

/*
* For passing in options to the update function
*/
abstract sealed class UpdateOption
case object Upsert extends UpdateOption
case object Apply extends UpdateOption

/*
* These traits provide lift-json related convienece methods for case classes 
* and their companion objects
*/
trait JsonObject[BaseDocument] {
	self: BaseDocument =>

	def meta: JsonObjectMeta[BaseDocument]

	// convert class to a json value
	def asJObject: JObject = meta.toJObject(this)

}

class JsonObjectMeta[BaseDocument](implicit mf: Manifest[BaseDocument]) {

	import net.liftweb.json.Extraction._

	private val formats = DefaultFormats

	// create an instance of BaseDocument from a JObject
	def create(in: JObject): BaseDocument = {
		extract(in)(formats, mf)
	}

	// convert class to a json object
	def toJObject(in: BaseDocument): JObject = serialize(in)(formats).asInstanceOf[JObject]
}

/* */
case class MongoId(id: String) {
  
  override def toString() = id //oid.toString //"ObjectId(\""+oid.toString+"\")"
  /*
  override def hashCode() = oid.hashCode()
  override def equals(other: Any): Boolean = other match {
  	case otherOid: ObjectId if (otherOid.equals(this.oid)) => true
  	case _ => false
  }
  */
}

object MongoId {
/*
	def apply(s: String): MongoId = {
    new MongoId(new ObjectId(s))
  }
*/  
  def apply(): MongoId = {
    new MongoId(ObjectId.get.toString)
  }

}

