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
* TODO: rewrite jValueToDBObject and dbObjectToJValue to skip mongo parser/serializer
* and go directly to/from JValues
* TODO: ObjectId and DBRef compatiblity
* TODO: Test all data types
* TODO: DefaultFormats -> MongoFormats
* TODO: Multiple mongos examples
*/

import scala.collection.immutable.HashSet
import scala.collection.mutable.{HashMap => MutableHashMap}

import net.liftweb.json.{DateFormat, Formats}

import com.mongodb.{ObjectId => MObjectId, _}

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
  private def getMongo(name: MongoIdentifier): Option[Mongo] = mongos.get(name)

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
}

/*
* Formats used by lift-json
*/
object MongoFormats extends MongoFormats
trait MongoFormats extends Formats {

	import java.text.{ParseException, SimpleDateFormat}
	import java.util.{Date, Locale, TimeZone}

  /*
  * The UTC TimeZone
  */
  val utc = TimeZone.getTimeZone("UTC")

  val dateFormat = new DateFormat {
  	val sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'", Locale.US)
    sdf.setTimeZone(utc)

    def parse(s: String) = try {
      Some(sdf.parse(s))
    }
    catch {
      case e: ParseException => None
    }

    def format(d: Date) = sdf.format(d)
  }
}

/*
* Helper methods
*/
object MongoHelpers {

	import java.util.UUID

	import com.mongodb.util.JSON // Mongo parser/serializer
	import net.liftweb.json.JsonAST
	import net.liftweb.json.JsonAST.JValue
	import net.liftweb.json.JsonDSL
	import net.liftweb.json.JsonParser

	/* ID helper methods */
  def newId = MObjectId.get
  def newUUID = UUID.randomUUID

  /*
  * Convert a JValue to a DBObject
  *
  * Mongo specific types
  * ObjectId --> ObjectId("kdhlkdh8769879")
  * DBRef --> 
  * 
  */
  def jValueToDBObject(in: JValue): DBObject = {
  	JSON.parse(JsonDSL.compact(JsonAST.render(in)))
  }

  /*
  * Convert a DBObject to a JValue
  */
  def dbObjectToJValue(in: DBObject): JValue =
  	JsonParser.parse(JSON.serialize(in))

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
class ObjectId(oid: MObjectId) {
  override def toString() = oid.toString //"ObjectId(\""+oid.toString+"\")"
  override def hashCode() = oid.hashCode()
  override def equals(other: Any): Boolean = other match {
  	case otherOid: MObjectId if (otherOid.equals(this.oid)) => true
  	case _ => false
  }
}

object ObjectId {
	def apply(s: String) = {
    new ObjectId(new MObjectId(s))
  }
  def apply() = {
    new ObjectId(new MObjectId())
  }

}
*/