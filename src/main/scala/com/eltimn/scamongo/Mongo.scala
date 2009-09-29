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
* TODO: Replace ListBuffers
* Record
* TODO: String validators & LocaleField.localeList
* TODO: callbacks, before/after update, before/after insert
* TODO: save, saved_?, runSafe, try-catch
* TODO: Map serializer
* TODO: ListField (Map serializer, JsonObjectListField)
* TODO: saveSafe, updateSafe methods
* TODO: useSession example
* TODO: PasswordField
* TODO: MapField (Map serializer)
* TODO: OptionField (or Box)
* TODO: CaseClassField
* TODO: MongoRefField fetch
* TODO: DBRefBase ($ - problems)
* TODO: Calendar ??
* Document
* TODO: save, update safe. Return Option.
* General
* TODO: eval ?
* TODO: query DSL: By, In, >=, etc.
* TODO: Boolean, ObjectId, Date, Map query examples 
* TODO: master/slave, replication
* TODO: MongoAuth example
* TODO: Test all data types
* TODO: MongoAdmin ?
* TODO: Binary support
*/

import scala.collection.immutable.HashSet
import scala.collection.mutable.{HashMap => MutableHashMap, ListBuffer}
import scala.reflect.Manifest

import net.liftweb.json.{DateFormat, DefaultFormats, Formats}
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
case class MongoAddress(host: String, port: Int, name: String) {

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
  def use[T](f: (DBBase) => T): T = {

  	val db = getMongo(DefaultMongoIdentifier) match {
			case Some(mongo) => mongo
			case _ => throw new MongoException("Mongo not found: "+DefaultMongoIdentifier.toString)
		}

		f(db)
  }
  
  /**
  * Executes function {@code f} with the mongo admin named {@code name}.
  */
  def useAdmin[T](name: MongoIdentifier)(f: (MongoAdmin) => T): T = {

  	val dba = getMongo(name) match {
			case Some(mongo) => new MongoAdmin(mongo.getAddress)
			case _ => throw new MongoException("Mongo not found: "+name.toString)
		}

		f(dba)
  }

  /**
  * Executes function {@code f} with the default mongoIdentifier
  */
  def useAdmin[T](f: (MongoAdmin) => T): T = {

  	val dba = getMongo(DefaultMongoIdentifier) match {
			case Some(mongo) => new MongoAdmin(mongo.getAddress)
			case _ => throw new MongoException("Mongo not found: "+DefaultMongoIdentifier.toString)
		}

		f(dba)
  }

  /**
  * Executes function {@code f} with the mongo named {@code name} and collection names {@code collectionName}.
  * Gets a collection for you.
	*/
  def useCollection[T](name: MongoIdentifier, collectionName: String)(f: (DBCollection) => T): T = {
    val coll = getCollection(name, collectionName) match {
			case Some(collection) => collection
			case _ => throw new MongoException("Collection not found: "+collectionName+". MongoIdentifier: "+name.toString)
		}

		f(coll)
  }
  
  /**
  * Same as above except uses DefaultMongoIdentifier
	*/
  def useCollection[T](collectionName: String)(f: (DBCollection) => T): T = {
    val coll = getCollection(DefaultMongoIdentifier, collectionName) match {
			case Some(collection) => collection
			case _ => throw new MongoException("Collection not found: "+collectionName+". MongoIdentifier: "+DefaultMongoIdentifier.toString)
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
  
  /**
  * Same as above except uses DefaultMongoIdentifier
  */
  def useSession[T](f: (DBBase) => T): T = {

  	val db = getMongo(DefaultMongoIdentifier) match {
			case Some(mongo) => mongo
			case _ => throw new MongoException("Mongo not found: "+DefaultMongoIdentifier.toString)
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

/*
* The default Mongo Formats
* dates use UTC and this format

object MongoFormats extends DefaultFormats {
	import java.text.SimpleDateFormat
	override def dateFormatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
}
*/
/*
* Helper methods
*/
object MongoHelpers {

	import java.util.UUID

	/* ID helper methods */
  def newMongoId = ObjectId.get.toString
  def newUUID = UUID.randomUUID.toString
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
	def fixCollectionName = _collectionName.toLowerCase match {
		case name if (name.contains("$")) => name.replace("$", "_d_")
		case name => name
	}

	/**
	* The name of the database collection.  Override this method if you
	* want to change the collection to something other than the name of
	* the MongoDocument case class with an 's' appended to the end.
	*/
	//def collectionName = fixCollectionName(_collectionName)
	def collectionName: String = fixCollectionName

	// override this to specify a MongoIdentifier for this MongoDocumnet type
  //def mongoIdentifier: MongoIdentifier = DefaultMongoIdentifier
  def mongoIdentifier: MongoIdentifier = DefaultMongoIdentifier
  
  // override this for custom Formats (date)
  def formats: Formats = DefaultFormats //.lossless
	implicit lazy val _formats: Formats = formats

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
	* Count documents by JObject query
	*/
	def count(qry: JObject):Long = count(JObjectParser.parse(qry))

	/*
	* Count documents by Map query
	*/
	def count(qry: Map[String, Any]):Long = count(MapParser.parse(qry))

	// delete a document
	def delete(k: String, v: Any) {
		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll =>
			coll.remove(new BasicDBObject(k, v))
		)
	}

	/*
	* Delete documents by a JObject query
	*/
	def delete(qry: JObject) {
		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll =>
			coll.remove(JObjectParser.parse(qry))
		)
	}

	/*
	* Delete documents by a Map query
	*/
	def delete(qry: Map[String, Any]) {
		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll =>
			coll.remove(MapParser.parse(qry))
		)
	}

	/* drop this document collection */
	def drop {
		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll =>
			coll.drop
		)
	}

	/*
	* Ensure an index exists using a JObject
	*/
	def ensureIndex(keys: JObject, opts: IndexOption*) {
		val ixOpts = opts.toList
		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll => {
			coll.ensureIndex(JObjectParser.parse(keys),
				ixOpts.find(_ == Force).map(x => true).getOrElse(false),
				ixOpts.find(_ == Unique).map(x => true).getOrElse(false)
			)
		})
	}

	/*
	* Ensure an index exists using a JObject, giving it a name
	*/
	def ensureIndex(keys: JObject, name: String, opts: IndexOption*) {
		val ixOpts = opts.toList
		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll => {
			coll.ensureIndex(JObjectParser.parse(keys), name,
				ixOpts.find(_ == Unique).map(x => true).getOrElse(false)
			)
		})
	}

	/*
	* Ensure an index exists using a Map
	*/
	def ensureIndex(keys: Map[String, Any], opts: IndexOption*) {
		val ixOpts = opts.toList
		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll => {
			coll.ensureIndex(MapParser.parse(keys),
				ixOpts.find(_ == Force).map(x => true).getOrElse(false),
				ixOpts.find(_ == Unique).map(x => true).getOrElse(false)
			)
		})
	}

	/*
	* Ensure an index exists using a Map, giving it a name
	*/
	def ensureIndex(keys: Map[String, Any], name: String, opts: IndexOption*) {
		val ixOpts = opts.toList
		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll => {
			coll.ensureIndex(MapParser.parse(keys), name,
				ixOpts.find(_ == Unique).map(x => true).getOrElse(false)
			)
		})
	}

	/**
	* Find all rows using a DBObject query. Internal use only.
	*/
	def getCursor(qry: DBObject, sort: Option[DBObject], opts: FindOption*): DBCursor = {
		val findOpts = opts.toList

		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll => {
			val cur = coll.find(qry).limit(
				findOpts.find(_.isInstanceOf[Limit]).map(x => x.value).getOrElse(0)
			).skip(
				findOpts.find(_.isInstanceOf[Skip]).map(x => x.value).getOrElse(0)
			)
			sort.foreach( s => cur.sort(s))
			cur
		})
	}

	/*
	* Update document with a DBObject query using the given Mongo instance.
	*/
	def update(qry: DBObject, newobj: DBObject, db: DBBase, opts: UpdateOption*): DBObject = {
		val dboOpts = opts.toList
		db.getCollection(collectionName).update(
			qry,
			newobj,
			dboOpts.find(_ == Upsert).map(x => true).getOrElse(false),
			dboOpts.find(_ == Apply).map(x => true).getOrElse(false)
		)
	}

	/*
	* Update document with a JObject query using the given Mongo instance.
	* For use with modifier operations $inc, $set, $push...
	*/
	def update(qry: JObject, newobj: JObject, db: DBBase, opts: UpdateOption*): JObject = {
		// these updates return the modifier object, not the object that was updated.
		JObjectParser.serialize(update(
			JObjectParser.parse(qry),
			JObjectParser.parse(newobj),
			db,
			opts :_*
		)).asInstanceOf[JObject]
	}

	/*
	* Update document with a JObject query. For use with modifier operations $inc, $set, $push...
	*/
	def update(qry: JObject, newobj: JObject, opts: UpdateOption*): JObject = {
		MongoDB.use(mongoIdentifier) ( db => {
			update(qry, newobj, db, opts :_*)
		})
	}
	
	/*
	* Update document with a Map query. For use with modifier operations $inc, $set, $push...
	*/
	def update(qry: Map[String, Any], newobj: Map[String, Any], db: DBBase, opts: UpdateOption*): DBObject = {
		// these updates return the modifier object, not the object that was updated.
		update(
			MapParser.parse(qry),
			MapParser.parse(newobj),
			db,
			opts :_*
		)
	}
	
	/*
	* Update document with a Map query. For use with modifier operations $inc, $set, $push...
	*/
	def update(qry: Map[String, Any], newobj: Map[String, Any], opts: UpdateOption*): DBObject = {
		MongoDB.use(mongoIdentifier) ( db => {
			update(qry, newobj, db, opts :_*)
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
	def asJObject()(implicit formats: Formats): JObject = meta.toJObject(this)

}

class JsonObjectMeta[BaseDocument](implicit mf: Manifest[BaseDocument]) {

	import net.liftweb.json.Extraction._
	
	/* override this for custom Formats (date)
  def joformats: Formats = DefaultFormats //.lossless
	private lazy val _joformats: Formats = joformats
	*/

	// create an instance of BaseDocument from a JObject
	def create(in: JObject)(implicit formats: Formats): BaseDocument =
		extract(in)(formats, mf)

	// convert class to a json object
	def toJObject(in: BaseDocument)(implicit formats: Formats): JObject =
		decompose(in)(formats).asInstanceOf[JObject]
}

/*
* Case class for a db reference (foreign key).
* ref = collection name, id is the value of the reference
*/
case class MongoRef(ref: String, id: String) {
	def objectId = new ObjectId(id)
}
/*extends JsonObject[DBRef]
object DBRef extends JsonObjectMeta*/

