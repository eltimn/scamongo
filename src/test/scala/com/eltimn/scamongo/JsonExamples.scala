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

import java.util.Date
import java.util.regex.Pattern

import org.specs.Specification
import org.specs.runner.{Runner, JUnit}

import net.liftweb.json.DefaultFormats
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonParser._
import net.liftweb.json.JsonDSL._

import com.mongodb.ObjectId

//class JsonExampleTest extends Runner(Examples) with JUnit

object JsonExamples extends Specification {

	doFirst { /** user-defined setup function */ }

	import com.mongodb.util.JSON // Mongo parser/serializer

	val debug = false

	def date(s: String) = MongoFormats.dateFormat.parse(s).get

	/*
	* MongoIdentifiers
	*/
	object TestDBa extends MongoIdentifier {
		val jndiName = "test_a"
	}
	object TestDBb extends MongoIdentifier {
		val jndiName = "test_b"
	}

	// create the Mongo instances
	MongoDB.defineMongo(DefaultMongoIdentifier, new MongoAddress("localhost", 27017, "test"))
	MongoDB.defineMongo(TestDBa, new MongoAddress("localhost", 27017, "test_a"))
	MongoDB.defineMongo(TestDBb, new MongoAddress("localhost", 27017, "test_b"))

	val primitives = compact(render({
		("str" -> "109") ~
		("int" -> 2147483647) ~
		("lng" -> 9223372036854775807L) ~
		("bool" -> true) ~
		("dbl" -> 127.5) ~
		("null" -> null)
	}))

	"Simple Person example" in {

		// create a new SimplePerson
		val pid = MongoHelpers.newId
		val p = SimplePerson(pid.toString, "Tim", 38)

		// save it
		p.save

		// retrieve it
		def pFromDb = SimplePerson.find(pid.toString)

		pFromDb.isDefined must_== true

		p mustEqual pFromDb.get

		// retrieve it using a Json query
		def pFromDbViaJson = SimplePerson.find(("_id" -> p._id))

		pFromDbViaJson.isDefined must_== true

		p mustEqual pFromDbViaJson.get

		// retrieve it using a String, Any
		def pFromDbViaString = SimplePerson.find("_id", p._id)

		pFromDbViaString.isDefined must_== true

		p must_== pFromDbViaString.get

		// modify the person
		val p2 = SimplePerson(p._id, "Timm", 27)
		p2.save

		pFromDb.isDefined must_== true

		p2 must_== pFromDb.get
		p2.name must_== pFromDb.get.name

		// find all documents
		val all = SimplePerson.findAll

		all.isEmpty must_== false

		if (!debug) {
			all.size must_== 1
			all.first must_== p2

			// delete it
			p2.delete

			pFromDb.isEmpty must_== true
			pFromDbViaJson.isEmpty must_== true
			pFromDbViaString.isEmpty must_== true
		}

  }

  "Multiple Simple Person example" in {

		// create new SimplePersons
		val p = SimplePerson(MongoHelpers.newId.toString, "Jill", 27)
		val p2 = SimplePerson(MongoHelpers.newId.toString, "Bob", 25)
		val p3 = SimplePerson(MongoHelpers.newId.toString, "Bob", 29)

		//p mustNotEqual p2

		// save them
		p.save
		p2.save
		p3.save

		// retrieve them
		def pFromDb = SimplePerson.find("_id", p._id)
		def p2FromDb = SimplePerson.find("_id", p2._id)
		def p3FromDb = SimplePerson.find(("_id" -> p3._id))

		pFromDb.isDefined must_== true
		p2FromDb.isDefined must_== true
		p3FromDb.isDefined must_== true

		p mustEqual pFromDb.get
		p2 mustEqual p2FromDb.get
		p3 mustEqual p3FromDb.get

		// find all persons named 'Bob'
		val allBobs = SimplePerson.findAll(("name" -> "Bob"))
		val allBobs2 = SimplePerson.findAll("name", "Bob")

		allBobs.isEmpty must_== false
		allBobs2.isEmpty must_== false

		if (!debug) {
			allBobs.size must_== 2
			allBobs2.size must_== 2

			// delete them
			p.delete
			p2.delete
			p3.delete

			pFromDb.isEmpty must_== true
			p2FromDb.isEmpty must_== true
			p3FromDb.isEmpty must_== true
		}

  }

  "Person example" in {

		// create a new Person
		val p = Person(MongoHelpers.newUUID.toString, "joe", 27, Address("Bulevard", "Helsinki"), List(Child("Mary", 5, Some(date("2004-09-04T18:06:22Z"))), Child("Mazy", 3, None)))

		//println(p.children.first.birthdate.toString)

		// save it
		p.save

		// retrieve it
		def pFromDb = Person.find("_id", p._id)

		pFromDb.isDefined must_== true

		p mustEqual pFromDb.get

		if (!debug) {
			// delete it
			p.delete

			pFromDb.isEmpty must_== true
		}
  }

  "Mongo tutorial example" in {

  	// build a TestCollection
  	val info = TCInfo(203, 102)
  	val tc = TestCollection(MongoHelpers.newId.toString, "MongoDB", "database", 1, info)
  	val tc2 = TestCollection(MongoHelpers.newId.toString, "OtherDB", "database", 1, info)

  	// save to db
  	tc.save
  	tc2.save

  	// get the doc back from the db
		def tcFromDb = TestCollection.find(tc._id)
		def tc2FromDb = TestCollection.find(tc2._id)

		tcFromDb.isDefined must_== true
		tcFromDb.get must_== tc
		tc2FromDb.isDefined must_== true
		tc2FromDb.get must_== tc2

		// upsert
		val tc3 = TestCollection(tc._id, "MongoDB", "document", 2, info)
		val q = ("name" -> "MongoDB") // the query to select the document(s) to update
		val o = TestCollection.toJson(tc3)  // the new object to update with, replaces the entire document, except possibly _id
		TestCollection.update(q, o)

		// get the doc back from the db and compare
		tcFromDb.isDefined must_== true
		tcFromDb.get must_== tc3

		// modifier operations $inc, $set, $push...
		val o2 = (("$inc" -> ("count" -> 1)) ~ ("$set" -> ("dbtype" -> "docdb")))
		TestCollection.update(q, o2)

		// get the doc back from the db and compare
		tcFromDb.isDefined must_== true
		tcFromDb.get must_== TestCollection(tc._id, tc.name, "docdb", 3, info)

		// this one should insert a new row
		val o3 = (("$inc" -> ("count" -> 1)) ~ ("$set" -> ("dbtype" -> "docdb")))
		TestCollection.update(("name" -> "nothing"), o3, ("upsert", true), ("apply" -> true))

		TestCollection.findAll.length must_== 2

		if (!debug) {
			// delete them
			tc.delete
			tc2.delete
		}

		TestCollection.findAll.size must_== 0

		// insert multiple documents
		for (i <- List.range(1, 101)) {
			IDoc(MongoHelpers.newId.toString, i).save
    }

		// count the docs
		IDoc.count must_== 100

		// get the count using a query
		IDoc.count(("i" -> ("$gt" -> 50))) must_== 50

		// get a List of all documents
		val all = IDoc.findAll
		for (d <- all) {
			if (debug) println(d.toString)
		}
		all.length must_== 100

		// get a single document with a query ( i = 71 )
		val doc = IDoc.find(("i" -> 71))

		doc.isDefined must_== true
		doc.get.i must_== 71

		// get a set of documents with a query
		// e.g. find all where i > 50
		val list1 = IDoc.findAll(("i" -> ("$gt" -> 50)))

		list1.length must_== 50

		// range - 20 < i <= 30
		val list2 = IDoc.findAll(("i" -> ("$gt" -> 20) ~ ("$lte" -> 30)))

		list2.length must_== 10

		// limiting result set
		val list3 = IDoc.findAll(("i" -> ("$gt" -> 50)), 3)

		list3.length must_== 3

		// skip
		val list4 = IDoc.findAll(("i" -> ("$gt" -> 50)), 0, 10)
		var cntr4 = 0
		for (idoc <- list4) {
			cntr4 += 1
			idoc.i must_== 60+cntr4
		}
		list4.length must_== 40

		// skip and limit (get first 10, skipping the first 5, where i > 50)
		val list5 = IDoc.findAll(("i" -> ("$gt" -> 50)), 10, 5)
		var cntr5 = 0
		for (idoc <- list5) {
			cntr5 += 1
			idoc.i must_== 55+cntr5
		}
		list5.length must_== 10

		// sorting (it's also easy to sort the List after it's returned)
		val list6 = IDoc.findAll(("i" -> ("$gt" -> 0)), ("i" -> -1)) // descending
		var cntr6 = 100
		for (idoc <- list6) {
			idoc.i must_== cntr6
			cntr6 -= 1
		}
		list6.length must_== 100

		// remove some docs by a query
		IDoc.delete(("i" -> ("$gt" -> 50)))

		IDoc.findAll.length must_== 50
  }

  "Mongo useSession example" in {

  	val info = TCInfo(203, 102)
		val tc = TestCollection(MongoHelpers.newId.toString, "MongoSession", "db", 1, info)
		val tc2 = TestCollection(MongoHelpers.newId.toString, "MongoSession", "db", 1, info)
		val tc3 = TestCollection(MongoHelpers.newId.toString, "MongoDB", "db", 1, info)

  	// use a Mongo instance directly with a session
  	MongoDB.useSession(DefaultMongoIdentifier) ( db => {
			//val coll = db.getCollection("testCollection")

			// save to db
			TestCollection.save(tc, db)
			db.getLastError.get("err") must beNull
			TestCollection.save(tc2, db) // this should return an error
			db.getLastError.get("err").toString must startWith("E11000 duplicate key errorindex")
			TestCollection.save(tc3, db)
			db.getLastError.get("err") must beNull

			// query for the docs by type
			val qry = ("dbtype" -> "db")
			TestCollection.findAll(qry).size must_== 2

			// modifier operations $inc, $set, $push...
			val o2 = ("$inc" -> ("count" -> 1)) // increment count by 1
			//("$set" -> ("dbtype" -> "docdb")) // set dbtype
			TestCollection.update(qry, o2, db)
			db.getLastError.get("updatedExisting") must_== true
			/* The update method only updates one document. see:
			http://jira.mongodb.org/browse/SERVER-268
			*/
			db.getLastError.get("n") must_== 1

			// try updating against the unique key
			val o3 = ("$set" -> ("name" -> "MongoDB")) // set name
			TestCollection.update(qry, o3, db, ("upsert", true))
			db.getLastError.get("err").toString must startWith("E12011 can't $inc/$set an indexed field")
			db.getLastError.get("n") must_== 0

			// regex query example
			val key = "name"
			val regex = "^Mongo"
			val dbo = MongoHelpers.dbObjectBuilder.add(key, Pattern.compile(regex)).get
			val lst = TestCollection.findAll(dbo, 0, 0, None)
			lst.size must_== 2

			// use regex and another clause
			val dbo2 = MongoHelpers.dbObjectBuilder.add(key, Pattern.compile(regex)).add("count", 1).get
			val lst2 = TestCollection.findAll(dbo2, 0, 0, None)
			lst2.size must_== 1

			if (!debug) {
				// delete them
				TestCollection.delete(qry)
				db.getLastError.get("n") must_== 2
				TestCollection.findAll.size must_== 0
			}

		})
  }

  doLast {
  	if (!debug) {
			/** drop the collections */
			MongoDB.useCollection(SimplePerson.mongoIdentifier, SimplePerson.collectionName) ( coll => {
				coll.drop
			})
			MongoDB.useCollection(Person.mongoIdentifier, Person.collectionName) ( coll => {
				coll.drop
			})
			MongoDB.useCollection(TestCollection.mongoIdentifier, TestCollection.collectionName) ( coll => {
				coll.drop
			})
			MongoDB.useCollection(IDoc.mongoIdentifier, IDoc.collectionName) ( coll => {
				coll.drop
			})
		}
  }
}

case class SimplePerson(_id: String, name: String, age: Int) extends MongoDocument[SimplePerson] {
	def meta = SimplePerson
}

object SimplePerson extends MongoDocumentMeta[SimplePerson] {

	override val collectionName = "simplepersons"

	override def mongoIdentifier = DefaultMongoIdentifier

	def fromJson(in: JValue): SimplePerson = {
		implicit val formats = MongoFormats
		in.extract[SimplePerson]
	}

	def toJson(per: SimplePerson): JValue = {
		("_id" -> per._id) ~ ("name" -> per.name) ~ ("age" -> per.age)
	}

}

case class Address(street: String, city: String)
case class Child(name: String, age: Int, birthdate: Option[Date])

case class Person(_id: String, name: String, age: Int, address: Address, children: List[Child])
	extends MongoDocument[Person] {

	def meta = Person
}

object Person extends MongoDocumentMeta[Person] {

	implicit val formats = MongoFormats

	def fromJson(in: JValue): Person = {
		in.extract[Person]
	}

	def toJson(per: Person): JValue = {
		("_id" -> per._id) ~
		("name" -> per.name) ~
		("age" -> per.age) ~
		("address" -> (("street" -> per.address.street) ~ ("city" -> per.address.city))) ~
		("children" ->
			per.children.map { c =>
				(
					("name" -> c.name) ~
					("age" -> c.age) ~
					("birthdate" -> c.birthdate.map(formats.dateFormat.format(_)))
				)
			}
		)
	}

}

// Mongo tutorial classes
case class TCInfo(x: Int, y: Int)
case class TestCollection(_id: String, name: String, dbtype: String, count: Int, info: TCInfo)
	extends MongoDocument[TestCollection] {

	def meta = TestCollection
}

object TestCollection extends MongoDocumentMeta[TestCollection] {

	implicit val formats = MongoFormats

	// create a unique index on name
	ensureIndex(("name" -> 1), ("unique", true))

	def fromJson(in: JValue): TestCollection = {
		in.extract[TestCollection]
	}

	def toJson(tc: TestCollection): JValue = {
		("_id" -> tc._id) ~
		("name" -> tc.name) ~
		("dbtype" -> tc.dbtype) ~
		("count" -> tc.count) ~
		("info" -> (("x" -> tc.info.x) ~ ("y" -> tc.info.y)))
	}

}

case class IDoc(_id: String, i: Int) extends MongoDocument[IDoc] {

	def meta = IDoc
}

object IDoc extends MongoDocumentMeta[IDoc] {

	implicit val formats = MongoFormats

	// create an index on "i", ascending
	ensureIndex(("i" -> 1))

	def fromJson(in: JValue): IDoc = {
		in.extract[IDoc]
	}

	def toJson(tc: IDoc): JValue = {
		("_id" -> tc._id) ~
		("i" -> tc.i)
	}

}
