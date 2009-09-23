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

import net.liftweb.json.JsonDSL._
import net.liftweb.json.JsonAST.JObject
import net.liftweb.record.field._
import net.liftweb.util.{Box, Full}

import org.specs.Specification
import org.specs.runner.{Runner, JUnit}

import com.mongodb.{BasicDBObject, BasicDBList, ObjectId}
import com.mongodb.util.JSON

import field._

//class RecordExampleTest extends Runner(Examples) with JUnit
object RecordExamples extends Specification {

	import net.liftweb.util.TimeHelpers._

	val debug = false

	doFirst { // create the Mongo instance
		MongoDB.defineMongo(DefaultMongoIdentifier, new MongoAddress("localhost", 27017, "test"))
	}

	"TestRecord example" in {
    val tr = TestRecord.createRecord
    tr.stringfield.set("test record string field")
    tr.emailfield.set("test")
    tr.validate.size must_== 2
    tr.passwordfield.set("test")
    tr.emailfield.set("test@example.com")
    tr.validate.size must_== 0
    val newId = tr.id

    val per = RPerson("joe", 27, Address("Bulevard", "Helsinki"), List(Child("Mary", 5, Some(now)), Child("Mazy", 3, None)))

    tr.person.set(per.asJObject)

    tr.id mustEqual newId

		// save the person in the db
		tr.save

		// retrieve from db
		def fromDb = TestRecord.find("_id", newId)

		fromDb.isDefined must_== true

		for (t <- fromDb) {
			t._id.value must_== tr._id.value
			t.booleanfield.value must_== tr.booleanfield.value
			MongoFormats.dateFormat.format(t.datetimefield.value.getTime) must_==
			MongoFormats.dateFormat.format(tr.datetimefield.value.getTime)
			t.doublefield.value must_== tr.doublefield.value
			t.intfield.value must_== tr.intfield.value
			t.localefield.value must_== tr.localefield.value
			t.longfield.value must_== tr.longfield.value
			//t.passwordfield.value must_== tr.passwordfield.value
			t.stringfield.value must_== tr.stringfield.value
			t.timezonefield.value must_== tr.timezonefield.value
			val p = RPerson.create(t.person.value)
			p.name must_== per.name
			p.age must_== per.age
			p.address must_== per.address
			p.children.size must_== per.children.size
			for (i <- List.range(0, p.children.size-1)) {
				p.children(i).name must_== per.children(i).name
				p.children(i).age must_== per.children(i).age
				MongoFormats.dateFormat.format(p.children(i).birthdate.get) must_==
				MongoFormats.dateFormat.format(per.children(i).birthdate.get)
			}
		}
  }

  "Ref example" in {

  	val ref1 = RefDoc.createRecord
  	val ref2 = RefDoc.createRecord

  	ref1.save must_== ref1
  	ref2.save must_== ref2

  	val md1 = MainDoc.createRecord
  	val md2 = MainDoc.createRecord
  	val md3 = MainDoc.createRecord
  	val md4 = MainDoc.createRecord

  	md1.name.set("md1")
  	md2.name.set("md2")
  	md3.name.set("md3")
  	md4.name.set("md4")

  	md1.refdoc.set(ref1.getRef)
  	md2.refdoc.set(ref1.getRef)
  	md3.refdoc.set(ref2.getRef)
  	md4.refdoc.set(ref2.getRef)

  	md1.save must_== md1
  	md2.save must_== md2
  	md3.save must_== md3
  	md4.save must_== md4

  	MainDoc.count must_== 4
  	RefDoc.count must_== 2

  	// fetch a refdoc
  	val refFromFetch = md1.refdoc.fetch
  	refFromFetch.isDefined must_== true
		refFromFetch.open_!.id must_== ref1.id

		// query for a single doc with a JObject query
		val md1a = MainDoc.find(("name") -> "md1")
		md1a.isDefined must_== true
		md1a.foreach(o => o.id must_== md1.id)

		// query for a single doc with a k, v query
		val md1b = MainDoc.find("_id", md1.id)
		md1b.isDefined must_== true
		md1b.foreach(o => o.id must_== md1.id)

		// find all documents
		MainDoc.findAll.size must_== 4
		RefDoc.findAll.size must_== 2

		// find all documents with JObject query
		val mdq1 = MainDoc.findAll(("name" -> "md1"))
		mdq1.size must_== 1

		// find all documents with $in query, sorted
		val names = new BasicDBList
		names.add("md1")
		names.add("md2")
		val qry = new BasicDBObject("name", new BasicDBObject("$in", names))
		val mdq2 = MainDoc.findAll(qry, ("name" -> -1))
		mdq2.size must_== 2
		mdq2.first.id must_== md2.id

		// Find all documents using a k, v query
		val mdq3 = MainDoc.findAll("_id", md1.id)
		mdq3.size must_== 1


  }

  doLast {
  	if (!debug) {
			/** drop the collections */
			TestRecord.drop
			MainDoc.drop
  		RefDoc.drop
  	}

  	// clear the mongo instances
		MongoDB.close
	}
}

class TestRecord extends MongoRecord[TestRecord] {

	//import MongoHelpers._

	def meta = TestRecord

	def id = _id.value

	object _id extends StringField(this, 24) {
		override def defaultValue = MongoHelpers.newUUID
	}

	//object binaryfield extends BinaryField(this)
	object booleanfield	extends BooleanField(this)
	//object countryfield extends CountryField(this)
	object datetimefield extends DateTimeField(this)
	//object decimalfield extends DecimalField(this)
	object doublefield extends DoubleField(this)
	object emailfield extends EmailField(this, 220)
	//object enumfield extends EnumField(this)
	object intfield extends IntField(this)
	object localefield extends LocaleField(this)
	object longfield extends LongField(this)
	object passwordfield extends PasswordField(this)
	//object postalcodefield extends PostalCodeField(this, countryfield)
	object stringfield extends StringField(this, 32)
	//object textareafield extends TextareaField(this, 200)
	object timezonefield extends TimeZoneField(this)

	// JObjectField
	object person extends JObjectField(this)

	/* CaseClassField
	object person2 extends CaseClassField[TestRecord, RPerson](this) {

		def defaultValue = RPerson("", 0, Address("", ""), List())

		def setFromAny(in: Any): Box[RPerson] = in match {
			case jv: CaseType => Full(set(jv))
			case Some(jv: CaseType) => Full(set(jv))
		  case Full(jv: CaseType) => Full(set(jv))
		  //case dbo: DBObject => Full(set(dbObjectToJObject(dbo)))
		  case seq: Seq[_] if !seq.isEmpty => seq.map(setFromAny)(0)
		  case (s: String) :: _ => setFromString(s)
		  case null => Full(set(null))
		  case s: String => setFromString(s)
		  case None | Empty | Failure(_, _, _) => Full(set(null))
		  case o => setFromString(o.toString)
		}

		def setFromString(in: String): Box[RPerson] = {
			Full(set(RPerson.fromJObject(dbObjectToJObject(JSON.parse(in)))))
		}
	}
	*/
}

object TestRecord extends TestRecord with MongoMetaRecord[TestRecord]

case class RPerson(name: String, age: Int, address: Address, children: List[Child])
	extends JsonObject[RPerson] {
	def meta = RPerson
}

object RPerson extends JsonObjectMeta[RPerson]

class MainDoc extends MongoRecord[MainDoc] with MongoId[MainDoc] {

	def meta = MainDoc

	//import field._
	//object _id extends MongoIdField(this)
	object name extends StringField(this, 12)

	object refdoc extends MongoRefField(this) {

		def fetch = {
			RefDoc.find(new ObjectId(value.id))
		}
	}
}

object MainDoc extends MainDoc with MongoMetaRecord[MainDoc] {
	//override def mongoIdentifier = TestDBb
	//override def collectionName = "mymaindocs"
}

class RefDoc extends MongoRecord[RefDoc] with MongoId[RefDoc] {
	def meta = RefDoc
}

object RefDoc extends RefDoc with MongoMetaRecord[RefDoc]

