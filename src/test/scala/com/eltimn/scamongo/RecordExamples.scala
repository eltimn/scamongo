package com.eltimn.scamongo

import net.liftweb.record.field._

import org.specs.Specification
import org.specs.runner.{Runner, JUnit}

//class RecordExampleTest extends Runner(Examples) with JUnit
object RecordExamples extends Specification {
	
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


	"Person example" in {
    val newPerson = RPerson.createRecord
    newPerson.name.set("Tim")
    val newId = newPerson.id
    
    newPerson.id mustEqual newId

		// save the person in the db
		newPerson.save
    
  }
}

class RPerson extends MongoRecord[RPerson] {
	def meta = RPerson

	object name extends StringField(this, 32)
}

object RPerson extends RPerson with MongoMetaRecord[RPerson]
