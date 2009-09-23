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

import net.liftweb.util.{Box, Full}
import net.liftweb.record.{MetaRecord, Record}
import net.liftweb.record.field.StringField

import com.mongodb.{BasicDBObject, DBObject, DBRefBase}

trait MongoRecord[MyType <: MongoRecord[MyType]] extends Record[MyType] {
	self: MyType =>
	
	/*
	* every mongo record must have an _id field. Override this with the value of your _id object.
	*/
  def id: Any

	/**
	* Was this instance deleted from backing store?
	*/
	private var was_deleted_? = false

	/**
	* The meta record (the object that contains the meta result for this type)
	*/
	def meta: MongoMetaRecord[MyType]

	/**
	* Save the instance and return the instance
	*/
	def save(): MyType = {
		
		runSafe {
			meta.save(this)
		}
		this
	}

	/**
	* Delete the instance from backing store
	*/
	def delete_! : Boolean = {
		if (!can_delete_?) false else
		runSafe {
			was_deleted_? = meta.delete_!(this)
			was_deleted_?
		}
	}

	/**
	* Can this model object be deleted?
	*/
	def can_delete_? : Boolean =  meta.saved_?(this) && !was_deleted_?

/* Set mongoIdentifier in meta object. No need to support calcDbId for sharding
	private var dbMongoIdentifier: Option[MongoIdentifier] = None

	def mongoIdentifier = dbMongoIdentifier getOrElse calcDbId

	def dbCalculateMongoIdentifier: PartialFunction[MyType, MongoIdentifier] = Map.empty

	private def calcDbId = if (dbCalculateMongoIdentifier.isDefinedAt(this)) dbCalculateMongoIdentifier(this)
												else meta.dbDefaultMongoIdentifier
*/

	/**
	* Append a function to perform after the commit happens
	* @param func - the function to perform after the commit happens
	
	def doPostCommit(func: () => Unit) {
		//DB.appendPostFunc(connectionIdentifier, func)
	}
*/

	/**
	* Populate this record's fields with the values from a DBObject
	*
	* @param obj - The DBobject
	*/
	def fromDBObject(obj: DBObject): Box[MyType] = {
		meta.fromDBObject(this, obj)
	}
}

/**
* Mix this into a Record to add a MongoIdField
*/
trait MongoId[OwnerType <: MongoRecord[OwnerType]] {
  //self: OwnerType =>
  
  import field.MongoIdField
  
	object _id extends MongoIdField(this.asInstanceOf[OwnerType])

  // convenience method that returns the value of _id
  def id = _id.value
  
  //def getRefBase: DBRefBase = _id.getRefBase
  def getRef: DBRef = _id.getRef

  //def getDbRef = new DBRefBase(owner.meta.getDb, owner.meta.collectionName, value)
  //def getRef = MongoDbRef(this.asInstanceOf[OwnerType].owner.meta.getDb.toString, owner.meta.collectionName, this.asInstanceOf[OwnerType].value)
}

/*
* Need a way to map collectionName to Record class
*/
case class DBRef(ref: String, id: String)
