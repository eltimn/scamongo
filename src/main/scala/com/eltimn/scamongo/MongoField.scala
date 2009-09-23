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

import scala.xml.{Node, NodeSeq, Text}

import net.liftweb.http.{S}
import net.liftweb.http.js.JE.Str
import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.JsonParser
import net.liftweb.record.{Field, Record}
import net.liftweb.util.{Box, Empty, Failure, FieldError, Full, Log}

import com.mongodb._
import com.mongodb.util.{JSON, JSONParseException}

/*
abstract class CaseClassField[OwnerType <: Record[OwnerType], CaseType](rec: OwnerType) extends Field[CaseType, OwnerType] {

	import MongoHelpers._

	def asJs = Str(toString)

	def asXHtml: NodeSeq = <div></div>

	//def defaultValue = JObject(List())

	/*
	def setFromAny(in: Any): Box[CaseType] = in match {
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
  */

	/*
	def setFromString(in: String): Box[CaseType] = {
		Full(set())
	}
	*/

	def toForm = <div></div>

	def owner = rec
}
*/



/**
 * Desribes common aspects related with Mongo fields
 */
trait MongoFieldFlavor[MyType] {

	// convert this field's value into a DBObject so it can be stored in Mongo.
  def toDBObject: DBObject

	// set this field's value using a DBObject returned from Mongo.
	def setFromDBObject(obj: DBObject): Box[MyType]

}


/**
 * List field. This version uses a Scala List
*/
abstract class MongoListField[OwnerType <: MongoRecord[OwnerType], ListType](rec: OwnerType)
	extends Field[List[ListType], OwnerType] with MongoFieldFlavor[List[ListType]] {

	def owner = rec

	def defaultValue = List[ListType]()

	def setFromAny(in: Any): Box[List[ListType]] = {
    in match {
    	case list: List[ListType] => Full(set(list))
    	case obj: DBObject => setFromDBObject(obj)
    	case seq: Seq[_] if !seq.isEmpty => seq.map(setFromAny)(0)
      case null => Full(set(null))
      case s: String => setFromString(s)
      case Some(list: List[ListType]) => Full(set(list))
      case Full(list: List[ListType]) => Full(set(list))
      case None | Empty | Failure(_, _, _) => Full(set(null))
      case o => setFromString(o.toString)
    }
  }

	// assume string is json
	def setFromString(s: String): Box[List[ListType]] = {
		setFromJson(s)
	}

	def setFromJson(s: String): Box[List[ListType]] = {
		setFromDBObject(JSON.parse(s))
	}

	private def elem = S.fmapFunc(S.SFuncHolder(this.setFromAny(_))) {
    funcName =>
    <input type="text"
      name={funcName}
      value={value match {case null => "" case s => s.toString}}
      tabindex={tabIndex toString}/>
  }

	def toForm: NodeSeq = <div></div> /*{
    uniqueFieldId match {
      case Full(id) =>
         <div id={id+"_holder"}><div><label for={id+"_field"}>{displayName}</label></div>{elem % ("id" -> (id+"_field"))}<lift:msg id={id}/></div>
      case _ => <div>{elem}</div>
    }

  }*/

  def asJs = Str(value.toString)

  def asXHtml: NodeSeq = <div></div> /*{
    var el = elem

    uniqueFieldId match {
      case Full(id) =>  el % ("id" -> (id+"_field"))
      case _ => el
    }
  }*/
}

/*
* Field represented as a Map

abstract class MongoMapField[OwnerType <: MongoRecord[OwnerType], MapKeyType, MapValueType](rec: OwnerType)
	extends Field[Map[MapKeyType, MapValueType], OwnerType] with MongoFieldFlavor[Map[MapKeyType, MapValueType]] {

	def owner = rec

	def defaultValue = Map[MapKeyType, MapValueType]()

	def setFromAny(in: Any): Box[Map[MapKeyType, MapValueType]] = {
    in match {
    	case map: Map[MapKeyType, MapValueType] => Full(set(map))
    	case obj: DBObject => setFromDBObject(obj)
    	case seq: Seq[_] if !seq.isEmpty => seq.map(setFromAny)(0)
      case (s: String) :: _ => setFromString(s)
      case null => Full(set(null))
      case s: String => setFromString(s)
      case Some(map: Map[MapKeyType, MapValueType]) => Full(set(map))
      case Full(map: Map[MapKeyType, MapValueType]) => Full(set(map))
      case None | Empty | Failure(_, _, _) => Full(set(null))
      case o => this.setFromString(o.toString)
    }
  }

	// assume string is json
	def setFromString(s: String): Box[Map[MapKeyType, MapValueType]] = {
		setFromJson(s)
	}

	def setFromJson(s: String): Box[Map[MapKeyType, MapValueType]] = {
		setFromDBObject(JSON.parse(s))
	}

	private def elem = S.fmapFunc(S.SFuncHolder(this.setFromAny(_))) {
    funcName =>
    <input type="text"
      name={funcName}
      value={value match {case null => "" case s => s.toString}}
      tabindex={tabIndex toString}/>
  }

	def toForm: NodeSeq = <div></div> /*{
    uniqueFieldId match {
      case Full(id) =>
         <div id={id+"_holder"}><div><label for={id+"_field"}>{displayName}</label></div>{elem % ("id" -> (id+"_field"))}<lift:msg id={id}/></div>
      case _ => <div>{elem}</div>
    }

  }*/

  def asJs = Str(value.toString)

  def asXHtml: NodeSeq = <div></div> /*{
    var el = elem

    uniqueFieldId match {
      case Full(id) =>  el % ("id" -> (id+"_field"))
      case _ => el
    }
  }*/
}
*/
