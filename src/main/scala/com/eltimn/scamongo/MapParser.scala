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

import java.util.Calendar

import com.mongodb.{BasicDBObjectBuilder, BasicDBList, DBObject}

private[scamongo] object MapParser {
	
	import net.liftweb.json.Formats

	/*
	* Parse a Map[String, Any] into a DBObject
	*/
	def parse(map: Map[String, Any]): DBObject = {
		Parser.parse(map)
  }

	object Parser {
	
		import Meta.Reflection._

		def parse(map: Map[String, Any]): DBObject = {

			val dbo = BasicDBObjectBuilder.start

			map.keys.foreach {
				k => map(k).asInstanceOf[AnyRef] match {
					case x if primitive_?(x.getClass) => dbo.add(k, x)
					case x if datetype_?(x.getClass) => dbo.add(k, datetype2dbovalue(x))
					case x if mongotype_?(x.getClass) => dbo.add(k, mongotype2dbovalue(x))
					case m: Map[String, Any] => dbo.add(k, parse(m))
					case l: List[Any] => dbo.add(k, parseList(l))
					case o => dbo.add(k, o.toString)
				}
			}
			dbo.get
		}

		private def parseList(list: List[Any]): DBObject = {
			val dbl = new BasicDBList

			list.foreach { i => i.asInstanceOf[AnyRef] match {
				case x if primitive_?(x.getClass) => dbl.add(x)
				case x if datetype_?(x.getClass) => dbl.add(datetype2dbovalue(x))
				case x if mongotype_?(x.getClass) => dbl.add(mongotype2dbovalue(x))
				case m: Map[String, Any] => dbl.add(parse(m))
				case l: List[Any] => dbl.add(parseList(l))
				case o => dbl.add(o.toString)
			}}
			dbl
		}
  }
}
