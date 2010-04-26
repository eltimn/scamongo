import sbt._

class ScamongoProject(info: ProjectInfo) extends DefaultProject(info) {
	
	//override def compileOptions = super.compileOptions ++ Seq(Unchecked)
	
	val mongo = "org.mongodb" % "mongo-java-driver" % "1.4" % "compile->default"

	val liftjson = "net.liftweb" % "lift-json" % "2.0-M4" % "compile->default"
	val liftrecord = "net.liftweb" % "lift-record" % "2.0-M4" % "compile->default"

	val junit = "junit" % "junit" % "4.7"
  val specs = "org.scala-tools.testing" % "specs" % "1.6.1"

	// other repositories
	//val scalaToolsSnapshots = ScalaToolsSnapshots
}
