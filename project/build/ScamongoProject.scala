import sbt._

class ScamongoProject(info: ProjectInfo) extends DefaultProject(info) {
	override def useMavenConfigurations = true
	
	val liftjson = "net.liftweb" % "lift-json" % "1.1-SNAPSHOT" % "compile->default"
	val liftrecord = "net.liftweb" % "lift-record" % "1.1-SNAPSHOT" % "compile->default"

	val junit = "junit" % "junit" % "4.5"
  val specs = "org.scala-tools.testing" % "specs" % "1.5.0"
  
	// required because Ivy doesn't pull repositories from poms
	//val smackRepo = "m2-repository-smack" at "http://maven.reucon.com/public"

	// other repositories
	val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"
}
