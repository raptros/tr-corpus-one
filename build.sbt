import AssemblyKeys._

assemblySettings

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

name := "tr-corpus-one"

version := "0.1"

scalaVersion := "2.9.2"

libraryDependencies += "com.nicta" %% "scoobi" % "0.6.0-cdh3"

resolvers ++= Seq(
  "Sonatype-snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "cloudera" at "https://repository.cloudera.com/content/repositories/releases",
  "apache" at "https://repository.apache.org/content/repositories/releases",
  "scoobi" at "http://nicta.github.com/scoobi/releases",
  "gwtwiki" at "http://gwtwiki.googlecode.com/svn/maven-repository/"
)

libraryDependencies ++= Seq(
  "edu.umd" % "cloud9" % "1.3.5",
  "info.bliki.wiki" % "bliki-core" % "3.0.16",
  "commons-lang" % "commons-lang" % "2.6"
)

resolvers += "Virtual-Void repository" at "http://mvn.virtual-void.net"

addCompilerPlugin("net.virtualvoid" % "scala-enhanced-strings_2.9.1" % "0.5.2")

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.0-M3"

scalacOptions ++= Seq("-deprecation", "-unchecked")

jarName in assembly := "tacc-hadoop-assembly.jar"

mainClass in assembly := None

test in assembly := {}

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case x => {
      val oldstrat = old(x)
      if (oldstrat == MergeStrategy.deduplicate) MergeStrategy.first
      else oldstrat
    }
  }
}

mainClass in oneJar := None
