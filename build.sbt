name := "tr-corpus-one"

version := "0.1"

scalaVersion := "2.10.1"

libraryDependencies += "com.nicta" %% "scoobi" % "0.7.0-RC1-cdh4"

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

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.0"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

