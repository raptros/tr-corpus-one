import com.github.retronym.SbtOneJar
import sbt._
import Keys._

object TaccHadoopBuild extends Build {
  def standardSettings = Seq(
    exportJars := true
  ) ++ Defaults.defaultSettings

  lazy val main = Project("tr-corpus-one", file("."))

}

