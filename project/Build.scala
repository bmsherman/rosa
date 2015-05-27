import sbt._
import Process._
import Keys._

object Leon extends Build {
  private val scriptName      = "leon"

  def scriptFile      = file(".") / scriptName

  def is64 = System.getProperty("sun.arch.data.model") == "64"
  def ldLibraryDir32 = file(".") / "lib-bin" / "32"
  def ldLibraryDir64 = file(".") / "lib-bin" / "64"

  val cleanTask = TaskKey[Unit]("cleanscript", "Cleans up the generated binaries and scripts.") <<= (streams, clean) map { (s,c) =>
    if(scriptFile.exists && scriptFile.isFile) {
      scriptFile.delete
    }
  }

  val nl = System.getProperty("line.separator")

  /*val setupScriptTask = TaskKey[Seq[String]]("setup", "Generate the " + setupScriptName + "  Bash script")

  val setupSetting = setupScriptTask <<= (streams, dependencyClasspath in Compile, classDirectory in Compile) map { (s, deps, out) =>

    val depsPaths = deps.map(_.data.absolutePath)

    println(depsPaths)
    val scalaHomeDir = depsPaths.find(_.endsWith("lib/scala-library.jar")) match {
      case None =>
	depsPaths.find(_.endsWith("jars/scala-library-2.10.2.jar")) match {
	      case None => throw new Exception("Couldn't guess SCALA_HOME.")
	      case Some(p) => p.substring(0, p.length - 29)
	    }
      case Some(p) => p.substring(0, p.length - 21)
    }*/
  val scriptTask = TaskKey[Unit]("script", "Generate the leon Bash script") <<= (streams, dependencyClasspath in Compile, classDirectory in Compile, resourceDirectory in Compile) map { (s, cps, out, res) =>
    try {
      val f = file("leon")
      // Paths discovery
      if(f.exists) {
        s.log.info("Regenerating '"+f.getName+"' script ("+(if(is64) "64b" else "32b")+")...")
        f.delete
      } else {
        s.log.info("Generating '"+f.getName+"' script ("+(if(is64) "64b" else "32b")+")...")
      }

      val paths = (res.getAbsolutePath +: out.getAbsolutePath +: cps.map(_.data.absolutePath)).mkString(":")

      IO.write(f, s"""|#!/bin/bash --posix
                      |
                      |SCALACLASSPATH="$paths"
                      |
                      |java -Xmx2G -Xms512M -classpath $${SCALACLASSPATH} -Dscala.usejavacp=false scala.tools.nsc.MainGenericRunner -classpath $${SCALACLASSPATH} leon.Main $$@ 2>&1 | tee last.log
                      |""".stripMargin)

      f.setExecutable(true)
    } catch {
      case e: Throwable =>
        s.log.error("There was an error while generating the script file: " + e.getLocalizedMessage)
    }
  }

  val sourceGen = {
    sourceGenerators in Compile += Def.task {
      val libFiles = ((baseDirectory.value / "library") ** "*.scala").getPaths.mkString("List(raw\"", "\", raw\"", "\")")

      val build = (sourceManaged in Compile).value / "leon" / "Build.scala";

      IO.write(build, s"""|package leon;
                          |
                          |object Build {
                          |val libFiles = $libFiles;
                          |}""".stripMargin)

      Seq(build)
    }.taskValue
  }

  object LeonProject {
    val settings = Seq(
      scriptTask,
      cleanTask,
      sourceGen
    )
  }

  lazy val root = Project(
    id = "leon",
    base = file("."),
    settings = Project.defaultSettings ++ LeonProject.settings
  ).dependsOn(Github.bonsai, Github.scalaSmtLib)

  object Github {
    def project(repo: String, version: String) = RootProject(uri(s"${repo}#${version}"))

    lazy val bonsai      = project("git://github.com/colder/bonsai.git",     "8f485605785bda98ac61885b0c8036133783290a")
    lazy val scalaSmtLib = project("git://github.com/regb/scala-smtlib.git", "1b85768a2a5384170a6d90f4aea56ca7330939fd")
  }
}
