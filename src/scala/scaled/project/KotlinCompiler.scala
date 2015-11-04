//
// Scaled Kotlin Project Support - Kotlin project support for Scaled project framework.
// http://github.com/scaled/kotlin-project/blob/master/LICENSE

package scaled.project

import java.nio.file.{Files, Path}
import scaled._
import scaled.pacman.{Pacman, RepoId}
import scaled.util.{BufferBuilder, Chars, Errors, SubProcess}

object KotlinCompiler {
  // matches: "/foo/bar/baz.kt:LL:CC: some error message"
  val outputM = Matcher.regexp("""^([^:]+):(\d+):(\d+): (warning|error): (.*)""")

  /** The default version of kotlinc used if none is specified. */
  val DefaultKotlincVersion = "1.0.0-beta-1038"
}

abstract class KotlinCompiler (proj :Project) extends Compiler(proj) {
  import KotlinCompiler._

  /** The source directories. */
  def sourceDirs :Seq[Path]
  /** The build classpath. */
  def buildClasspath :Seq[Path]
  /** The directory in which classes will be written. */
  def outputDir :Path

  /** Options to pass to `javac`. */
  def javacOpts :Seq[String] = Seq()
  /** Options to pass to `kotlinc`. */
  def kotlincOpts :Seq[String] = Seq()
  /** The version of the Kotlin compiler to use. */
  def kotlincVers :String = DefaultKotlincVersion

  val log = proj.metaSvc.log
  // val compileSvc = proj.metaSvc.service[KotlinCompilerService]

  // override def reset () {} // NOOP!

  override def describeOptions (bb :BufferBuilder) {
    bb.addKeyValue("kotlinc: ", if (kotlincOpts.isEmpty) "<none>" else kotlincOpts.mkString(" "))
    bb.addKeyValue("kcvers: ", kotlincVers)
  }

  protected def compile (buffer :Buffer, file :Option[Path]) =
    compile(buffer, file, sourceDirs, buildClasspath, outputDir)

  /** A hook called just before we initiate compilation. */
  protected def willCompile () {}

  protected def compile (buffer :Buffer, file :Option[Path], sourceDirs :Seq[Path],
                         classpath :Seq[Path], output :Path) = {
    willCompile()

    // resolve the appropriate version of kotlinc
    val kotlincId = s"org.jetbrains.kotlin:kotlin-compiler:$kotlincVers"
    val pathSep = System.getProperty("path.separator")
    val kotlinCompilerPath = Pacman.repo.mvn.resolve(RepoId.parse(kotlincId)).values.
      mkString(pathSep)

    // enumerate the to-be-compiled source files
    val sources = Seq.builder[String]()
    def addSrc (p :Path) = if (p.getFileName.toString endsWith ".kt") sources += p.toString
    file match {
      case None    => Project.onFiles(sourceDirs, addSrc)
      case Some(p) => addSrc(p)
    }

    val result = Promise[Boolean]()
    if (sources.isEmpty) result.succeed(true)
    else {
      // create our command line
      val cmd = Seq[String](
        "java",
        "-cp",
        kotlinCompilerPath,
        "org.jetbrains.kotlin.cli.jvm.K2JVMCompiler",
        "-cp",
        classpath.mkString(pathSep),
        "-d",
        output.toString
      ) ++ kotlincOpts ++ sources

      // fork off a java process to run the kotlin compiler
      SubProcess(SubProcess.Config(cmd.toArray, cwd=proj.root.path),
                 proj.metaSvc.exec, buffer, result.succeed)
    }
    result
  }

  protected def nextNote (buffer :Buffer, start :Loc) = {
    buffer.findForward(outputM, start) match {
      case Loc.None => Compiler.NoMoreNotes
      case ploc => try {
        val file = proj.root.path.resolve(outputM.group(1))
        val eline = outputM.group(2).toInt-1
        val ecol = outputM.group(3).toInt-1
        val ekind = outputM.group(4)
        val errPre = outputM.group(5).trim
        // every line after the path with leading whitespace is part of the message
        val desc = Seq.builder[String]()
        desc += errPre
        var pnext = ploc.nextStart
        while (pnext < buffer.end && buffer.line(pnext).indexOf(Chars.isWhitespace) == 0) {
          desc += buffer.line(pnext).asString
          pnext = pnext.nextStart
        }
        (Compiler.Note(Store(file), Loc(eline, ecol), desc.build(), ekind == "error"), pnext)
      } catch {
        case e :Exception => log.log("Error parsing error buffer", e) ; Compiler.NoMoreNotes
      }
    }
  }
}
