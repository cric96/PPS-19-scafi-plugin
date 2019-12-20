package it.unibo.scafi.plugin

import java.io.File
import java.net.{URL, URLClassLoader}
import java.nio.file.Files

import scala.reflect.internal.util.{BatchSourceFile, Position}
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.reporters.AbstractReporter
import scala.tools.nsc.{Global, Settings}

class ScafiCompilerPlatform(verbose : Boolean) {
  private val settings = new Settings() //setting used to create the context of compiler
  settings.embeddedDefaults(this.getClass.getClassLoader)
  settings.verbose.value = verbose
  private val virtualDir = new VirtualDirectory("(memory)", None)
  settings.outputDirs.setSingleOutput(virtualDir) //all compile source are store in memory
  settings.usejavacp.value = true
  private def createGlobal(report : AbstractReporter) : Global = {
    new Global(settings,report) {
      override protected def computeInternalPhases () {
        super.computeInternalPhases
        //add the phase added on this module
        for (phase <- new ScafiDSLPlugin(this).components)
          phasesSet += phase
      }
    }
  }

  /**
    * compile a scala code passed as a string
    * @param code: the scala code.
    * @return the compilation's report with the code generated, the error, warning and info count produced by
    *         the compiler (with the scafi plugin injected)
    */
  def compile(code : String) : CompilationReport = {
    val reporter = new DebuggerReporter(settings)
    val global = createGlobal(reporter)
    val compilation  = new global.Run()
    val sources = List(createFromString(code))
    compilation.compileSources(sources)
    //a way to check code after a phase is to used compiltation.units
    try {
      reporter.report().appendCode(compilation.units.map(_.body.toString()))
    } finally {
      reporter.clearOutputCount()
    }
  }


  private def createFromString(code : String) : BatchSourceFile = new BatchSourceFile("<test>",code)
}
case class CompilationReport(errors : List[String],
                             warnings: List[String],
                             info : List[String],
                             code : List[String] = List()) {
  def hasErrors = errors.nonEmpty

  def hasWarnings = warnings.nonEmpty

  def hasInfo = info.nonEmpty

  def appendCode(compiledCode : Iterator[String]) : CompilationReport = this.copy(code = compiledCode.toList)
}

class DebuggerReporter(override val settings: Settings) extends AbstractReporter {
  private var outputMap : Map[Int,List[String]] = initInfoMap()
  private def initInfoMap() : Map[Int,List[String]] = List(INFO.id, WARNING.id, ERROR.id).map(_ -> List()).toMap

  override def display(pos: Position, msg: String, severity: Severity): Unit = {
    outputMap += severity.id -> (msg :: outputMap(severity.id))
    if(settings.verbose.value) {
      println(s"pos : $pos, severity : $severity, msg = $msg")
    }
  }

  override def displayPrompt(): Unit = { }

  def clearOutputCount(): Unit = {
    outputMap = initInfoMap()
  }

  def report() : CompilationReport = CompilationReport(outputMap(ERROR.id), outputMap(WARNING.id), outputMap(INFO.id))
}

final class ReplClassloader(parent: ClassLoader) extends ClassLoader(parent)  {
  override def getResource(name: String): URL = {
    // Rather pass `settings.usejavacp.value = true` (which doesn't work
    // under SBT) we do the same as SBT and respond to a resource request
    // by the compiler for the magic name "app.classpath", write the JAR files
    // from our classloader to a temporary file, and return that as the resource.
    if (name == "app.class.path") {
      def writeTemp(content: String): File = {
        val f = File.createTempFile("classpath", ".txt")
        //          IO.writeFile(f, content)
        val p = new java.io.PrintWriter(f)
        p.print(content)
        p.close
        f
      }
      println("Attempting to configure Scala classpath based on classloader: " + getClass.getClassLoader)
      val superResource = super.getResource(name)
      if (superResource != null) superResource // In SBT, let it do it's thing
      else getClass.getClassLoader match {
        case u: URLClassLoader =>
          // Rather pass `settings.usejavacp.value = true` (which doesn't work
          // under SBT) we do the same as SBT and respond to a resource request
          // by the compiler for the magic name "app.classpath"
          println("yay...")
          val files = u.getURLs.map(x => new java.io.File(x.toURI))
          val f = writeTemp(files.mkString(File.pathSeparator))
          println(Files.readAllLines(f.toPath))
          f.toURI.toURL
        case other =>
          // We're hosed here.
          println("uh-oh")
          null
      }
    } else super.getResource(name)
  }
}