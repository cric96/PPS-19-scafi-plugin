package it.unibo.scafi.plugin

import scala.reflect.internal.util.{BatchSourceFile, Position}
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.reporters.AbstractReporter
import scala.tools.nsc.{Global, Settings}

class ScafiCompilerPlatform(verbose : Boolean) {
  private val settings = new Settings() //setting used to create the context of compiler
  settings.verbose.value = verbose
  private val virtualDir = new VirtualDirectory("(memory)", None)
  settings.outputDirs.setSingleOutput(virtualDir) //all compile source are store in memory
  settings.usejavacp.value = true //used to find the scala compiler by the global
  //create global, attach the new plugin phases, using a report to check error and warning
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
    val sources = List(global.newSourceFile(code))
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
