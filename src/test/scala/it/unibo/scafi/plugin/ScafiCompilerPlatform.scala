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
  //TODO find a more powerful way to avoid problems with sbt, the problems is the java.class.path values!
  settings.usejavacp.value = true //used to find the scala compiler by the global
  //report creation, used to retrive warning, error and info
  private val reporter = new DebuggerReporter(settings)
  //compiler creation, where the magic happens
  val global = new Global(settings,reporter) {
    override protected def computeInternalPhases () {
      super.computeInternalPhases
      //add the phase added on this module
      for (phase <- new ScafiDSLPlugin(this).components)
        phasesSet += phase
    }
  }
  //create compiler

  def compile(code : String) : CompilationReport = {
    val compilation  = new global.Run()
    val sources = List(createFromString(code))
    compilation.compileSources(sources)
    //a way to check code after a phase is to used compiltation.units
    try {
      this.reporter.report().appendCode(compilation.units.map(_.body.toString()))
    } finally {
      this.reporter.clearOutputCount()
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
