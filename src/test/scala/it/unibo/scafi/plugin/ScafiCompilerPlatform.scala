package it.unibo.scafi.plugin

import scala.reflect.internal.util.Position
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.reporters.AbstractReporter
import scala.tools.nsc.{Global, Settings}

/**
  * platform used to compile code during testing. Code is passed as a string.
  * @param verbose : it prints some debug lines
  * @param pluginOptions : it adds new options to the compiler
  */
class ScafiCompilerPlatform(verbose : Boolean, pluginOptions : String *) {
  private val settings = new Settings() //setting used to create the context of compiler
  settings.embeddedDefaults(this.getClass.getClassLoader) //this solve problems with online testing
  settings.verbose.value = verbose
  pluginOptions.foreach(setting => settings.pluginOptions.appendToValue(s"scafi:$setting")) //adds plugins option
  private val virtualDir = new VirtualDirectory("(memory)", None)
  settings.outputDirs.setSingleOutput(virtualDir) //all compile source are store in memory
  settings.usejavacp.value = true //used to find the scala compiler by the global
  //create global, attach the new plugin phases, using a report to check error and warning
  private def createGlobal(report : AbstractReporter) : Global = {
    new Global(settings,report) {
      override protected def loadRoughPluginsList() = new ScafiDSLPlugin(this) :: super.loadRoughPluginsList()
    }
  }
  private def currentCompiled(run : Global#Run) : String = {
    run.units.map(_.body.toString()).mkString("\n")
  }
  /**
    * compile a scala code passed as a string
    * @param code: the scala code.
    * @return the compilation's report with: the generated code, the error, warning and info count produced by
    *         the compiler (with the scafi plugin injected)
    */
  def compile(code : String) : CompilationReport = {
    val reporter = new DebuggerReporter(settings)
    val global = createGlobal(reporter)
    val compilation  = new global.Run()
    val codeUnit = global.newCompilationUnit(code)
    compilation.compileUnits(List(codeUnit), compilation.parserPhase)
    reporter.report()
  }

  /**
    * it compiles until the code transformation phase
    *
    * @param code
    * @return the compilation's report with the code generated, the error, warning and info count produced by
    *         the compiler (with the scafi plugin injected)
    */
  def transform(code : String) : (String, CompilationReport) = {
    val reporter = new DebuggerReporter(settings)
    val global = createGlobal(reporter)
    val transform = new global.Run()
    val codeUnit = global.newCompilationUnit(code)
    transform.compileLate(codeUnit)
    (currentCompiled(transform), reporter.report())
  }
}

/**
  * This is a class that contains the information retrieved during compilation
  * @param errors : set of retrieved errors
  * @param warnings : set of retrieved warnings
  * @param info : set of produced info
  */
case class CompilationReport(errors : Seq[String],
                             warnings: Seq[String],
                             info : Seq[String]) {
  def hasErrors : Boolean = errors.nonEmpty

  def hasWarnings : Boolean = warnings.nonEmpty

  def hasInfo : Boolean = info.nonEmpty
}

/**
  * This is a platform-specific reporter and it collects
  * warnings and errors to make a detailed compilation report.
  */
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

  def report() : CompilationReport = CompilationReport(outputMap(ERROR.id), outputMap(WARNING.id), outputMap(INFO.id))
}