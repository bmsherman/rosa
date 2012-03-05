package leon
package plugin

import scala.tools.nsc
import scala.tools.nsc.{Global,Phase}
import scala.tools.nsc.plugins.{Plugin,PluginComponent}
import purescala.Definitions.Program

/** This class is the entry point for the plugin. */
class LeonPlugin(val global: Global, val actionAfterExtraction : Option[Program=>Unit] = None) extends Plugin {
  import global._

  val name = "leon"
  val description = "The Leon static analyzer"

  var stopAfterAnalysis: Boolean = true
  var stopAfterExtraction: Boolean = false
  var silentlyTolerateNonPureBodies: Boolean = false

  /** The help message displaying the options for that plugin. */
  override val optionsHelp: Option[String] = Some(
    "  -P:leon:uniqid             When pretty-printing purescala trees, show identifiers IDs" + "\n" +
    "  -P:leon:with-code          Allows the compiler to keep running after the static analysis" + "\n" +
    "  -P:leon:parse              Checks only whether the program is valid PureScala" + "\n" +
    "  -P:leon:extensions=ex1:... Specifies a list of qualified class names of extensions to be loaded" + "\n" +
    "  -P:leon:nodefaults         Runs only the analyses provided by the extensions" + "\n" +
    "  -P:leon:functions=fun1:... Only generates verification conditions for the specified functions" + "\n" +
    "  -P:leon:unrolling=[0,1,2]  Unrolling depth for recursive functions" + "\n" + 
    "  -P:leon:axioms             Generate simple forall axioms for recursive functions when possible" + "\n" + 
    "  -P:leon:tolerant           Silently extracts non-pure function bodies as ''unknown''" + "\n" +
    "  -P:leon:bapa               Use BAPA Z3 extension (incompatible with many other things)" + "\n" +
    "  -P:leon:impure             Generate testcases only for impure functions" + "\n" +
    "  -P:leon:testcases=[1,2]    Number of testcases to generate per function" + "\n" +
    "  -P:leon:testbounds=l:u     Lower and upper bounds for integers in recursive datatypes" + "\n" +
    "  -P:leon:timeout=N          Sets a timeout of N seconds" + "\n" +
    "  -P:leon:XP                 Enable weird transformations and other bug-producing features" + "\n" +
    "  -P:leon:BV                 Use bit-vectors for integers" + "\n" +
    "  -P:leon:prune              Use additional SMT queries to rule out some unrollings" + "\n" +
    "  -P:leon:cores              Use UNSAT cores in the unrolling/refinement step" + "\n" +
    "  -P:leon:quickcheck         Use QuickCheck-like random search" + "\n" +
    "  -P:leon:parallel           Run all solvers in parallel" + "\n" +
    "  -P:leon:templates          Use new ``FunctionTemplate'' technique" + "\n" +
    "  -P:leon:noLuckyTests       Do not perform additional tests to potentially find models early"
  )

  /** Processes the command-line options. */
  private def splitList(lst: String) : Seq[String] = lst.split(':').map(_.trim).filter(!_.isEmpty)
  override def processOptions(options: List[String], error: String => Unit) {
    for(option <- options) {
      option match {
        case "with-code"  =>                     stopAfterAnalysis = false
        case "uniqid"     =>                     leon.Settings.showIDs = true
        case "parse"      =>                     stopAfterExtraction = true
        case "tolerant"   =>                     silentlyTolerateNonPureBodies = true
        case "nodefaults" =>                     leon.Settings.runDefaultExtensions = false
        case "axioms"     =>                     leon.Settings.noForallAxioms = false
        case "bapa"       =>                     leon.Settings.useBAPA = true
        case "impure"     =>                     leon.Settings.impureTestcases = true
        case "XP"         =>                     leon.Settings.experimental = true
        case "BV"         =>                     leon.Settings.bitvectorBitwidth = Some(32)
        case "prune"      =>                     leon.Settings.pruneBranches = true
        case "cores"      =>                     leon.Settings.useCores = true
        case "quickcheck" =>                     leon.Settings.useQuickCheck = true
        case "parallel"   =>                     leon.Settings.useParallel = true
        case "templates"  =>                     leon.Settings.useTemplates = true
        case "noLuckyTests" =>                   leon.Settings.luckyTest = false
        case s if s.startsWith("unrolling=") =>  leon.Settings.unrollingLevel = try { s.substring("unrolling=".length, s.length).toInt } catch { case _ => 0 }
        case s if s.startsWith("functions=") =>  leon.Settings.functionsToAnalyse = Set(splitList(s.substring("functions=".length, s.length)): _*)
        case s if s.startsWith("extensions=") => leon.Settings.extensionNames = splitList(s.substring("extensions=".length, s.length))
        case s if s.startsWith("testbounds=") => leon.Settings.testBounds = try { val l = splitList(s.substring("testBounds=".length, s.length)).map(_.toInt); if (l.size != 2) (0, 3) else (l(0), l(1)) } catch { case _ => (0, 3) }
        case s if s.startsWith("timeout=") => leon.Settings.solverTimeout = try { Some(s.substring("timeout=".length, s.length).toInt) } catch { case _ => None }
        case s if s.startsWith("testcases=") =>  leon.Settings.nbTestcases = try { s.substring("testcases=".length, s.length).toInt } catch { case _ => 1 }
        case _ => error("Invalid option: " + option)
      }
    }
  }

  val components = List[PluginComponent](new AnalysisComponent(global, this))
  val descriptions = List[String]("leon analyses")
}
