package leon
package numerics

import purescala.Common._
import purescala.Definitions._
import purescala.Trees._
import purescala.TreeOps._
import purescala.TypeTrees._

object CertificationPhase extends LeonPhase[Program,CertificationReport] {
  val name = "Certification"
  val description = "Floating-point certification"


  /*override val definedOptions: Set[LeonOptionDef] = Set( )*/

  def generateVerificationConditions(reporter: Reporter, program: Program):
    List[VerificationCondition] = {

    var allVCs: Seq[VerificationCondition] = Seq.empty

    val analyser = new Analyser(reporter)

    for(funDef <- program.definedFunctions.toList) {

      if (funDef.body.isDefined) {
        allVCs ++= analyser.generateVCs(funDef)
      }
    }
    allVCs.toList
  }

  def checkVerificationConditions(reporter: Reporter, vcs: Seq[VerificationCondition]):
    CertificationReport = {
    CertificationReport.emptyReport
  }

  def run(ctx: LeonContext)(program: Program): CertificationReport = {
    val reporter = ctx.reporter
    reporter.info("Running Certification phase")

    val vcs = generateVerificationConditions(reporter, program)
    reporter.info("Generated " + vcs.size + " verification conditions")
    println("=======  VCs: ======")
    println(vcs)
    checkVerificationConditions(reporter, vcs)
  }
  
}
