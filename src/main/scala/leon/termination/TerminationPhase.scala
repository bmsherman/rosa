/* Copyright 2009-2015 EPFL, Lausanne */

package leon
package termination

import purescala.Definitions._
import purescala.DefOps._

object TerminationPhase extends LeonPhase[Program,TerminationReport] {
  val name = "Termination"
  val description = "Check termination of PureScala functions"

  def run(ctx : LeonContext)(program : Program) : TerminationReport = {
    val startTime = System.currentTimeMillis

//    val tc = new SimpleTerminationChecker(ctx, program)
    val tc = new ComplexTerminationChecker(ctx, program)

    tc.initialize()

    val results = visibleFunDefsFromMain(program).toList.sortWith(_.getPos < _.getPos).map { funDef =>
      (funDef -> tc.terminates(funDef))
    }
    val endTime = System.currentTimeMillis

    new TerminationReport(results, (endTime - startTime).toDouble / 1000.0d)
  }
}
