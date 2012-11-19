package leon
package synthesis
package rules

import purescala.Trees._
import purescala.Common._
import purescala.TypeTrees._
import purescala.TreeOps._
import purescala.Extractors._
import purescala.Definitions._

class ADTSplit(synth: Synthesizer) extends Rule("ADT Split.", synth, 70) {
  def applyOn(task: Task): RuleResult = {
    val p = task.problem

    val candidate = p.as.collect {
      case IsTyped(id, AbstractClassType(cd)) =>

        val optCases = for (dcd <- cd.knownDescendents) yield dcd match {
          case ccd : CaseClassDef =>
            val toSat = And(p.c, Not(CaseClassInstanceOf(ccd, Variable(id))))

            val isImplied = synth.solver.solveSAT(toSat) match {
              case (Some(false), _) => true
              case _ => false
            }

            if (!isImplied) {
              Some(ccd)
            } else {
              None
            }
          case _ =>
            None
        }

        val cases = optCases.flatten

        if (!cases.isEmpty) {
          Some((id, cases))
        } else {
          None
        }
    }


    candidate.find(_.isDefined) match {
      case Some(Some((id, cases))) =>
        val oas = p.as.filter(_ != id)

        val subInfo = for(ccd <- cases) yield {
           val args   = ccd.fieldsIds.map(id => FreshIdentifier(id.name, true).setType(id.getType)).toList

           val subPhi = subst(id -> CaseClass(ccd, args.map(Variable(_))), p.phi)
           val subProblem = Problem(args ::: oas, p.c, subPhi, p.xs)
           val subPattern = CaseClassPattern(None, ccd, args.map(id => WildcardPattern(Some(id))))

           (ccd, subProblem, subPattern)
        }


        val onSuccess: List[Solution] => Solution = {
          case sols =>
            var globalPre = List[Expr]()

            val cases = for ((sol, (ccd, problem, pattern)) <- (sols zip subInfo)) yield {
              globalPre ::= And(CaseClassInstanceOf(ccd, Variable(id)), sol.pre)

              SimpleCase(pattern, sol.term)
            }

            Solution(Or(globalPre), sols.flatMap(_.defs).toSet, MatchExpr(Variable(id), cases))
        }

        RuleOneStep(subInfo.map(_._2).toList, onSuccess)
      case _ =>
        RuleInapplicable
    }
  }
}