/* Copyright 2009-2015 EPFL, Lausanne */

package leon
package verification

import purescala.Common._
import purescala.Trees._
import purescala.TreeOps._
import purescala.TypeTrees._
import purescala.Definitions._

class InductionTactic(vctx: VerificationContext) extends DefaultTactic(vctx) {
  override val description = "Induction tactic for suitable functions"
  override val shortDescription = "induction"

  private def firstAbsClassDef(args: Seq[ValDef]): Option[(AbstractClassType, ValDef)] = {
    args.map(vd => (vd.getType, vd)).collect {
      case (act: AbstractClassType, vd) => (act, vd)
    }.headOption
  }

  private def selectorsOfParentType(parentType: ClassType, cct: CaseClassType, expr: Expr): Seq[Expr] = {
    val childrenOfSameType = cct.fields.filter(_.tpe == parentType)
    for (field <- childrenOfSameType) yield {
      CaseClassSelector(cct, expr, field.id)
    }
  }

  override def generatePostconditions(fd: FunDef): Seq[VerificationCondition] = {
    (fd.body.map(safe), firstAbsClassDef(fd.params), fd.postcondition) match {
      case (Some(b), Some((parentType, arg)), Some((id, p))) =>
        val post = safe(p)
        val body = safe(b)

        for (cct <- parentType.knownCCDescendents) yield {
          val selectors = selectorsOfParentType(parentType, cct, arg.toVariable)

          val subCases = selectors.map { sel =>
            val res = id.freshen
            replace(Map(arg.toVariable -> sel),
              Implies(precOrTrue(fd), Let(res, body, replace(Map(id.toVariable -> res.toVariable), post)))
            )
          }

          val vc = Implies(And(CaseClassInstanceOf(cct, arg.toVariable), precOrTrue(fd)), Implies(And(subCases), Let(id, body, post)))

          new VerificationCondition(vc, fd, VCPostcondition, this).setPos(fd)
        }

      case (body, _, post) =>
        if (post.isDefined && body.isDefined) {
          reporter.warning(fd.getPos, "Could not find abstract class type argument to induct on")
        }
        super.generatePostconditions(fd)
    }
  }

  override def generatePreconditions(fd: FunDef): Seq[VerificationCondition] = {
    (fd.body.map(safe), firstAbsClassDef(fd.params)) match {
      case (Some(b), Some((parentType, arg))) =>
        val body = safe(b)

        val calls = collectWithPC {
          case fi @ FunctionInvocation(tfd, _) if tfd.hasPrecondition => (fi, safe(tfd.precondition.get))
        }(body)

        calls.flatMap {
          case ((fi @ FunctionInvocation(tfd, args), pre), path) =>
            for (cct <- parentType.knownCCDescendents) yield {
              val selectors = selectorsOfParentType(parentType, cct, arg.toVariable)

              val subCases = selectors.map { sel =>
                replace(Map(arg.toVariable -> sel),
                  Implies(precOrTrue(fd), replace((tfd.params.map(_.toVariable) zip args).toMap, pre))
                )
              }

              val vc = Implies(And(Seq(CaseClassInstanceOf(cct, arg.toVariable), precOrTrue(fd), path)), Implies(And(subCases), replace((tfd.params.map(_.toVariable) zip args).toMap, pre)))

              new VerificationCondition(vc, fd, VCPrecondition, this).setPos(fi)
            }
        }

      case (body, _) =>
        if (body.isDefined) {
          reporter.warning(fd.getPos, "Could not find abstract class type argument to induct on")
        }
        super.generatePreconditions(fd)
    }
  }
}
