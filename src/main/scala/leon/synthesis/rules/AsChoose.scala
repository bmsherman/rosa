/* Copyright 2009-2015 EPFL, Lausanne */

package leon
package synthesis
package rules

case object AsChoose extends Rule("As Choose") {
  def instantiateOn(sctx: SynthesisContext, p: Problem): Traversable[RuleInstantiation] = {
      Some(new RuleInstantiation(p, this, SolutionBuilder.none, this.name, this.priority) {
        def apply(sctx: SynthesisContext) = {
          RuleClosed(Solution.choose(p))
        }
      })
  }
}

