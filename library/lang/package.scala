/* Copyright 2009-2015 EPFL, Lausanne */

package leon

import leon.annotation._
import scala.language.implicitConversions

package object lang {
  @ignore
  sealed class IsValid(val property : Boolean) {
    def holds : Boolean = {
      assert(property)
      property
    }
  }

  @ignore
  implicit def any2IsValid(x: Boolean) : IsValid = new IsValid(x)

  @ignore
  object InvariantFunction {
    def invariant(x: Boolean): Unit = ()
  }

  @ignore
  implicit def while2Invariant(u: Unit) = InvariantFunction

  @ignore
  def error[T](reason: java.lang.String): T = sys.error(reason)

  @library
  def passes[A, B](in: A, out: B)(tests: Map[A,B]): Boolean = {
    if (tests contains in) {
      tests(in) == out
    } else {
      true
    }
  }
}
