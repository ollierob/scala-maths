package net.ollie.maths.functions.special

import net.ollie.maths.functions.{ComplexFunctionBuilder, BuiltFunction}
import net.ollie.maths.{Number, Expression}
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.constants.{Zero, One}
import net.ollie.maths.methods.Series
import net.ollie.maths.numbers.complex.Complex

/**
 * Multivalued function.
 * Created by Ollie on 09/02/14.
 * @see http://mathworld.wolfram.com/LambertW-Function.html
 */
object LambertW
        extends ComplexFunctionBuilder {

    protected[this] def empty = Zero

    protected[this] def create(expr: Expression) = new LambertWFunction(expr)

    override def apply(re: Real): Number = ???

    def apply(z: Complex): LambertWBranches = {
        new LambertWBranches(new LambertWZeroComplexBranch(z), new LambertWMinusOneBranch(z))
    }

    def apply(p: PositiveReal): LambertWZeroBranch with PositiveReal = new LambertWZeroPositiveBranch(p)

}

trait LambertW {

    def of: Expression

    override def toString = s"LambertW($of)"

}

class LambertWFunction(val of: Expression)
        extends BuiltFunction
        with LambertW {

    protected[this] def builder = LambertW

    protected[this] def derivative(x: Expression) = LambertW(x) / (x * (1 + LambertW(x)))

    def isEmpty = of.isEmpty

}

abstract class LambertWZeroBranch(val of: Number)
        extends Number
        with LambertW {

    def isEmpty = of.isEmpty

    override def toString = s"LambertW0($of)"

}

class LambertWZeroPositiveBranch(override val of: PositiveReal)
        extends LambertWZeroBranch(of)
        with PositiveReal {

    private lazy val taylor = Series(nth _, One)

    private def nth(n: Natural): Real = {
        (of ^ n) * ((-n) ^ (n - 1)) / (n !)
    }

    protected[this] def doEvaluate(precision: Precision) = taylor.evaluate(precision)

}

class LambertWZeroComplexBranch(override val of: Complex)
        extends LambertWZeroBranch(of)
        with Complex {

    def im = ???

    def re = ???

}

class LambertWMinusOneBranch(val of: Complex)
        extends LambertW
        with Complex {

    def im = ???

    def re = ???

    override def toString = s"LambertW-1($of)"

}

class LambertWBranches(val w0: LambertWZeroComplexBranch, val w1: LambertWMinusOneBranch)
        extends MultivaluedSet(Set[Complex](w0, w1))