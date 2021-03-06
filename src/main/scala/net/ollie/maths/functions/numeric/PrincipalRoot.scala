package net.ollie.maths.functions.numeric

import ch.obermuhlner.math.big.BigDecimalMath
import net.ollie.maths.CachedEvaluated
import net.ollie.maths.methods.{EvaluationIterator, IterativelyEvaluated}
import net.ollie.maths.numbers.complex.{Complex, ImaginaryUnit}
import net.ollie.maths.numbers.constants.{MinusOne, One, Zero}
import net.ollie.maths.numbers.{Natural, PositiveReal, Precision, Real}
import net.ollie.utils.BigDecimals

/**
 * Created by Ollie on 15/02/14.
 */
trait PrincipalRoot
    extends PositiveReal {

    def of: PositiveReal

    def degree: Natural

    def isEmpty = of.isEmpty

    override def toString = s"PrincipalRoot($degree)($of)"

}

object PrincipalRoot {

    def apply(of: Real, degree: Natural): Complex = of match {
        case Zero => Zero
        case One => One
        case MinusOne => if (degree.isEven) ImaginaryUnit else MinusOne
        case _ if of.isPositive => apply(of.abs, degree)
        case _ => apply(Complex(of), degree)
    }

    def apply(of: PositiveReal, degree: Natural): PositiveReal = degree.toInt match {
        case Some(i: Int) => new DirectPrincipalRoot(of, i)
        case _ => new NewtonPrincipalRoot(of, degree)
    }

    def apply(of: Complex, degree: Natural): Complex = ??? //TODO

}

private class DirectPrincipalRoot(val of: PositiveReal, val n: Int)
    extends PrincipalRoot
        with CachedEvaluated {

    def degree = n

    private lazy val degreeBd = BigDecimal(n).underlying()

    protected[this] def doEvaluate(precision: Precision) = {
        BigDecimals.root(of.evaluate(precision), degreeBd, precision)
    }

}

/**
 *
 * @see http://en.wikipedia.org/wiki/Nth_root_algorithm
 */
private class NewtonPrincipalRoot(val of: PositiveReal, val degree: Natural)
    extends PrincipalRoot
        with IterativelyEvaluated {

    def initialGuess: Real = of / degree

    override def evaluationIterator(startPrecision: Precision) = new EvaluationIterator {

        var x = initialGuess

        override def next(nth: Natural, precision: Precision) = {
            val deltaX = ((of / (x ^ (degree - 1))) - x) / degree
            x += deltaX
            x.evaluate(precision)
        }

    }

    protected[this] def doApproximatelyEvaluate(precision: Precision) = evaluate(precision)

}