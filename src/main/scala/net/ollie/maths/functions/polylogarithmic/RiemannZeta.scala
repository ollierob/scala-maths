package net.ollie.maths.functions.polylogarithmic

import net.ollie.maths.CachedEvaluated
import net.ollie.maths.expressions.Expression
import net.ollie.maths.functions.{BuiltFunction, RealFunctionBuilder}
import net.ollie.maths.numbers._
import net.ollie.maths.methods.Series
import net.ollie.maths.numbers.constants._
import net.ollie.maths.functions.numeric.Ln
import net.ollie.maths.sequences.{BernoulliPlusSequence, BernoulliSequence}

/**
 * Created by Ollie on 17/02/14.
 *
 * @see http://mathworld.wolfram.com/RiemannZetaFunction.html
 */
object RiemannZeta
    extends RealFunctionBuilder {

    def apply(re: Real): Real = re match {
        case Zero => empty
        case i: Integer => apply(i)
        case _ => ???
    }

    def apply(i: Integer): Real = i match {
        case Zero => empty
        case _ if !i.isPositive => -BernoulliPlusSequence(i.abs.succ) / i.abs.succ
        case One => Infinity
        case _ if i.isEven => new EvenIntegerZeta(i.abs)
        case _ => Zero
    }

    protected[this] def empty = -Half

    protected[this] def create(expr: Expression) = new RiemannZetaOf(expr)

}

trait RiemannZeta {

    def of: Expression

    override def toString = s"RiemannZeta($of)"

}

class RiemannZetaOf(val of: Expression)
    extends BuiltFunction
        with RiemannZeta {

    def isEmpty = false

    protected[this] def derivative(x: Expression) = -Series(dk _, 2)

    private def dk(k: Integer): Expression = Ln(k) / (k ^ of)

    override protected[this] def builder = RiemannZeta

}

private class EvenIntegerZeta(val of: Natural)
    extends Real
        with RiemannZeta
        with CachedEvaluated {

    require(!of.isEmpty)
    require(of.isEven)

    def isEmpty = false

    private lazy val halfN: Natural = (of / 2).numerator.abs

    private lazy val rep: Real = (MinusOne ^ (halfN.succ)) * BernoulliPlusSequence(of) * ((2 * Pi) ^ of) / (2 * (of !))

    protected[this] def doEvaluate(precision: Precision) = rep.evaluate(precision)

}