package net.ollie.maths.functions.polylogarithmic

import net.ollie.maths.CachedEvaluated
import net.ollie.maths.expressions.Expression
import net.ollie.maths.functions.numeric.Ln
import net.ollie.maths.functions.{BuiltFunction, RealFunctionBuilder}
import net.ollie.maths.methods.Series
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.constants._
import net.ollie.maths.sequences.{BernoulliMinusSequence, BernoulliPlusSequence}

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
        case _ if !i.isPositive => -BernoulliMinusSequence(i.abs.succ) / i.abs.succ
        case One => Infinity
        case _ if i.isEven => new EvenIntegerZeta(i.abs)
        case _ => Zero
    }

    override protected[this] def empty = -Half

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

private class EvenIntegerZeta(val n: Natural)
    extends Real
        with RiemannZeta
        with CachedEvaluated {

    require(!n.isEmpty && n.isEven)

    def isEmpty = false

    def of = n

    private lazy val halfN: Integer = Integer.round(n / 2)

    private lazy val rep: Real = (MinusOne ^ (halfN + 1)) * BernoulliPlusSequence(n) * ((2 * Pi) ^ n) / (2 * (n !))

    protected[this] def doEvaluate(precision: Precision) = rep.evaluate(precision)

}