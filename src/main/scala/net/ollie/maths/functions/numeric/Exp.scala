package net.ollie.maths.functions.numeric

import net.ollie.maths._
import net.ollie.maths.functions.ExpressionBuilder
import net.ollie.maths.numbers._
import org.nevec.rjm.BigDecimalMath
import net.ollie.maths.numbers.constants.{Zero, One, EulersNumber}

/**
 * Created by Ollie on 18/01/14.
 * @see http://mathworld.wolfram.com/ExponentialFunction.html
 * @see Ln
 */
object Exp
        extends ExpressionBuilder {

    def apply(n: Number): Number = n match {
        case re: Real => apply(re)
        case _ => ???
    }

    def apply(re: Real): Real = re match {
        case Zero => empty
        case MinusInfinity => Zero
        case _ => new ExpOf(re)
    }

    def unapply(exp: Exp): Option[Expression] = Some(exp.of)

    protected[this] def create(expr: Expression) = expr match {
        case Ln(of) => of
        case _ => new Exp(expr)
    }

    protected[this] def empty = One

}

class Exp(val of: Expression)
        extends Function
        with Invertible {

    def isEmpty = false

    protected[this] def at(n: Number) = Exp(n)

    protected[this] def apply(at: Expression) = Exp(at)

    override def toString = s"Exp($of)"

    def inverse = Ln(of)

    protected[this] def derivative(at: Expression) = Exp(at)

}

class ExpOf(re: Real)
        extends PositiveRealPower(EulersNumber, re)
        with Real {

    override def isEmpty = MinusInfinity == re

    protected[this] override def doEvaluate(precision: Precision) = BigDecimalMath.exp(re.evaluate(precision).underlying())

}
