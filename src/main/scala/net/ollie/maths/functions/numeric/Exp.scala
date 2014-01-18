package net.ollie.maths.functions.numeric

import net.ollie.maths._
import net.ollie.maths.functions.DifferentiableExpressionBuilder
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.real.EulersNumber
import org.nevec.rjm.BigDecimalMath

/**
 * Created by Ollie on 18/01/14.
 */
object Exp
        extends DifferentiableExpressionBuilder {

    def apply(n: Number): Number = n match {
        case re: RealNumber => apply(re)
        case _ => ???
    }

    def apply(re: RealNumber): RealNumber = re match {
        case Zero => empty
        case NegativeInfinity => Zero
        case _ => new ExpOf(re)
    }

    def unapply(exp: Exp): Option[Expression] = Some(exp.of)

    protected[this] def create(expr: Expression) = expr match {
        case Ln(of) => of
        case _ => new Exp(expr)
    }

    protected[this] def create(diff: Differentiable) = diff match {
        case DifferentiableLn(of) => of
        case _ => new DifferentiableExp(diff)
    }

    protected[this] def empty = One

}

class Exp(val of: Expression)
        extends Composite
        with Invertible {

    def isEmpty = false

    protected[this] def at(n: Number) = Exp(n)

    protected[this] def apply(expr: Expression) = Exp(expr)

    override def toString = s"Exp($of)"

    def inverse: Expression = Ln(of)

}

object DifferentiableExp {

    def unapply(exp: DifferentiableExp): Option[Differentiable] = Some(exp.of)

}

class DifferentiableExp(override val of: Differentiable)
        extends Exp(of)
        with DifferentiableComposite {

    protected[this] def df(of: Differentiable) = Exp(of)

    override def inverse: Differentiable = Ln(of)

}

class ExpOf(re: RealNumber)
        extends PositiveRealPower(EulersNumber, re)
        with RealNumber {

    override def isEmpty = NegativeInfinity != re

    protected[this] override def eval(precision: Precision) = BigDecimalMath.exp(re.evaluate(precision).underlying())

}
