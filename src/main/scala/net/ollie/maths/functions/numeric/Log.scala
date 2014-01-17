package net.ollie.maths.functions.numeric


import scala.math.BigDecimal.RoundingMode.RoundingMode

import net.ollie.maths._
import net.ollie.maths.functions.{DifferentiableUnivariate, DifferentiableUnivariateBuilder}
import net.ollie.maths.methods.{InfiniteSeries, TaylorSeries}
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.real.EulersNumber

/**
 * Created by Ollie on 16/01/14.
 */
trait Log
        extends Invertible {

    def base: RealNumber

    def of: Expression

    def inverse: Expression = base ^ of

    def isEmpty = false

    override def toString = s"Log($base)($of)"

}

object Log {

    def apply(of: PositiveRealNumber, base: RealNumber): RealNumber = {
        if (of.isEmpty) -Infinity
        else base match {
            case _ if base.isStrictlyPositive => Ln(of) / Ln(base.abs)
            case _ => ???
        }
    }

    def apply(of: Expression, base: RealNumber): Expression = Ln(of) / Ln(base)

    def apply(of: Differentiable, base: RealNumber): Differentiable = of match {
        case n: PositiveRealNumber => apply(n, base)
        case _ => Ln(of) / Ln(base)
    }

}

object Log10 {

    def apply(of: PositiveRealNumber): RealNumber = Log(of, 10)

    def apply(expr: Expression): Expression = Log(expr, 10)

}

object Ln
        extends DifferentiableUnivariateBuilder {

    def apply(n: Number): Number = n match {
        case Zero => empty
        case p: PositiveRealNumber => apply(p)
        case _ => ???
    }

    def apply(re: PositiveRealNumber): RealNumber = new RealLn(re)

    protected[this] def create(expr: Expression) = new Ln(expr)

    protected[this] def create(diff: Differentiable) = new DifferentiableLn(diff)

    def apply(variable: Variable) = new LnX(variable)

    protected[this] def empty: RealNumber = -Infinity

}

class Ln(val of: Expression)
        extends Log
        with Composite {

    def base = EulersNumber

    protected[this] def at(n: Number) = Ln(n)

    protected[this] def apply(expr: Expression) = Ln(expr)

    override def toString = s"Ln($of)"

}

class DifferentiableLn(override val of: Differentiable)
        extends Ln(of)
        with DifferentiableComposite {

    protected[this] def df(of: Differentiable) = 1 / of

}

class LnX(val variable: Variable)
        extends DifferentiableLn(variable)
        with DifferentiableUnivariate
        with DifferentiableComposite {

    override def variables = super[DifferentiableUnivariate].variables

    override def df(x: Variable): DifferentiableUnivariate = 1 / x

}

class RealLn(override val of: PositiveRealNumber)
        extends Ln(of)
        with RealNumber {

    override def inverse = super[RealNumber].inverse

    protected[this] def eval(precision: Precision)(implicit mode: RoundingMode) = evaluation.evaluate(precision)

    protected def evaluation: RealNumber = evaluator

    private lazy val evaluator: RealNumber = {
        if (of <= One) TaylorSeries(Ln, of, -1)
        else InfiniteSeries(n => 1 / (n * ((of / (of - 1)) ^ n)), One)
    }

    override def toConstant = super[RealNumber].toConstant

    override def variables = super[RealNumber].variables

}