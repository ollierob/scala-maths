package net.ollie.maths.functions.numeric

import net.ollie.maths._
import net.ollie.maths.functions.ExpressionBuilder
import net.ollie.maths.numbers._
import org.nevec.rjm.BigDecimalMath
import net.ollie.maths.numbers.constants.EulersNumber

/**
 * Created by Ollie on 16/01/14.
 */
trait Log
        extends Invertible {

    def base: Real

    def of: Expression

    def inverse = base ^ of

    def isEmpty = false

    override def toString = s"Log($base)($of)"

}

object Log {

    def apply(of: PositiveReal, base: Real): Real = {
        if (of.isEmpty) -Infinity
        else base match {
            case _ if base.isStrictlyPositive => Ln(of) / Ln(base.abs)
            case _ => ???
        }
    }

    def apply(of: Expression, base: Real): Expression = Ln(of) / Ln(base)

}

object Log10 {

    def apply(of: PositiveReal): Real = Log(of, 10)

    def apply(expr: Expression): Expression = Log(expr, 10)

}

object Ln
        extends ExpressionBuilder {

    def apply(n: Number): Number = n match {
        case Zero => empty
        case re: Real if re.isStrictlyPositive => apply(re.abs)
        case _ => Operation.illegal(s"Number $n was not a real, positive number!")
    }

    def apply(re: PositiveReal): Real = new RealLn(re)

    protected[this] def create(expr: Expression) = expr match {
        case Exp(of) => of
        case _ => new Ln(expr)
    }

    protected[this] def empty: Real = -Infinity

    def unapply(ln: Ln): Option[Expression] = Some(ln.of)

}

class Ln(val of: Expression)
        extends Log
        with Function {

    def base = EulersNumber

    protected[this] def at(n: Number) = Ln(n)

    protected[this] def apply(expr: Expression) = Ln(expr)

    override def toString = s"Ln($of)"

    protected[this] def derivative(at: Expression) = 1 / at

}

class RealLn(override val of: PositiveReal)
        extends Ln(of)
        with Real {

    override def inverse = super[Real].inverse

    protected[this] def doEvaluate(precision: Precision) = BigDecimalMath.log(of.evaluate(precision).underlying())

    override def toConstant = super[Real].toConstant

    override def variables = super[Real].variables

}