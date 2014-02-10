package net.ollie.maths.functions.numeric

import net.ollie.maths._
import net.ollie.maths.functions.FunctionBuilder
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.constants.{One, Zero, EulersNumber}
import org.nevec.rjm.BigDecimalMath

/**
 * Created by Ollie on 16/01/14.
 */
trait Log
        extends Invertible {

    type Inverse = Expression //TODO

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
        extends FunctionBuilder {

    def apply(n: Number): Number = n match {
        case Zero => empty
        case re: Real if re.isStrictlyPositive => apply(re.abs)
        case _ => Operation.illegal(s"Number $n was not a real, positive number!")
    }

    def apply(re: PositiveReal): Real = new RealLn(re)

    protected[this] def create(expr: Expression) = expr match {
        case Exp(of) => of
        case _ => new LnOf(expr)
    }

    protected[this] def empty: Real = -Infinity

    def unapply(ln: Ln): Option[Expression] = Some(ln.of)

}

trait Ln {

    def of: Expression

    override def toString = s"Ln($of)"

}

class LnOf(val of: Expression)
        extends Log
        with Function
        with Ln {

    def base = EulersNumber

    protected[this] def at(n: Number) = Ln(n)

    protected[this] def apply(expr: Expression) = Ln(expr)

    protected[this] def derivative(at: Expression) = 1 / at

}

class RealLn(override val of: PositiveReal)
        extends Real
        with Ln {

    override def inverse = super[Real].inverse

    def isEmpty = of == One

    protected[this] def doEvaluate(precision: Precision) = BigDecimalMath.log(of.evaluate(precision).underlying())

    override def toConstant = super[Real].toConstant

    override def variables = super[Real].variables

}