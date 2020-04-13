package net.ollie.maths.functions.numeric

import net.ollie.maths._
import net.ollie.maths.expressions.{Composition, Expression, Invertible}
import net.ollie.maths.functions.ComplexFunctionBuilder
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.complex.{Complex, ComplexInfinity}
import net.ollie.maths.numbers.constants.{EulersNumber, One, Zero}
import net.ollie.utils.BigDecimals

/**
 * Created by Ollie on 16/01/14.
 */
trait Log
    extends Invertible {

    def base: Real

    def of: Expression

    override def toString = s"Log($base)($of)"

}

object Log {

    def apply(of: PositiveReal, base: Real): Real = {
        if (of.isEmpty) -Infinity
        else base match {
            case _ if base.isPositive => Ln(of) / Ln(base.abs)
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
    extends ComplexFunctionBuilder {

    override type Z = Constant

    override def apply(re: Real): Complex = re match {
        case _ if re.isPositive => Complex(Ln(re.abs))
        case Zero => ComplexInfinity
        case _ => apply(Complex(re)).principal
    }

    def apply(re: PositiveReal): Real = re match {
        case One => Zero
        case EulersNumber => One
        case _ => new RealLn(re)
    }

    def apply(z: Complex): ComplexLogarithms = new ComplexLn(z)

    protected[this] def create(expr: Expression) = expr match {
        case Exp(of) => of
        case _ => new LnOf(expr)
    }

    protected[this] def empty: Real = -Infinity

    def unapply(ln: Ln): Option[Expression] = Some(ln.of)

}

trait Ln
    extends Log {

    def of: Expression

    def base = EulersNumber

    override def toString = s"Ln($of)"

}

class LnOf(val of: Expression)
    extends Log
        with Composition
        with Ln {

    protected[this] def at(n: Constant) = Ln(n)

    protected[this] def apply(expr: Expression) = Ln(expr)

    protected[this] def derivative(at: Expression) = 1 / at

    def inverse: Expression = base ^ of

    def isEmpty = false

}

class RealLn(override val of: PositiveReal)
    extends Real
        with Ln
        with CachedEvaluated {

    require(!of.isEmpty)

    override def isPositive = of > One

    def isEmpty = of == One

    override def abs = super[Real].abs

    protected[this] def doEvaluate(precision: Precision) = {
        BigDecimals.logE(of.evaluate(precision), precision)
    }

    override def toConstant = super[Real].toConstant

    override def variables = super[Real].variables

}

trait ComplexLogarithms
    extends Multivalued
        with Ln {

    type Contents = Complex

    def of: Complex

}

private class ComplexLn(val of: Complex)
    extends ComplexLogarithms {

    require(!of.isEmpty)

    val principal = Complex(Ln(of.abs), of.arg)

    def isEmpty = false

    def inverse = ??? //TODO

}