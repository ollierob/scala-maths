package net.ollie.maths.functions.numeric

import net.ollie.maths._
import net.ollie.maths.functions.ComplexFunctionBuilder
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.constants.{Zero, One, EulersNumber}
import org.nevec.rjm.BigDecimalMath
import net.ollie.maths.numbers.complex.{ComplexInfinity, Complex}

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
        extends ComplexFunctionBuilder {

    override type Z = Number

    override def apply(re: Real): Complex = re match {
        case _ if re.isStrictlyPositive => Complex(Ln(re.abs))
        case Zero => ComplexInfinity
        case _ => apply(Complex(re)).principal
    }

    def apply(re: PositiveReal): Real with Ln = {
        new RealLn(re)
    }

    def apply(z: Complex): ComplexLogarithms = new ComplexLn(z)

    protected[this] def create(expr: Expression) = expr match {
        case Exp(of) => of
        case _ => new LnOf(expr)
    }

    protected[this] def empty: Real = -Infinity

    def unapply(ln: Ln): Option[Expression] = Some(ln.of)

}

trait Ln extends Log {

    def of: Expression

    def base = EulersNumber

    override def toString = s"Ln($of)"

}

class LnOf(val of: Expression)
        extends Log
        with Function
        with Ln {

    protected[this] def at(n: Number) = Ln(n)

    protected[this] def apply(expr: Expression) = Ln(expr)

    protected[this] def derivative(at: Expression) = 1 / at

    def inverse: Expression = base ^ of

    def isEmpty = false

}

class RealLn(override val of: PositiveReal)
        extends Real
        with Ln {

    require(!of.isEmpty)

    override def isStrictlyPositive = of > One

    def isEmpty = of == One

    override def abs = {
        println(s"ABS OF $this")
        super[Real].abs
    }

    protected[this] def doEvaluate(precision: Precision) = {
        BigDecimalMath.log(of.evaluate(precision).underlying())
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

class ComplexLn(val of: Complex)
        extends ComplexLogarithms {

    require(!of.isEmpty)

    def principal = Complex(Ln(of.abs), of.arg)

    def isEmpty = false

    def inverse: Number = ??? //TODO

}