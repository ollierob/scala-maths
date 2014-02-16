package net.ollie.maths.functions.angular

import scala.Some

import net.ollie.maths._
import net.ollie.maths.functions.{BuiltFunction, RealFunctionBuilder, FunctionBuilder}
import net.ollie.maths.numbers.{Integer, Precision, Real}
import net.ollie.maths.numbers.constants.{Pi, One}
import org.nevec.rjm.BigDecimalMath

/**
 * Created by Ollie on 03/01/14.
 */
object Cos
        extends RealFunctionBuilder {

    import Angle._

    def apply(re: Real): Real with Cos = re match {
        case a: Angle => apply(a)
        case _ => apply(re radians)
    }

    def apply(angle: Angle): Real with Cos = new RealCos(angle)

    def unapply(cos: Cos): Option[Expression] = Some(cos.of)

    protected[this] def create(expr: Expression) = new CosOf(expr)

    protected[angular] def empty = One

}

trait Cos
        extends Expression {

    val of: Expression

    override def toString = s"Cos($of)"

}

class CosOf(val of: Expression)
        extends BuiltFunction
        with Cos
        with Invertible {

    protected[this] def builder = Cos

    protected[this] def derivative(x: Expression) = -Sin(x)

    def inverse = ArcCos(of)

    def isEmpty = false

    override def equals(that: Expression) = that match {
        case Cos(y) => of == y || super.equals(that)
        case _ => super.equals(that)
    }

    override def toString = s"Cos($of)"

}

class RealCos(override val of: Angle)
        extends Real
        with Cos {

    private lazy val reduced = of.reduce

    //private lazy val series = MaclaurinSeries(Cos, of.toRadians)

    protected[this] def doEvaluate(precision: Precision) = {
        //series.evaluate(precision)
        precision(BigDecimalMath.cos(of.evaluate(precision).underlying()))
    }

    private lazy val empty: Boolean = {
        (2 * of / Pi) match {
            case i: Integer => !i.isEven
            case _ => false
        }
    }

    def isEmpty = empty

    override def inverse = super[Real].inverse

    override def variables = super[Real].variables

    override def toConstant = Some(this)

}

object Sec
        extends FunctionBuilder {

    def apply(n: Number) = Cos(n).inverse

    def apply(re: Real): Real = Cos(re).inverse

    def apply(re: Angle): Real = Cos(re).inverse

    protected[this] def create(expr: Expression) = 1 / Cos(expr)

    protected[this] def empty = Cos.empty.inverse

}
