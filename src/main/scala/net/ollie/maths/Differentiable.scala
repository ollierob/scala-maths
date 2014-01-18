package net.ollie.maths

import net.ollie.maths.functions.Represented
import net.ollie.maths.functions.numeric.Ln
import net.ollie.maths.numbers.Zero

/**
 * An expression that can be differentiated with respect to a particular variable.
 * Created by Ollie on 03/01/14.
 */
trait Differentiable
        extends Expression {

    override def unary_-(): Differentiable = Differentiable.negate(this)

    override def +(that: Expression) = that match {
        case d: Differentiable => this + d
        case _ => super.+(that)
    }

    def +(that: Differentiable): Differentiable = Differentiable.series(this, that)

    def -(that: Differentiable): Differentiable = this + (-that)

    override def *(that: Expression) = that match {
        case d: Differentiable => this * d
        case _ => super.*(that)
    }

    def *(that: Differentiable): Differentiable = Differentiable.product(this, that)

    def /(that: Differentiable): Differentiable = Differentiable.fraction(this, that)

    def ^(that: Differentiable): Differentiable = Differentiable.power(this, that)

    def df(x: Variable): Differentiable

}

object Differentiable {

    def negate(diff: Differentiable): Differentiable = new DifferentiableNegatedExpression(diff)

    def series(left: Differentiable, right: Differentiable): Differentiable = if (left.isEmpty) right else if (left.isEmpty) right else new DifferentiableSeries(Seq(left, right))

    def series(terms: Seq[Differentiable]): Differentiable = terms.filter(!_.isEmpty) match {
        case Nil => Zero
        case term :: Nil => term
        case otherwise => new DifferentiableSeries(otherwise)
    }

    def product(left: Differentiable, right: Differentiable): Differentiable = if (left.isEmpty || right.isEmpty) Zero else product(Seq(left, right))

    def product(terms: Seq[Differentiable]): Differentiable = terms match {
        case Nil => Zero
        case diff :: Nil => diff
        case _ => new DifferentiableProduct(terms)
    }

    def fraction(numerator: Differentiable, denominator: Differentiable): Differentiable = (numerator, denominator) match {
        case _ if numerator.isEmpty => Zero
        case (frac: DifferentiableFraction, _) => frac * denominator
        case _ => new DifferentiableFraction(numerator, denominator)
    }

    def power(base: Differentiable, power: Differentiable): Differentiable = new DifferentiablePower(base, power)

}

class DifferentiableNegatedExpression(of: Differentiable)
        extends NegatedExpression(of)
        with Differentiable {

    def df(x: Variable) = -(of.df(x))

}

class DifferentiableSeries(terms: Seq[Differentiable])
        extends Differentiable {

    def df(x: Variable) = Differentiable.series(terms.map(_.df(x)))

    private final val series = Series(terms)

    def replace(variables: Map[Variable, Expression]) = series.replace(variables)

    def toConstant = series.toConstant

    def variables = series.variables

    def isEmpty = series.isEmpty

}

class DifferentiableProduct(expressions: Seq[Differentiable])
        extends Differentiable {

    require(!expressions.isEmpty)

    def df(x: Variable) = {
        var sum: Differentiable = Zero
        for (i <- 0 to expressions.length - 1) {
            val d: Differentiable = expressions(i).df(x)
            if (!d.isEmpty) sum = sum + Differentiable.product(expressions.updated(i, d))
        }
        sum
    }

    private final val product = Product(expressions)

    def replace(variables: Map[Variable, Expression]) = product.replace(variables)

    def toConstant = product.toConstant

    def variables = product.variables

    def isEmpty = product.isEmpty

    override def *(that: Expression) = product * that

    override def *(that: Differentiable) = if (that.isEmpty) Zero else Differentiable.product(expressions :+ that)

    override def toString = product.toString

}

class DifferentiableFraction(override val numerator: Differentiable, override val denominator: Differentiable)
        extends ExpressionFraction(numerator, denominator)
        with Differentiable {

    def df(x: Variable) = (numerator.df(x) / denominator) - ((numerator * denominator.df(x)) / (denominator * denominator))

    override def unary_-() = (-numerator) / denominator

    override def *(that: Differentiable) = that match {
        case _ if that.isEmpty => Zero
        case frac: DifferentiableFraction => Differentiable.fraction(numerator * frac.numerator, denominator * frac.denominator)
        case _ => Differentiable.fraction(numerator * that, denominator)
    }

    override def /(that: Differentiable) = Differentiable.fraction(numerator, denominator * that)

}

class DifferentiablePower(base: Differentiable, power: Differentiable)
        extends Power(base, power)
        with Differentiable {

    def df(x: Variable): Differentiable = (base ^ (power - 1)) * ((base.df(x) * power) + (base * Ln(power))) * power.df(x)

}

trait DifferentiableComposite
        extends Differentiable
        with Composite {

    protected[this] override def of: Differentiable

    def df(x: Variable): Differentiable = of.df(x) * df(of)

    protected[this] def df(of: Differentiable): Differentiable

}

trait DifferentiableRepresented
        extends Differentiable
        with Represented {

    protected[this] override def f: Differentiable

    def df(x: Variable) = f.df(x)

}