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

    def series[T <: Differentiable](terms: Iterable[T]): Differentiable = terms.filter(!_.isEmpty) match {
        case Nil => Zero
        case term :: Nil => term
        case otherwise => new DifferentiableSeries(otherwise.toSeq)
    }

    def product(left: Differentiable, right: Differentiable): Differentiable = if (left.isEmpty || right.isEmpty) Zero else product(Seq(left, right))

    def product[T <: Differentiable](terms: Seq[T]): Differentiable = terms match {
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

class DifferentiableSeries[+T <: Differentiable](override val terms: Seq[T])
        extends Series(terms)
        with Differentiable {

    def df(x: Variable) = Differentiable.series(terms.map(_.df(x)))

    override def +(that: Expression) = that match {
        case d: Differentiable => this + d
        case _ => super.*(that)
    }

    override def +(that: Differentiable) = that match {
        case _ if that.isEmpty => this
        case s: DifferentiableSeries[_] => Differentiable.series(terms ++: s.terms)
        case _ => Differentiable.series(terms :+ that)
    }

}

class DifferentiableProduct[+T <: Differentiable](val terms: Seq[T])
        extends Differentiable
        with Aggregate {

    def df(x: Variable) = {
        var sum: Differentiable = Zero
        for (i <- 0 to terms.length - 1) {
            val d: Differentiable = terms(i).df(x)
            if (!d.isEmpty) sum = sum + Differentiable.product(terms.updated(i, d))
        }
        sum
    }

    override def *(that: Expression) = that match {
        case d: Differentiable => this * d
        case _ => super.*(that)
    }

    override def *(that: Differentiable) = that match {
        case _ if that.isEmpty => Zero
        case p: DifferentiableProduct[_] => Differentiable.product(terms ++: p.terms)
        case _ => Differentiable.product(terms :+ that)
    }

    protected[this] def apply(expressions: Seq[Expression]) = Product(expressions)

    private lazy val product = Product(terms)

    def toConstant = product.toConstant

    def isEmpty = product.isEmpty

    override def toString = s"d$product"

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