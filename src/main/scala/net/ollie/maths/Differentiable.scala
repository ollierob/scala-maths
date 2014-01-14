package net.ollie.maths

import net.ollie.maths.functions.Represented
import net.ollie.maths.numbers.Zero

/**
 * Created by Ollie on 03/01/14.
 */
trait Differentiable extends Expression {

    def +(that: Differentiable): Differentiable = DifferentiableSeries(this, that)

    def -(that: Differentiable): Differentiable = this + (-that)

    override def *(that: Expression) = that match {
        case d: Differentiable => this * d
        case _ => super.*(that)
    }

    def *(that: Differentiable): Differentiable = DifferentiableProduct(this, that)

    def /(that: Differentiable): Differentiable = ???

    def ^(that: Differentiable): Differentiable = ???

    def df(x: Variable): Differentiable

    override def unary_-(): Differentiable = new DifferentiableNegatedExpression(this)

}

object Differentiable {

    implicit def convert(n: Number): Differentiable = new Differentiable {

        def df(x: Variable) = Zero

        def isEmpty = n.isEmpty

        def replace(variables: Map[Variable, Expression]) = n.replace(variables)

        def toConstant = Some(n)

        def variables = n.variables

        override def toString = n.toString

    }

}

class DifferentiableNegatedExpression(of: Differentiable)
        extends NegatedExpression(of)
        with Differentiable {

    def df(x: Variable) = -(of.df(x))

}

trait DifferentiableComposite
        extends Differentiable
        with Composite {

    protected[this] override def of: Differentiable

    def df(x: Variable) = of.df(x) * df(of)

    protected[this] def df(of: Differentiable): Differentiable

}

trait DifferentiableRepresented
        extends Differentiable
        with Represented {

    protected[this] override def f: Differentiable

    def df(x: Variable) = f.df(x)

}

trait DifferentiableUnivariate
        extends Differentiable
        with Univariate {

    override def unary_-(): DifferentiableUnivariate = new DifferentiableNegatedExpression(this) with DifferentiableUnivariate {

        override def unary_-() = DifferentiableUnivariate.this

        override def variable: Variable = DifferentiableUnivariate.this.variable

        override def variables = super[DifferentiableUnivariate].variables

        override def df(x: Variable): DifferentiableUnivariate = -(DifferentiableUnivariate.this.df(x))

        override def toString = "-(" + DifferentiableUnivariate.this.toString + ")"

    }

    def df(x: Variable): DifferentiableUnivariate

    def dx: DifferentiableUnivariate = df(variable)

}

object DifferentiableSeries {

    def apply(left: Differentiable, right: Differentiable): Differentiable = if (left.isEmpty) right else if (left.isEmpty) right else new DifferentiableSeries(Seq(left, right))

    def apply(terms: Seq[Differentiable]): Differentiable = terms.filter(!_.isEmpty) match {
        case Nil => Zero
        case term :: Nil => term
        case otherwise => new DifferentiableSeries(otherwise)
    }

}

class DifferentiableSeries(terms: Seq[Differentiable])
        extends Differentiable {

    def df(x: Variable) = DifferentiableSeries(terms.map(_.df(x)))

    private final val series = Series(terms)

    def replace(variables: Map[Variable, Expression]) = series.replace(variables)

    def toConstant = series.toConstant

    def variables = series.variables

    def isEmpty = series.isEmpty

}

object DifferentiableProduct {

    def apply(left: Differentiable, right: Differentiable): Differentiable = if (left.isEmpty || right.isEmpty) Zero else apply(Seq(left, right))

    def apply(terms: Seq[Differentiable]): Differentiable = terms match {
        case Nil => Zero
        case diff :: Nil => diff
        case _ => new DifferentiableProduct(terms)
    }

}

class DifferentiableProduct(expressions: Seq[Differentiable])
        extends Differentiable {

    require(!expressions.isEmpty)

    def df(x: Variable) = {
        var sum: Differentiable = Zero
        for (i <- 0 to expressions.length - 1) {
            val d: Differentiable = expressions(i).df(x)
            if (!d.isEmpty) sum = sum + DifferentiableProduct(expressions.updated(i, d))
        }
        sum
    }

    private final val product = Product(expressions)

    def replace(variables: Map[Variable, Expression]) = product.replace(variables)

    def toConstant = product.toConstant

    def variables = product.variables

    def isEmpty = product.isEmpty

    override def *(that: Expression) = product * that

    override def *(that: Differentiable) = if (that.isEmpty) Zero else DifferentiableProduct(expressions :+ that)

    override def toString = product.toString

}