package net.ollie.maths

import net.ollie.maths.numbers.{IntegerNumber, One, Zero}

/**
 * Created by Ollie on 11/01/14.
 */
trait Aggregate
        extends Expression {

    protected[this] def terms: Seq[Expression]

    def variables = terms.map(_.variables).flatten.toSet

    def replace(variables: Map[Variable, Expression]) = apply(terms.map(_.replace(variables)))

    protected[this] def apply(expressions: Seq[Expression]): Expression

}

object Series {

    def apply(left: Expression, right: Expression): Expression = if (left.isEmpty) right else if (right.isEmpty) left else new Series(Seq(left, right))

    def apply(terms: Seq[Expression]): Expression = terms.filter(!_.isEmpty) match {
        case Nil => Zero
        case expr :: Nil => expr
        case otherwise => new Series(otherwise)
    }

}

class Series(val terms: Seq[Expression])
        extends Aggregate {

    protected[this] def apply(expressions: Seq[Expression]) = Series(expressions)

    def toConstant: Option[Number] = {
        val mapped: Seq[Option[Number]] = terms.map(_.toConstant)
        val zero: Option[Number] = Some(Zero)
        mapped.foldLeft(zero)(add)
    }

    private def add(result: Option[Number], current: Option[Number]): Option[Number] = {
        if (!result.isDefined || !current.isDefined) return None
        val n = result.get ?+ current.get
        if (n.isDefined) n
        else current.get ?+ result.get
    }

    def isEmpty = terms.forall(_.isEmpty)

    override def toString = terms.mkString("Σ(", " + ", ")")

    override def hashCode = terms.hashCode

}

object Product {

    def apply(left: Expression, right: Expression): Expression = if (left.isEmpty || right.isEmpty) Zero else new Product(Seq(left, right))

    def apply(expressions: Seq[Expression]): Expression = expressions match {
        case Nil => Zero
        case expression :: Nil => expression
        case _ => new Product(expressions)
    }

    def apply[N <: Number](f: (IntegerNumber) => N, range: Range)(implicit multiplication: MultiplicationArithmetic[N#System, N#System, N#System]): N#System = {
        var product: N#System = multiplication.one
        for (j <- range) {
            product = multiplication.multiply(product, f(j).narrow)
        }
        product
    }

    def apply[N <: Number](f: (IntegerNumber) => N, min: IntegerNumber, max: IntegerNumber)(implicit multiplication: MultiplicationArithmetic[N#System, N#System, N#System]): N#System = {
        apply(f, min.toInt.get to max.toInt.get)(multiplication)
    }

}

class Product(val terms: Seq[Expression])
        extends Aggregate {

    require(!terms.isEmpty)

    protected[this] def apply(expressions: Seq[Expression]): Expression = Product(expressions)

    def isEmpty = terms.exists(_.isEmpty)

    def toConstant: Option[Number] = {
        val constants = terms.map(_.toConstant)
        val one: Option[Number] = Some(One)
        constants.foldLeft(one)(multiply)
    }

    private def multiply(result: Option[Number], current: Option[Number]): Option[Number] = {
        if (!result.isDefined || !current.isDefined) return None //TODO use option foldLeft?
        val n = result.get ?* current.get
        if (n.isDefined) n
        else current.get ?* result.get
    }

    override def toString = terms.mkString("Π(", " * ", ")")

    override def hashCode = terms.hashCode

}
