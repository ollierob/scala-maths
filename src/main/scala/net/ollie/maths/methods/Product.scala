package net.ollie.maths.methods

import net.ollie.maths._
import net.ollie.maths.numbers.{IntegerNumber, One, Zero}
import scala.Some

/**
 * Created by Ollie on 19/01/14.
 */
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

class Product[+T <: Expression](val terms: Seq[T])
        extends Aggregate {

    require(!terms.isEmpty)

    override def *(that: Expression): Expression = terms match {
        case p: Product[_] => Product(terms ++: p.terms)
        case _ => Product(terms :+ that)
    }

    protected[this] def apply(expressions: Seq[Expression]) = Product(expressions)

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

    def df(x: Variable) = {
        var sum: Expression = Zero
        for (i <- 0 to terms.length - 1) {
            val d: Expression = terms(i).df(x)
            if (!d.isEmpty) sum = sum + Product(terms.updated(i, d))
        }
        sum
    }

    override def toString = terms.mkString("(", " * ", ")")

    override def hashCode = terms.hashCode

}