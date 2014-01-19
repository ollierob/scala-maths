package net.ollie.maths

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.numbers.{IntegerNumber, NaturalNumber}

/**
 * Created by Ollie on 19/01/14.
 */
class ExpressionTest extends FlatSpec with Matchers {

    val x: Variable = Variable("x")

    "5 - 2x" should "reduce to constant" in {
        val ex: Expression = 5 - (2 * x.asInstanceOf[Expression])
        ex.replace(x, 1).toConstant shouldBe (Some(NaturalNumber(3)))
        (-ex).replace(x, 1).toConstant shouldBe (Some(IntegerNumber(-3)))
    }

}
