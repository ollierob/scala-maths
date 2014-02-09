package net.ollie.maths

import net.ollie.maths.numbers.{Integer, Natural}
import org.scalatest.{FlatSpec, Matchers}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import net.ollie.maths.numbers.constants.{Zero, One}

/**
 * Created by Ollie on 19/01/14.
 */
@RunWith(classOf[JUnitRunner])
class ExpressionTest extends FlatSpec with Matchers {

    val x: Variable = Variable("x")

    "5 - 2x" should "reduce to constant" in {
        val ex: Expression = 5 - (2 * x.asInstanceOf[Expression])
        ex.replace(x, 1).toConstant shouldBe (Some(Natural(3)))
        (-ex).replace(x, 1).toConstant shouldBe (Some(Integer(-3)))
    }

    "x / 2" should "reduce to a constant" in {
        val ex: Expression = x / 2
        ex.replace(x, 0).toConstant shouldBe Some(Zero)
        ex.replace(x, 2).toConstant shouldBe Some(One)
    }

}
