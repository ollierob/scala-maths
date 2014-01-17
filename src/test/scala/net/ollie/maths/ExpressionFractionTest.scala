package net.ollie.maths

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.numbers.IntegerFraction

/**
 * Created by Ollie on 17/01/14.
 */
class ExpressionFractionTest extends FlatSpec with Matchers {

    val x = Variable("x")

    "1/x" should "replace correctly" in {
        val i: Differentiable = 1 / x
        val j = i.replace(x, 2)
        j.toConstant shouldBe (Some(IntegerFraction(1, 2)))
    }

}
