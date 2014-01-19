package net.ollie.maths.functions.numeric

import net.ollie.maths.{Differentiable, Variable}
import net.ollie.maths.numbers.{IntegerNumber, NaturalNumber}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 19/01/14.
 */
class SumTest extends FlatSpec with Matchers {

    "Sum of n*x from 0 to 5" should "be 15*x" in {
        val x = Variable("x")
        def f(n: IntegerNumber): Differentiable = n * x
        val sum = Sum(f, 0, 5)
        println(sum)
        sum.isEmpty shouldBe (false)
        sum.variables shouldBe (Set(x))
        sum.df(x).toConstant shouldBe (Some(NaturalNumber(15)))
        sum.replace(x, 1).toConstant shouldBe (Some(NaturalNumber(15)))
        sum.replace(x, 2).toConstant shouldBe (Some(NaturalNumber(30)))
    }

}
