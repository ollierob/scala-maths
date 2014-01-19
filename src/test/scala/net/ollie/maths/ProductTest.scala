package net.ollie.maths

import net.ollie.maths.numbers.{IntegerNumber, NaturalNumber, RealNumber}
import org.scalatest.{FlatSpec, Matchers}
import net.ollie.maths.numbers.complex.ComplexNumber
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import net.ollie.maths.methods.Product

/**
 * Created by Ollie on 11/01/14.
 */
@RunWith(classOf[JUnitRunner])
class ProductTest extends FlatSpec with Matchers {

    behavior of "Numeric product"

    it should "evaluate factorial" in {
        def fact(i: IntegerNumber): RealNumber = i
        Product(fact, 1, 5) shouldBe (NaturalNumber(5) !)
    }

    it should "evaluate binomial coefficient" in {
        val n = NaturalNumber(5)
        val k = NaturalNumber(3)
        def f(i: IntegerNumber): RealNumber = (n - k + i) / i
        Product(f, 1, k) shouldBe (NaturalNumber(10))
    }

    behavior of "Expression product"

    it should "convert to constant" in {
        Product(2, 3).toConstant shouldBe (Some(NaturalNumber(6)))
    }

    it should "not convert to constant" in {
        val x = Variable("x")
        val product = Product(2, x)
        product.toConstant shouldBe (None)
        product.replace(x, 3).toConstant shouldBe (Some(NaturalNumber(6)))
    }

    it should "convert to constant across different number systems" in {
        val r = RealNumber(5)
        val z = ComplexNumber(7, 11)
        val x = Variable("x")
        val y = Variable("y")
        val product = Product(x, y)
        product.replace(Map(x -> r, y -> z)).toConstant shouldBe (Some(ComplexNumber(35, 55)))
    }

    it should "multiply numeric terms" in {
        val product = Product(-1, 5)
        product.toConstant shouldBe (Some(IntegerNumber(-5)))
    }

}
