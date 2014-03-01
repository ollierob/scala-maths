package net.ollie.maths

import net.ollie.maths.methods.Product
import net.ollie.maths.numbers.{Integer, Natural, Real}
import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner
import net.ollie.maths.numbers.complex.Complex
import net.ollie.maths.numbers.constants.One

/**
 * Created by Ollie on 11/01/14.
 */
@RunWith(classOf[JUnitRunner])
class ProductTest extends FlatSpec with Matchers {

    behavior of "Numeric product"

    it should "evaluate factorial" in {
        def fact(i: Integer): Real = i
        Product(fact, 1, 5) shouldBe (Natural(5) !)
    }

    it should "evaluate binomial coefficient" in {
        val n = Natural(5)
        val k = Natural(3)
        def f(i: Integer): Real = (n - k + i) / i
        Product(f, 1, k) shouldBe (Natural(10))
    }

    behavior of "Expression product"

    it should "convert to constant" in {
        Product(2, 3).toConstant shouldBe (Some(Natural(6)))
    }

    it should "not convert to constant" in {
        val x = Variable("x")
        val product = Product(2, x)
        product.toConstant shouldBe (None)
        product.replace(x, 3).toConstant shouldBe Some(Natural(6))
    }

    it should "convert to constant across different number systems" in {
        val r = Real(5)
        val z = Complex(7, 11)
        val x = Variable("x")
        val y = Variable("y")
        val product = Product(x, y)
        product.replace(Map(x -> r, y -> z)).toConstant shouldBe Some(Complex(35, 55))
    }

    it should "multiply numeric terms" in {
        val product = Product(-1, 5)
        product.toConstant shouldBe (Some(Integer(-5)))
    }

    "x * 1" should "simplify when x is replaced" in {
        val x = Variable("x")
        val product = Product(x, One)
        product.replace(x, One) shouldBe One
    }

    behavior of "x * y * z"

    {

        val x = Variable("x")
        val y = Variable("y")
        val z = Variable("z")
        val p = Product(Seq(x, y, z))

        it should "simplify when variables are replaced" in {
            p.replace(x, 3).replace(y, 1).replace(z, 2).toConstant shouldBe Some(Natural(1 * 2 * 3))
        }

        it should "have variables" in {
            p.variables shouldBe Set(x, y, z)
        }

        it should "replace variables" in {
            p.replace(z, x).variables shouldBe Set(x, y)
            p.replace(z, y).replace(y, x).variables shouldBe Set(x)
        }

        it should "divide by z" in {
            p / z shouldBe x * y
        }

    }

}
