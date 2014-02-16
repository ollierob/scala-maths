package net.ollie.maths.methods

import net.ollie.maths.{Expression, Variable}
import net.ollie.maths.numbers.{Real, Integer, Natural}
import org.scalatest.{FlatSpec, Matchers}
import net.ollie.maths.numbers.complex.Complex

/**
 * Created by Ollie on 19/01/14.
 */
class SeriesTest extends FlatSpec with Matchers {

    val x = Variable("x")
    val y = Variable("y")

    behavior of "x + y"

    {

        val series = Series(Seq(x, y))

        it should "not be a constant" in {
            series.toConstant shouldBe None
        }

        it should "convert to constant" in {
            val r = Real(3)
            val z = Complex(5, 7)
            series.replace(Map(x -> r, y -> z)).toConstant shouldBe (Some(Complex(8, 7)))
            series.replace(Map(x -> z, y -> r)).toConstant shouldBe (Some(Complex(8, 7)))
        }

        it should "not convert to constant" in {
            val r = Real(3)
            series.replace(x, r).toConstant shouldBe (None)
            series.replace(y, r).toConstant shouldBe (None)
            series.replace(Map(x -> r, y -> r)).toConstant shouldBe (Some(r + r))
        }

        it should "add z" in {
            val z = Variable("z")
            series + z shouldBe Series(Seq(x, y, z))
        }

        it should "add to z" in {
            val z = Variable("z")
            z + series shouldBe Series(Seq(z, x, y))
        }

    }

    "Sum of degree*x degree 0 to 5" should "be 15*x" in {
        def f(n: Integer): Expression = n * x
        val sum = Series(f, 0, 5)
        println(sum)
        sum.isEmpty shouldBe (false)
        sum.variables shouldBe (Set(x))
        sum.df(x).toConstant shouldBe (Some(Natural(15)))
        sum.replace(x, 1).toConstant shouldBe (Some(Natural(15)))
        sum.replace(x, 2).toConstant shouldBe (Some(Natural(30)))
    }

    behavior of "x + y + z"

    {

        val z = Variable("z")

        it should "be formed by adding z to x + y" in {
            val series = Series(Seq(x, y)) + z
            series shouldBe Series(Seq(x, y, z))
        }

        val series = Series(Seq(x, y, z))

        it should "simplify if x and z are replaced" in {
            val replaced = series.replace(Map(x -> 1, z -> 2))
            replaced should (be(3 + y) or be(1 + y + 2))
        }

    }

}
