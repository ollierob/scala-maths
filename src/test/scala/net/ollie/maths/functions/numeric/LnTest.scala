package net.ollie.maths.functions.numeric

import net.ollie.maths.Variable
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.complex.{Complex, ImaginaryUnit}
import net.ollie.maths.numbers.constants.{EulersNumber, One, Pi, Zero}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import net.ollie.maths.numbers.Precision._

/**
 * Created by Ollie on 16/01/14.
 */
class LnTest extends AnyFlatSpec with Matchers {

    behavior of "Ln(1)"

    {

        it should "be 0" in {
            Ln(One) shouldBe Zero
        }

    }

    behavior of "Ln(2)"

    {
        
        it should "be 0" in {
            Ln(2).evaluate(4 dp) shouldBe BigDecimal("0.6931")
        }

    }

    behavior of "Ln(4)"

    {

        it should "evaluate" in {
            Ln(4).evaluate(4 dp) shouldBe BigDecimal("1.3863")
        }

        it should "be positive" in {
            Ln(4).isPositive shouldBe true
            Ln(4) > 1 shouldBe true
        }

        it should "not be empty" in {
            Ln(4).isEmpty shouldBe false
        }

    }

    "1 / Ln(4)" should "evaluate" in {
        val r = Ln(4).inverse
        r.evaluate(4 dp) shouldBe BigDecimal("0.7213")
    }

    "4 * Ln(4)" should "evaluate" in {
        val r = 4 * Ln(4)
        r.evaluate(4 dp) shouldBe BigDecimal("5.5452")
    }

    "1 / (4 * Ln(4))" should "evaluate" in {
        val r = One / (4 * Ln(4))
        r.evaluate(4 dp) shouldBe BigDecimal("0.1803")
    }

    val x = Variable("x")

    behavior of "Ln(Ln(x))"

    {

        val f = Ln(Ln(x))

        it should "evaluate e" in {
            f.replace(x, EulersNumber).toConstant shouldBe Some(Zero)
        }

        it should "differentiate" in {
            val df = f.df(x)
            df.replace(x, 4).toConstant shouldBe Some(1 / (4 * Ln(4)))
            val n = 1 / (4 * Ln(4))
        }

        "Ln(Ln(4))" should "evaluate" in {
            val r: Complex = Ln(Ln(4))
            val re: Real = Real(r).get
            re.evaluate(4 dp) shouldBe BigDecimal("0.3266")
        }

    }

    "Ln(2x)" should "differentiate to 1/x" in {
        val two = Integer(2)
        val ln = Ln(two * x)
        ln.df(x).replace(x, One).toConstant shouldBe (Some(One))
    }

    "Ln(Exp(x))" should "be x" in {
        Ln(Exp(x)) shouldBe x
    }

    "Ln(i)" should "equal i Pi/2" in {
        val z: ComplexLogarithms = Ln(ImaginaryUnit)
        z.principal shouldBe Complex(Zero, Pi / 2)
    }

}
