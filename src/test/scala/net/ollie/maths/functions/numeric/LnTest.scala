package net.ollie.maths.functions.numeric

import org.scalatest.{Matchers, FlatSpec}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import net.ollie.maths.{Number, Variable}
import net.ollie.maths.numbers.{One, IntegerNumber, RealNumber}

/**
 * Created by Ollie on 16/01/14.
 */
@RunWith(classOf[JUnitRunner])
class LnTest extends FlatSpec with Matchers {

    import net.ollie.maths.numbers.Precision._

    val x = Variable("x")

    "Ln(5)" should "evaluate" in {
        val r = Ln(5)
        r.evaluate(4 dp) shouldBe (BigDecimal("1.6094"))
    }

    "Ln(Ln(x))" should "differentiate" in {
        val r = Ln(Ln(x))
        val df = r.df(x) //Should be 1/(x ln(x))
        val replaced: Number = df.replace(x, 4).toConstant.get
        val re: RealNumber = replaced.asInstanceOf[RealNumber]
        re.evaluate(4 dp) shouldBe (BigDecimal("0.1803"))
    }

    "Ln(2x)" should "differentiate to 1/x" in {
        val two = IntegerNumber(2)
        val ln = Ln(two * x)
        ln.df(x).replace(x, One).toConstant shouldBe (Some(One))
    }

    "Ln(Exp(x))" should "be x" in {
        Ln(Exp(x)) shouldBe (x)
    }

}
