package net.ollie.maths.numbers.quaternion

import net.ollie.maths.numbers.One
import net.ollie.maths.numbers.complex.ComplexNumber
import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner

/**
 * Created by Ollie on 11/01/14.
 */
@RunWith(classOf[JUnitRunner])
class QuaternionTest extends FlatSpec with Matchers {

    val i = Quaternion.i(1)
    val j = Quaternion.j(1)
    val k = Quaternion.k(1)

    behavior of "i"

    it should "multiply by i" in {
        i * i shouldBe (-One)
    }

    it should "multiply by j" in {
        i * j shouldBe (k)
    }

    it should "multiply by k" in {
        i * k shouldBe (-j)
    }

    behavior of "j"

    it should "multiply by i" in {
        j * i shouldBe (-k)
    }

    "i * j * k" should "be -1" in {
        i * j * k shouldBe (-One)
    }

    behavior of "2 + 3i + 5j + 7j"

    {

        val q = Quaternion(2, 3, 5, 7)

        it should "equal itself" in {
            (q == q) shouldBe (true)
        }

        it should "add 11 + 13i" in {
            val i = ComplexNumber(11, 13)
            (q + i) shouldBe (Quaternion(13, 16, 5, 7))
            (i + q) shouldBe (q + i)
        }

        it should "multiply by 11 + 13i" in {
            val i = ComplexNumber(11, 13)
            (q * i) shouldBe (Quaternion(-17, 59, 146, 12))
        }

        it should "add 11 + 13i + 17j + 19k" in {
            val q2 = Quaternion(11, 13, 17, 19)
            q + q2 shouldBe (Quaternion(13, 18, 22, 26))
        }

        it should "multiply by 11 + 13i + 17j + 19k" in {
            val q2 = Quaternion(11, 13, 17, 19)
            q * q2 shouldBe (Quaternion(-235, 35, 123, 101))
        }

    }

}
