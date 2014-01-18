scala-maths
===========

Symbolic maths library.

Build up expressions in a human-readable manner:

    val x = Variable("x")
    val ex = (2 * Ln(x)) + x
    val df = ex.df(x)
    val sin = Sin(df)
    val n = sin.replace(x, Pi).toConstant.get
    n.evaluate(4 dp) shouldBe(BigDecimal("0.9978"))
