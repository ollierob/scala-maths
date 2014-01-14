package net.ollie.maths.numbers.complex

import net.ollie.maths.numbers.RealNumber
import net.ollie.maths.functions.numeric.PositiveSquareRoot

/**
 * Created by Ollie on 12/01/14.
 */
class CartesianComplexNumber(val re: RealNumber, val im: ImaginaryNumber)
        extends ComplexNumber {

    def abs = PositiveSquareRoot(re.squared + im.coefficient.squared)

    def arg = ???

}
