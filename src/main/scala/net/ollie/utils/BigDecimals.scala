package net.ollie.utils

import java.math

import ch.obermuhlner.math.big.BigDecimalMath
import net.ollie.maths.numbers.Precision

object BigDecimals {

    def decimalDigits(bd: BigDecimal): Int = {
        if (bd.scale == 0) return 0
        val str = bd.underlying().toPlainString
        val dot = str.indexOf('.')
        if (dot < 0) 0
        else str.length - dot - 1
    }

    def nonDecimalDigits(bd: BigDecimal): Int = {
        if (bd < 0) return nonDecimalDigits(-bd)
        val str = bd.underlying().toPlainString
        val dot = str.indexOf('.')
        if (dot < 0) str.length
        else if (dot == 1 && str.charAt(0) == '0') 0
        else str.length - dot
    }

    def logE(bd: BigDecimal, precision: Precision): BigDecimal = {
        precision.applyTo(bd.underlying(), (bd, mc) => BigDecimalMath.log(bd.underlying(), mc))
    }

    def sin(bd: BigDecimal, precision: Precision): BigDecimal = {
        precision.applyTo(bd.underlying(), (bd, mc) => BigDecimalMath.sin(bd.underlying(), mc))
    }

    def asin(bd: BigDecimal, precision: Precision): BigDecimal = {
        precision.applyTo(bd.underlying(), (bd, mc) => BigDecimalMath.asin(bd, mc))
    }

    def cos(bd: BigDecimal, precision: Precision): BigDecimal = {
        precision.applyTo(bd.underlying(), (bd, mc) => BigDecimalMath.cos(bd, mc))
    }

    def acos(bd: BigDecimal, precision: Precision): BigDecimal = {
        precision.applyTo(bd, (bd, mc) => BigDecimalMath.acos(bd, mc))
    }

    def atan(bd: BigDecimal, precision: Precision): BigDecimal = {
        precision.applyTo(bd, (bd, mc) => BigDecimalMath.atan(bd, mc))
    }

    def pow(base: BigDecimal, power: BigDecimal, precision: Precision): BigDecimal = {
        precision.applyTo(base, (bd, mc) => BigDecimalMath.pow(bd, power, mc))
    }

    def sqrt(base: BigDecimal, precision: Precision): BigDecimal = {
        precision.applyTo(base, (bd, mc) => BigDecimalMath.sqrt(bd, mc))
    }

    def root(base: BigDecimal, power: BigDecimal, precision: Precision): BigDecimal = {
        precision.applyTo(base, (bd, mc) => BigDecimalMath.root(bd, power, mc))
    }

    private implicit def apply(bd: math.BigDecimal): BigDecimal = BigDecimal(bd)

    private implicit def unapply(bd: BigDecimal): math.BigDecimal = bd.underlying()

}
