package net.ollie.maths

/**
 * Created by Ollie on 18/01/14.
 */
object Operation {

    def undefined = throw new UndefinedOperationException

    def indeterminate = throw new IndeterminateOperationException

    def overflow(message: String) = throw new ArithmeticOverflowException(message)

    def illegal(message: String) = throw new IllegalOperationException(message)

    def ?!? = undefined

}

class UndefinedOperationException extends Exception

class IndeterminateOperationException extends Exception

class ArithmeticOverflowException extends ArithmeticException

class IllegalOperationException(message: String) extends Exception(message)