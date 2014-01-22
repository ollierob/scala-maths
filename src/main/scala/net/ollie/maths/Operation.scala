package net.ollie.maths

/**
 * Created by Ollie on 18/01/14.
 */
object Operation {

    def undefined = throw new UndefinedOperationException

    def indeterminate = throw new IndeterminateOperationException

    def illegal(message: String) = throw new IllegalOperationException(message)

    def ?!? = undefined

}

class UndefinedOperationException extends Exception

class IndeterminateOperationException extends Exception

class IllegalOperationException(message: String) extends Exception(message)