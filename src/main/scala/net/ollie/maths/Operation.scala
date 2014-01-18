package net.ollie.maths

/**
 * Created by Ollie on 18/01/14.
 */
object Operation {

    def undefined = throw new UndefinedOperationException

    def ?!? = undefined

}

class UndefinedOperationException extends Exception
