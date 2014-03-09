package net.ollie.maths.equations

import net.ollie.maths.{Variable, Expression}

/**
 * Created by Ollie on 09/03/14.
 */
trait EquationSystem {

    def equations: Seq[Equation]

    def variables = equations.map(_.variables).flatten.toSet

    def replace(variables: Map[Variable, Expression])(implicit builder: EquationSystemBuilder): EquationSystem = {
        val replaced = equations.map(_.replace(variables))
        builder(replaced)
    }

    def trySolve: Option[Solution]

}

trait EquationSystemBuilder {

    def apply(equations: Seq[Equation]): EquationSystem

}