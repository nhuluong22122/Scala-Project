package expression

import value._ //telling the compiler go see if you can find it in this package

trait Expression {
    def execute(env: Environment): Value
}