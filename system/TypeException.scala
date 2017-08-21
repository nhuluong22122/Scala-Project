package system
import expression._
import value._
class TypeException(name: String) extends JediException("Type Exception: " + name)