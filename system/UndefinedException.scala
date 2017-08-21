package system

import expression._

class UndefinedException(name: Identifier) extends JediException("Underfined identifier: " + name.name) 