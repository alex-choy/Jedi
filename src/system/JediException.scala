package system

class JediException(val gripe: String = "Jedi error: ") 
  extends Exception(gripe) 