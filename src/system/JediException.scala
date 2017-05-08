package system

/*
 * Notes:
 * Jedi exceptions = user errors =
 * undefined identifier | syntax error | type error
 */
class JediException(val gripe: String = "Jedi error ") extends Exception(gripe)