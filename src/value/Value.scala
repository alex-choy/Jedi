package value

/*
 * Notes:
 * values = numbers | booleans | notifications | environments | etc.
 * values are produced by executing expressions.
 * Example: executing the expression 2 + 3 produces the value 5
 * Expressions are created by programmers and are strings, values
 * are created by computers and are binary.
 */
trait Value extends java.io.Serializable 