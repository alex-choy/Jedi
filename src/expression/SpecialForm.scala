package expression

/*
 * Notes:
 * syntax: special-form = conjunction | disjunction | conditional | declaration
 * special forms are expressions with "specialized" execution algorithms.
 */
trait SpecialForm extends Expression