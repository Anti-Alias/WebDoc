package webdoc
/**
 * Thrown when an unexpected character was encountered
 * @param c Chararacter encountered.
 * @param location Location in  file that c was encountered (row, column)
 */
case class UnexpectedCharacterException(c:Char, location:(Int, Int)) extends RuntimeException("Unexpected character " + c + " at (" + location._1 + ", " + location._2 + ")")
