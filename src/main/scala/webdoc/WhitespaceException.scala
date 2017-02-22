package webdoc

/**
 * Exception thrown when an unexpected whitespace character is found.
 * @param line Line it was found on.
 * @param column Column it was found on.
 */
class WhitespaceException(line:Int, column:Int) extends RuntimeException("Unexpected whitespace: (" + line + ", " + column + ")")