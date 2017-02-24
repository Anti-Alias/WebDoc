package webdoc
case class UnexpectedCharacterException(c:Char, line:Int, col:Int) extends RuntimeException("Unexpected character " + c + " at (" + line + ", " + col + ")")
