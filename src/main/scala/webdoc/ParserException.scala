package webdoc
case class ParserException(msg:String, location:(Int, Int)) extends RuntimeException(msg + " at " + location)
