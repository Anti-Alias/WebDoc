package webdoc


/***
 * Represents a RawElement.  This Element has no functionality, but allows for
 * a programmer traverse its AST.
 */
case class RawElement(meta:Map[String, Any]) extends Element