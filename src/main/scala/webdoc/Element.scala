package webdoc
import scala.collection.immutable.{Seq, Map}


/**
 * Represents any element in a WebDoc file
 */
case class Element(name:String, meta:Map[String, Element], children:Seq[Element])