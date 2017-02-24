package webdoc
import scala.collection.immutable.{Seq, Map}


/**
 * Represents any element in a WebDoc file
 * @param name Name of the Element
 * @param meta Mapping of metadata.  Keys are Strings.  Values can be Numbers, Booleans, Strings, and Elements.
 * In particular, the meta key 'children' should map to child Elements.
 */
case class Element(name:String, meta:Map[String, Any])
{
    override def toString:String =
    {
        // Creates builder
        var builder = new StringBuilder()

        // Header
        builder.append(name)
        builder.append('(')

        // Meta
        for(entry <- meta)
        {
            builder
                .append(entry._1)
                .append(" = ")
                .append(entry._2.toString)
                .append(' ')
        }

        // Footer
        builder.append(')')

        // Returns result
        builder.toString
    }
}
