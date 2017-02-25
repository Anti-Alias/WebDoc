package webdoc


/**
 * Represents any element in a WebDoc file
 * @param name Name of the Element
 * @param meta Mapping of metadata.  Keys are Strings.  Values can be Numbers, Booleans, Strings, and Elements.
 * In particular, the meta key 'children' should map to child Elements.
 */
case class Element(name:String, meta:Map[String, Any])
{
  /**
   * toString helper method
   */
  private def toString(a:Any):String = a match
  {
    case d:Double =>
      val i:Int = d.toInt
      if(i == d) i.toString
      else d.toString
    case str:String => "\"" + a + "\""
    case seq:Seq[_] =>
      val builder = new StringBuilder()
      builder.append('[')
      for(i <- 0 until seq.length)
      {
        val elem:Any = seq(i)
        builder.append(toString(elem))
        if(i != seq.length-1)
          builder.append(' ')
      }
      builder.append(']')
      builder.toString
    case a:Any => a.toString
  }
  
  /**
   * Acquires metadata from Element.
   */
  def apply(key:String):Any = meta.apply(key)
  
  /**
   * String representation of this Element.
   */
  override def toString:String =
  {
    // Creates builder
    var builder = new StringBuilder()

    // Header
    builder.append(name)
    builder.append('(')

    // Meta
    val entries:Seq[(String, Any)] = meta.toSeq
    for(i <- 0 until entries.length)
    {
      // Appends entry as a String
      val entry:(String, Any) = entries(i)
      builder
        .append(entry._1)
        .append('=')
        .append(toString(entry._2))
      
      // Prints space if not at last element
      if(i != entries.length-1)
        builder.append(' ')
    }

    // Footer
    builder.append(')')

    // Returns result
    builder.toString
  }
}
