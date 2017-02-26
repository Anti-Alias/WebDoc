package webdoc


/**
 * Represents any element in a WebDoc file
 * @param name Name of the Element
 * @param meta Mapping of metadata.  Keys are Strings.  Values can be Numbers, Booleans, Strings, and Elements.
 * In particular, the meta key 'children' should map to child Elements.
 */
trait Element
{
  /**
   * @return Children of this element.  Defaults to empty Seq
   * if not found.
   */
  lazy val children:Seq[Any] =
  {
    val found:Option[Any] = meta.get("children")
    found match
    {
      case Some(seq:Seq[_]) => seq
      case Some(a:Any) => Seq.empty
      case None => Seq.empty
    }
  }
  
  /**
   * Name of the Element.  Defaults to empty String if not found.
   */
  lazy val nameTag:String =
  {
    val found:Option[Any] = meta.get("name")
    found match
    {
      case Some(str:String) => str
      case Some(a:Any) => ""
      case None => ""
    }
  }
  
  /**
   * @return True if all children are elements.
   */
  def childrenAreElements:Boolean = children.forall{_.isInstanceOf[Element]}
  
  /**
   * @return Possible size this Element has.
   * None if property is not set.
   */
  def size:Option[Size] = Size.evaluate(meta.get("children"))
  
  /**
   * Metadata.  Determines information about Element.
   */
  def meta:Map[String, Any]
  
  /**
   * Acquires metadata from Element.
   */
  def apply(key:String):Any = meta.apply(key)
  
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
   * String representation of this Element.
   */
  override def toString:String =
  {
    // Creates builder
    var builder = new StringBuilder()

    // Creates helpful flags
    val hasName:Boolean = meta.get("name") match
    {
      case Some(str:String) => true
      case Some(a:Any) => false
      case None => false
    }
    val hasChildren:Boolean = meta.get("children") match
    {
      case Some(seq:Seq[_]) => true
      case Some(a:Any) => false
      case None => false
    }
    
    // Name at head only if it exists and is a String.
    if(hasName)
      builder.append(nameTag)
      
    // Appends start of object
    builder.append('(')

    // Meta
    val entries:Seq[(String, Any)] = meta.toSeq
    for(i <- 0 until entries.length)
    {
      // Appends entry as a String
      val entry:(String, Any) = entries(i)
      val key:String = entry._1
      if((key != "name" || !hasName) && (key != "children" || !hasChildren))
      {
        // Appends key/value pair
        builder
          .append(entry._1)
          .append('=')
          .append(toString(entry._2))
            
        // Prints space if not at last element
        if(i != entries.length-1 && entries(i+1)._1 != "children")
          builder.append(' ')
      }
    }

    // Footer
    builder.append(')')
    if(hasChildren)
      builder.append(toString(children))

    // Returns result
    builder.toString
  }
}