package webdoc


/**
* Represents the Size of an Element.
*/
case class Size(width:Double, height:Double) extends Seq[Double]
{
  /**
   * Accessor for Size
   */
  override def apply(index:Int):Double = index match
  {
    case 0 => width
    case 1 => height
    case _ => throw new IndexOutOfBoundsException(index + " out of bounds [0, 2)")
  }
  
  /**
   * Iterator for Size
   */
  override def iterator:Iterator[Double] = new Iterator[Double]
  {
    var index:Int = 0
    override def next:Double =
    {
      val result:Double = apply(index)
      index += 1
      result
    }
    override def hasNext:Boolean = index != 2
  }
  
  /**
   * Length of Size
   */
  override def length:Int = 2
}

/**
 * Companion object to Size
 */
object Size
{
  /**
   * Attempts to evaluate some object as a Size
   */
  def evaluate(a:Any):Option[Size] = a match
  {
    // Handles sequence of some sort...
    case seq:Seq[_] =>
    {
      if(seq.forall{_.isInstanceOf[Double]})
      {
        val newData = seq.map{_.asInstanceOf[Double]}
        if(newData.length != 2)
          None
        else
          Some(Size(newData(0), newData(1)))
      }
      else
        None
    }
    
    // Handles anything else
    case _ => None
  }
}