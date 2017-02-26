package webdoc.render
import javafx.scene.canvas.{Canvas}
import webdoc.{Element, WebdocException}



/**
 * Element that is capable of rendering itself as well as its
 * potential children.  How elements get rendered depends
 * of the type of Elembent it is.
 */
trait RenderingElement extends Element
{
  /**
   * Renders this Element onto a Canvas.
   */
  def render(c:Canvas):Unit
}


/**
 * Companion object to RenderingElement class
 */
object RenderingElemnent
{
  /**
   * Converts some object to a potential RenderingElement.
   * @return either a as itself, or as data transformed.
   */
  def toRenderingAny(a:Any):Any = a match
  {
    case elem:Element => toRenderingElement(elem)
    case seq:Seq[_] => seq map {toRenderingAny}
    case a:Any => a
  }
  
  /**
   * Converts an Element into a Rendering element.
   * This includes its children if it has any.
   */
  def toRenderingElement(e:Element):RenderingElement =
  {
    // Transforms meta first
    val newMeta:Map[String, Any] = e.meta.map
    {
      case (key:String, seq:Seq[_]) => (key, seq.map{toRenderingAny})
      case (key:String, elem:Element) => (key, toRenderingElement(elem))
      case elem:(String, Any) => elem
    }
    
    // Transforms Element by name
    e.nameTag match
    {
      case "hbox" => HBox(newMeta)
      case _ => throw WebdocException("Could not evalute element with name '" + e.nameTag + "'")
    }
  }
}