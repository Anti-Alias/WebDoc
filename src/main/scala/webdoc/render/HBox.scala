package webdoc.render
import javafx.scene.canvas.{Canvas}
import webdoc.{Element}



/**
 * Horizontal Box Element who's Children are rendered from left to right.
 * Its width is the sum of its children's width.
 * Its height is the height of it's tallest child.
 * All children must have a size array outlining their width and height.
 */
case class HBox(meta:Map[String, Any]) extends RenderingElement
{
  // Checks that children have a size array which should contain two doubles
  {
    val allAreElemnts:Boolean = children.forall{_.isInstanceOf[Element]}
    
  }
  override def render(c:Canvas)
  {
    
  }
}