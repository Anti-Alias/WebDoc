import javafx.application.{Application}
import javafx.stage.{Stage}
import javafx.scene.{Scene}
import javafx.scene.paint.{Color}
import javafx.scene.canvas.{Canvas, GraphicsContext}
import javafx.scene.control.{Button}
import javafx.scene.layout.{StackPane}
import javafx.beans.{Observable, InvalidationListener}
import javafx.beans.value.{ChangeListener, ObservableValue}
import java.io.{InputStream}
import webdoc.{Parser}


/**
* Main part of application
*/
object Main
{
  def main(args:Array[String])
  {
    // Tests parser
    val parser = Parser()
    val in:InputStream = getClass.getResourceAsStream("doc1.wdc")
    parser.parse(in)
    
    // Tests UI
    Application.launch(classOf[App], args:_*)
  }
}

/**
* JavaFX application
*/
class App extends Application
{
  /**
  * Repaint function for Canvas
  */
  def repaint(canvas:Canvas)
  {
    val gc:GraphicsContext = canvas.getGraphicsContext2D
    gc.setFill(Color.BLACK)
    gc.fillRect(0, 0, canvas.getWidth, canvas.getHeight)
  }

  override def start(primStage: Stage)
  {
    primStage.setTitle("The title")
    val root = new StackPane()
    val canvas = new Canvas(400, 400)
    val scene = new Scene(root, 800, 400)
    canvas.widthProperty.bind(scene.widthProperty)
    canvas.heightProperty.bind(scene.heightProperty)
    root.getChildren.add(canvas)

    val callback = new ChangeListener[Number]()
    {
      override def changed(obv:ObservableValue[_ <: Number], old:Number, now:Number)
      {
        repaint(canvas)
      }
    }
    scene.widthProperty.addListener(callback)
    scene.heightProperty.addListener(callback)
    repaint(canvas)

    primStage.setScene(scene)
    primStage.show
  }
}
