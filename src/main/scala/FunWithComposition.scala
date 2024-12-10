import cats.effect.unsafe.implicits.global
import doodle.core._
import doodle.image._
import doodle.syntax.all._
import doodle.image.syntax.all._
import doodle.java2d._

object FunWithComposition {
  // METHODS:
  def concentricCircles(count: Int, size: Int): Image =
    count match {
      case 0 => Image.empty
      case n => Image.circle(size).on(concentricCircles(n - 1, size + 10))
    }

  def concentricShapes(count: Int, singleShape: Int => Image): Image =
    count match {
      case 0 => Image.empty
      case n => singleShape(n).on(concentricShapes(n - 1, singleShape))
    }

  def withSizeOn(f: Int => Image, g: Int => Image): Int => Image =
    size => f(size).on(g(size))

  def strokeColor(color: Color): Image => Image =
    image => image.strokeColor(color)

  def spinning(n: Int): Color =
    Color.blue.desaturate(0.5.normalized).spin((n * 30).degrees)

  def fadeColor(n: Int): Color =
    Color.blue.fadeOut((1 - n / 20.0).normalized)

  def colored(shape: Int => Image, color: Int => Color): Int => Image =
    (n: Int) => shape(n).strokeWidth(10).strokeColor(color(n))

  // SHAPES:
  val circle: Int => Image =
    size => Image.circle(size)

  val square: Int => Image =
    size => Image.square(size)

  val triangle: Int => Image =
    size => Image.equilateralTriangle(size)

  val size: Int => Int =
    n => 100 + 24 * n

  // COMPOSITIONS:
  val blackCircle = size.andThen(circle)
  val redCircle = size.andThen(circle).andThen(strokeColor(Color.red))
  val blackSquares = size.andThen(square)

  // CALLING METHODS:
  val blackCircles: Image =
    concentricShapes(10, blackCircle)

  val redCircles: Image =
    concentricShapes(10, redCircle)

  val circlesOnSquares: Image =
    concentricShapes(
      10,
      // size
      //   .andThen(withSizeOn(circle, square))
      //   .andThen(strokeColor(Color.royalBlue))
      withSizeOn(redCircle, blackSquares)
    )

  val allShapes: Image =
    concentricShapes(10, colored(size.andThen(circle), spinning))
      .beside(
        concentricShapes(10, colored(size.andThen(triangle), fadeColor))
          .beside(concentricShapes(10, colored(size.andThen(square), spinning)))
      )

  @main def drawShapeWithFuncComp() = {
    // blackCircles.draw()
    // redCircles.draw()
    // circlesOnSquares.draw()
    allShapes.draw()
  }
}
