import cats.effect.unsafe.implicits.global
import doodle.core._
import doodle.image._
import doodle.syntax.all._
import doodle.image.syntax.all._
import doodle.java2d._

object FunWithFunctions {
  def fold(count: Int, build: (Int, Image) => Image): Image =
    count match {
      case 0 => Image.empty
      case n => build(n, fold(count - 1, build))
    }

  val gradientBoxes: (Int, Image) => Image =
    (count, image) =>
      Image
        .square(60)
        .fillColor(Color.royalBlue.spin(10.degrees * count))
        .noStroke
        .beside(image)

  val changingCircles = (count: Int, image: Image) =>
    Image
      .circle(60.0 * count)
      .fillColor(Color.crimson.spin(10.degrees * count))
      .noStroke
      .beside(image)

  def fold2(count: Int, base: Image, build: (Int, Image) => Image): Image =
    count match {
      case 0 => base
      case n => build(n, fold2(count - 1, base, build))
    }

  val sierpinski = (count: Int, image: Image) =>
    image.above(image.beside(image))

  val pinkTriangle = Image.equilateralTriangle(30).strokeColor(Color.hotpink).noFill

  @main def createFunctions() = {
    fold(5, gradientBoxes).draw()
    fold(5, changingCircles).draw()
    fold2(5, pinkTriangle, sierpinski).draw()
  }
}
