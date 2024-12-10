import cats.effect.unsafe.implicits.global
import doodle.core.*
import doodle.image.*
import doodle.syntax.all.*
import doodle.image.syntax.all.*
import doodle.java2d.*
import cats.effect.kernel.Par
import doodle.syntax.angle

object FunFractals {

  def boxes(count: Int): Image = {
    val aBox = Image.square(20).fillColor(Color.royalBlue)

    def iter(count: Int): Image = {
      count match {
        case 0 => Image.empty
        case n => aBox.beside(boxes(n - 1))
      }
    }

    iter(count)
  }

  def chessboard(count: Int): Image = {
    val redSquare = Image.square(60).fillColor(Color.red)
    val blackSquare = Image.square(60).fillColor(Color.black)
    val twoBytwo =
      redSquare.beside(blackSquare).above(blackSquare.beside(redSquare))

    def iter(count: Int): Image = {
      count match {
        case 0 => Image.empty
        case 1 => twoBytwo
        case n =>
          val unit = chessboard(n - 1)
          (unit.beside(unit)).above(unit.beside(unit))
      }
    }

    iter(count)
  }

  val pinkTriangle = Image.triangle(30, 30).fillColor(Color.pink)
  val triTriangle = pinkTriangle.above(pinkTriangle.beside(pinkTriangle))

  def sierpinski(count: Int): Image = {
    count match {
      case 0 => Image.empty
      case 1 => triTriangle
      case n =>
        val unit = sierpinski(n - 1)
        (unit.above(unit.beside(unit)))
    }
  }

  // Auxillary parameter
  def growingBoxes(count: Int, size: Int): Image = {
    count match {
      case 0 => Image.empty
      case n => Image.square(size).beside(growingBoxes(n - 1, size + 10))
    }
  }

  def changingColorBoxes(count: Int, degrees: Angle): Image = {
    def box(angle: Angle) =
      Image.square(20).fillColor(Color.royalBlue.spin(angle))

    def iter(count: Int): Image = {
      count match {
        case 0 => Image.empty
        case n =>
          box(degrees).beside(changingColorBoxes(n - 1, degrees + 15.degrees))
      }
    }

    iter(count)
  }

  def changingColorBoxesExample(n: Int): Image = {
    val aBox = Image.square(20)

    def iter(num: Int): Image = {
      num match {
        case 0 => Image.empty
        case n =>
          aBox
            .fillColor(Color.royalBlue.spin((15 * n).degrees))
            .beside(changingColorBoxesExample(n - 1))
      }
    }

    iter(n)
  }

  def concentricCircles(n: Int, size: Int): Image = {
    n match {
      case 0 => Image.empty
      case n => Image.circle(size).on(concentricCircles(n - 1, size + 25))
    }
  }

  def concentricCircles2(n: Int, size: Int, angle: Angle): Image = {
    n match {
      case 0 => Image.empty
      case n =>
        Image
          .circle(size)
          .strokeWidth(10)
          .strokeColor(Color.royalBlue.spin(angle))
          .on(concentricCircles2(n - 1, size + 25, angle + 15.degrees))
    }
  }
  @main def fractals() = {
    boxes(4).draw()
    chessboard(3).draw()
    sierpinski(4).draw()
    growingBoxes(5, 10).draw()
    changingColorBoxes(5, 0.degrees).draw()
    changingColorBoxesExample(5).draw()
    concentricCircles(5, 50).draw()
    concentricCircles2(5, 50, 15.degrees).draw()
  }
}
