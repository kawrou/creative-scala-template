import cats.effect.unsafe.implicits.global
import doodle.core.*
import doodle.image.*
import doodle.syntax.all.*
import doodle.image.syntax.all.*
import doodle.java2d.*
import cats.effect.kernel.Par
import doodle.syntax.angle

object PolarCoord {
  def polygonPoints(sides: Int, radius: Int): Image = {
    val dot = Image.circle(5)
    val s = sides

    def iter(count: Int, radius: Int): Image = {
      count match {
        case 0 => Image.empty
        case n =>
          dot
            .at(Point(radius, (360 / s * n).degrees))
            .on(iter(n - 1, radius))
      }
    }
    iter(sides, radius)
  }

  def polygonPoints2(sides: Int, radius: Double): Image = {
    val turn = (1.0 / sides).turns

    def loop(count: Int): Image =
      count match {
        case 0 => Image.empty
        case n =>
          Image
            .circle(5)
            .at(Point(radius, turn * n))
            .on(loop(n - 1))
      }

    loop(sides)
  }

  @main def coord() = {
    polygonPoints(6, 100).draw()
    polygonPoints(3, 50)
      .fillColor(Color.crimson)
      .beside(polygonPoints(5, 50).fillColor(Color.lawngreen))
      .beside(polygonPoints(7, 50).fillColor(Color.dodgerBlue))
      .draw()

    polygonPoints2(6, 100).draw()
    polygonPoints2(3, 50)
      .fillColor(Color.crimson)
      .beside(polygonPoints(5, 50).fillColor(Color.lawngreen))
      .beside(polygonPoints(7, 50).fillColor(Color.dodgerBlue))
      .draw()

  }
}
