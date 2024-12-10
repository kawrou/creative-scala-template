import cats.effect.unsafe.implicits.global
import doodle.core.*
import doodle.image.*
import doodle.syntax.all.*
import doodle.image.syntax.all.*
import doodle.java2d.*
import cats.effect.kernel.Par
import doodle.syntax.angle

object Paths {
  val hexagonPath = ClosedPath.empty
    .moveTo(100, 0.degrees)
    .lineTo(100, 60.degrees)
    .lineTo(100, 120.degrees)
    .lineTo(100, 180.degrees)
    .lineTo(100, 240.degrees)
    .lineTo(100, 300.degrees)
    .lineTo(100, 360.degrees)

  val hexagonImage = Image.path(hexagonPath)

  def regularPolygon(sides: Int, radius: Double): Image = {
    val turn = (1.0 / sides).turns

    def iter(count: Int): ClosedPath =
      count match {
        case 0 => ClosedPath.empty.moveTo(radius, 0.degrees)
        case n => iter(n - 1).lineTo(radius, turn * n)
      }

    Image.path(iter(sides))
  }

  //ClosedPath.empty.moveTo().lineTo()).lineTo()
  @main def path() = {
    regularPolygon(3, 25)
      .on(regularPolygon(5, 50))
      .on(regularPolygon(7, 75))
      .draw()
  }
}
