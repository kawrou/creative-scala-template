import cats.effect.unsafe.implicits.global
import doodle.core.*
import doodle.image.*
import doodle.syntax.all.*
import doodle.image.syntax.all.*
import doodle.java2d.*
import cats.effect.kernel.Par
import doodle.syntax.angle

object CreativePolygons {

  def strokeStyle(image: Image, percentage: Double): Image = {
    image
      .strokeWidth(3)
      .strokeColor(Color.crimson.darkenBy((percentage * percentage).normalized))
  }

  def fillStyle(image: Image, count: Int): Image = {
    image.noStroke.fillColor(if (count % 2 == 0) Color.white else Color.black)
  }

  def regularPolygon(radius: Double, sides: Int): Image = {
    val turn = (1.0 / sides).turns

    def iter(count: Int): ClosedPath =
      count match {
        case 0 => ClosedPath.empty.moveTo(radius, 0.degrees)
        case n => iter(n - 1).lineTo(radius, turn * n)
      }

    Image.path(iter(sides))
  }

  // Get Smaller
  def concentricPolygons(startingRadius: Double, sides: Int): Image = {

    def iter(count: Int): Image =
      count match
        case 3 => strokeStyle(regularPolygon(startingRadius * 2, 3), 0.0)
        case n =>
          strokeStyle(regularPolygon(startingRadius * n, n), n / sides.toDouble)
            .on(iter(n - 1))

    iter(sides)
  }

  // changing size
  // rotating shape
  // alternating colours
  def spiralPolygons(startingRadius: Double, sides: Int, turns: Int): Image = {

    def iter(count: Int): Image =
      count match {
      case 0 => fillStyle(regularPolygon(startingRadius, sides), count)
      case n =>
        iter(count - 1).on(
          fillStyle(
            regularPolygon(startingRadius + (startingRadius*n), sides).rotate(turns.degrees * n),
            count
          )
        )
      }
    iter(turns)
  }

  @main def createPolygons() = {
    concentricPolygons(30, 12).draw()
    spiralPolygons(20, 5, 15).draw()
  }

}
