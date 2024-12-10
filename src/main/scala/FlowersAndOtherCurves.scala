import cats.effect.unsafe.implicits.global
import doodle.core._
import doodle.image._
import doodle.syntax.all._
import doodle.image.syntax.all._
import doodle.java2d._

object FlowersAndOtherCurves {
  def rose(k: Int): Angle => Point =
    (angle: Angle) => Point((angle * k).cos * 400, angle)

  def lissajour(a: Int, b: Int, offset: Angle): Angle => Point =
    (angle: Angle) =>
      Point(400 * ((angle * a) + offset).sin, 400 * (angle * b).sin)

  val rose5 = rose(5)
  val rose7 = rose(7)
  val rose8 = rose(8)

  val lissajourInfinity = lissajour(1,2, 90.degrees)
  val lissajourCircle = lissajour(1,1,90.degrees)

  val pinkDot = Image.circle(20).fillColor(Color.violet)

  def drawShape(points: Int, marker: Image, curve: Angle => Point): Image = {
    val turn = Angle.one/points

    def loop(count: Int): Image = {
      count match {
        case 0 => 
          val pt = curve(Angle.zero)
          marker.at(pt)
        case n => 
          val pt = curve(turn * count)
          marker.at(pt).on(loop(n-1))
      }
    }

    loop(points)
  }

  @main def drawFlowers() = {
    drawShape(800, pinkDot, rose5).draw()
    drawShape(800, pinkDot, rose7).draw()
    drawShape(800, pinkDot, rose8).draw()
    drawShape(500, pinkDot, lissajourInfinity).draw()
    drawShape(300, pinkDot, lissajourCircle).draw()
  }
}
