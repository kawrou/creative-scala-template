import cats.effect.unsafe.implicits.global
import doodle.core._
import doodle.image._
import doodle.syntax.all._
import doodle.image.syntax.all._
import doodle.java2d._

object ParametricCurves {

  // points - the number of evenly spaced points around the circle
  // marker - the Image we draw at that point
  // curve - the parametric curve

  def drawCurve(points: Int, marker: Image, curve: Angle => Point): Image = {
    val turn = Angle.one / points //360 / 10 = 36 degrees

    def loop(count: Int): Image = {
      count match {
        case 0 => marker.at(curve(Angle.zero))
        case n => marker.at(curve(turn * count)).on(loop(n - 1))
      }
    }

    loop(points)
  }

  val curve = (angle: Angle) => Point(100, angle)
  val marker = Image.circle(10.0).fillColor(Color.hotpink).noStroke

  def drawSpiral(points: Int, dot: (Double, Angle) => Image, spiralPoint: (Double, Angle) => Point): Image = {
    val turn = Angle.one / points

    def loop(count: Int): Image = {
      count match {
        case 0 => 
          val pt = spiralPoint(0, Angle.zero)
          dot(count, Angle.zero).at(pt)
        case n => 
          val pt = spiralPoint(count*20, turn * count)
          dot(count*2, turn*count)
            .at(pt)
            .on(loop(n - 1))
      }
    }

    loop(points)
  }

  val spiral = (radius: Double, angle: Angle) => Point(radius, angle)
  val dot = (diameter: Double, angle: Angle) => 
    Image
      .circle(diameter)
      .fillColor(Color.red.spin(angle))
      .noStroke

  def drawSpiral2(points: Int, dot: Point => Image, curve: Angle => Point): Image = {
    val turn = Angle.one / points

    def loop(count: Int): Image = {
      count match {
        case 0 => 
          val pt = curve(Angle.zero)
          dot(pt).at(pt)
        case n => 
          val pt = curve(turn * count)
          dot(pt).at(pt).on(loop(n-1))
      }
    }

    loop(points)
  }

  val dot2 = (point: Point) => 
    Image
      .circle(point.r * 0.125 + 7)
      .fillColor(Color.red.spin(point.angle / -4.0))
      .noStroke

  val parametricSpiral = (angle: Angle) => Point(angle.toDegrees,angle)

  @main def drawCurves() = {
    drawCurve(10, marker, curve).draw()
    drawSpiral(21, dot, spiral).draw()
    drawSpiral2(21, dot2, parametricSpiral).draw()
  }
}
