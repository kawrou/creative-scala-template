import cats.effect.unsafe.implicits.global
import doodle.core._
import doodle.image._
import doodle.syntax.all._
import doodle.image.syntax.all._
import doodle.java2d._

object FunctionComposition {
  val dropShadow = (image: Image) =>
    image.on(image.strokeColor(Color.black).fillColor(Color.black).at(5, -5))

  val mirrored = (image: Image) =>
    image.beside(image.transform(Transform.horizontalReflection))

  val composed = mirrored.andThen(dropShadow)

  val star = Image
    .star(5, 100, 50)
    .fillColor(Color.fireBrick)
    .strokeColor(Color.dodgerBlue)
    .strokeWidth(7.0)

  def scale(factor: Double): Point => Point =
    (point: Point) => Point(point.r * factor, point.angle)

  val parametricCircle: Angle => Point =
    (angle: Angle) => Point(1.0, angle)

  val growingDot: Point => Image =
    (pt: Point) => Image.circle(pt.angle.toTurns * 100).at(pt)

  val circle100 = parametricCircle.andThen(scale(100))
  val circle200 = parametricCircle.andThen(scale(200))
  val circle300 = parametricCircle.andThen(scale(300))

  val growingCircle = parametricCircle
    .andThen(scale(400))
    .andThen(growingDot)

  val parametricSpiral: Angle => Point =
    (angle: Angle) => Point(Math.exp(angle.toTurns - 1), angle)

  val spiral100 = parametricSpiral.andThen(scale(100))
  val spiral200 = parametricSpiral.andThen(scale(200))
  val spiral300 = parametricSpiral.andThen(scale(300))

  val growingSpiral = parametricSpiral
    .andThen(scale(500))
    .andThen(growingDot)

  def drawCurve(points: Int, curve: Angle => Image): Image = {
    val turn = Angle.one / points

    def loop(count: Int): Image = {
      val angle = turn * count
      count match
        case 0 => Image.empty
        case n => curve(angle).on(loop(n - 1))
    }

    loop(points)
  }

  @main def drawCurvesWithComposition() = {
    dropShadow(star)
      .beside(mirrored(star))
      .beside(composed.apply(star))
      .draw()

    drawCurve(20, growingCircle).draw()
    drawCurve(20, growingSpiral).draw()
  }
}
