import cats.effect.unsafe.implicits.global
import doodle.core.*
import doodle.image.*
import doodle.syntax.all.*
import doodle.image.syntax.all.*
import doodle.java2d.*

object GradientSquare {
  //Return a circle of a colour spun by the angle
  def circle(color: Color, angle: Angle, diameter: Double): Image = 
    Image.circle(diameter).strokeWidth(10).strokeColor(color.spin(angle))

  //Returns multiple circles. But the circles need to be of different sizes
  def gradientCircle(color: Color, angle: Angle): Image = {
    circle(color, angle, 200) on
    circle(color, angle + 15.degrees, 250) on
    circle(color, angle + 30.degrees, 300) on
    circle(color, angle + 45.degrees, 350) on
    circle(color, angle + 60.degrees, 400) on
    circle(color, angle + 75.degrees, 450)
  } 

  val colouredCircle = circle(Color.blueViolet, 0.degrees, 100)
  val image = gradientCircle(Color.blueViolet, 0.degrees)

  @main def gardientCircle() = {
    colouredCircle.draw()
    image.draw()
  }
}


