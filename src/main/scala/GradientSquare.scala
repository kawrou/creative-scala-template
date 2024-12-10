import cats.effect.unsafe.implicits.global
import doodle.core.*
import doodle.image.*
import doodle.syntax.all.*
import doodle.image.syntax.all.*
import doodle.java2d.*

object GradientBox {
  // def boxes(color: Color): Image = {
  //   val box =
  //     Image
  //       .rectangle(150, 150)
  //       .strokeWidth(5.0)
  //       .strokeColor(color.spin(30.degrees))
  //       .fillColor(color)

  //   box.below(box).below(box).below(box).below(box)
  // }

  // val image = boxes(Color.paleGoldenrod)

  // Return a box with the color spun by the angle
  def box(color: Color, angle: Angle): Image = {
    Image
      .rectangle(150, 150)
      .strokeWidth(15)
      .strokeColor(color.spin(angle + 30.degrees))
      .fillColor(color.spin(angle))
  }

  // Return an Image of five boxes will with a gradient starting from the given color and changing by 15.degrees at each box.
  // gradientBoxes should make use of box
  def gradientBoxes(color: Color): Image = {
    box(color, 0.degrees) below
      box(color, 15.degrees) below
      box(color, 30.degrees) below
      box(color, 45.degrees) below
      box(color, 60.degrees)

  }

  val colouredBox = box(Color.blue, Angle.degrees(15))
  val gradientBox = gradientBoxes(Color.royalBlue)
  @main def gardientBoxes() = {
    colouredBox.draw()
    gradientBox.draw()
  }
}
