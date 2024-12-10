import cats.effect.unsafe.implicits.global
import doodle.core.*
import doodle.image.*
import doodle.syntax.all.*
import doodle.image.syntax.all.*
import doodle.java2d.*
import cats.effect.kernel.Par
import doodle.syntax.angle

object Recursion {
  val aBox = Image.square(200).fillColor(Color.royalBlue)

  def boxes(count: Int): Image = {
    count match {
      case 0 => Image.empty
      case n => aBox.beside(boxes(n - 1))
    }
  }

  val circles = Image
    .circle(150)
    .strokeWidth(10)
    .strokeColor(Color.black) on
    Image.circle(100).strokeWidth(10).strokeColor(Color.pink).strokeWidth(10)

  val stars = Image.star(5, 100, 50).strokeWidth(10).strokeColor(Color.green) on
    Image.star(5, 50, 25).strokeColor(Color.blue).strokeWidth(10)

  def alternatingRow1(count: Int): Image = {
    count % 2 match {
      case -1 => Image.empty
      case 0  => circles.beside(alternatingRow1(count - 1))
      case 1  => stars.beside(alternatingRow1(count - 1))
    }
  }

  val star = Image
    .star(5, 100, 50)
    .strokeColor(Color.teal)
    .strokeWidth(10)
    .on(
      Image
        .star(5, 50, 25)
        .strokeColor(Color.royalBlue)
        .strokeWidth(10)
    )

  val circle = Image
    .circle(150)
    .strokeWidth(10)
    .strokeColor(Color.black)
    .on(
      Image
        .circle(100)
        .strokeWidth(10)
        .strokeColor(Color.pink)
        .strokeWidth(10)
    )

  def alternatingRow2(count: Int): Image = {
    count match {
      case 0 => Image.empty
      case n =>
        if (n % 2 == 0) circle.beside(alternatingRow2(n - 1))
        else star.beside(alternatingRow2(n - 1))
    }
  }

  def sevenPointedStar(n: Int) = Image
    .star(7, n * 50, n * 25)
    .strokeWidth(10)
    .strokeColor(Color.orange.spin((5 * n).degrees))

  def funRow(count: Int): Image = {
    count match {
      case 0 => Image.empty
      case n => sevenPointedStar(n).beside(funRow(n - 1))
    }
  }

  val hexagon =
    Image.regularPolygon(6, 20).strokeWidth(10).strokeColor(Color.red)

  val blueCircles = Image
    .circle(20)
    .fillColor(Color.deepSkyBlue)
    .on(Image.circle(40).fillColor(Color.blue))

  def cross(count: Int): Image = {
    count match {
      case 0 => hexagon
      case n =>
        blueCircles
          .beside(blueCircles.above(cross(n - 1)).above(blueCircles))
          .beside(blueCircles)
    }
  }

  @main def recusion() = {
    alternatingRow1(4).draw()
    alternatingRow2(4).draw()
    funRow(5).draw()
    cross(3).draw()
  }
}
