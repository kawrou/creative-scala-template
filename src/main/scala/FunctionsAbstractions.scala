import cats.effect.unsafe.implicits.global
import doodle.core._
import doodle.image._
import doodle.syntax.all._
import doodle.image.syntax.all._
import doodle.java2d._

object FunctionsAbstractions {
  // def stackedBoxes(count: Int): Image =
  //     count match {
  //         case 0 => Image.empty
  //         case n => Image.square(20).beside(stackedBoxes(n-1))
  //     }

  // def loop(count: Int): Image =
  //     count match {
  //         case 0 => Image.empty
  //         case n =>
  //             Image
  //                 .circle(5)
  //                 .at(Point(radius, turn * n))
  //                 .on(loop(n-1))
  //     }

  // def aMethod(count: Int, build: ???): Image =
  //     count match {
  //         case 0 => Image.empty
  //         case n => build(n, aMethod(count-1, build))
  //     }


  // Function Literal - is it same as anonymous/lambda functions?
  // (parameter: type, ...) => expression

  // Function types
  // (A,B) => C
  def fold(count: Int, build: (Int, Image) => Image): Image =
    count match {
      case 0 => Image.empty
      case n => build(n, fold(count - 1, build))
    }

  val aBox =
    Image.square(60).fillColor(Color.royalBlue).strokeColor(Color.crimson)
  val stack = (count: Int, image: Image) => aBox.beside(image)

  def squareF(x: Int, f: Int => Int): Int = f(x) * f(x)
  val add42 = (x: Int) => x + 42
  squareF(0, add42)

  //Funcitons as Objects
  // All first class values are objects, including functions.
  // So functions can themselves have methods.
  val addTen = (a: Int) => a + 10
  val double = (a: Int) => a * 2
  val combined = addTen.andThen(double)
  combined(5)
  // Calling a function is actually calling the method "apply".
  // Scala allows us to drop "apply" method and write the call like a function call.
  // combined.apply(5)

  // We can convert methods to functions by using "_"
  def times42(x: Int): Int = x * 42
  val times42Function = times42 _
  // val times42Function = times42(_)
  times42Function(2)


  //PRACTICE

  val squareNnumber = (num: Int) => num ^ 2

  val spinHue = (color: Color) => color.spin(15.degrees)

  val makeRow = (image: Image) => 
    image.beside(image.rotate(90.degrees))
    .beside(image.rotate(180.degrees))
    .beside(image.rotate(240.degrees))
    .beside(image.rotate(360.degrees))  


  @main def runFuncAbstraction() = {
    fold(5, stack).draw()
  }
}
