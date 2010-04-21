package edu.luc.cs.laufer.cs473.miniool

import junit.framework.TestCase
import org.scalatest.junit.AssertionsForJUnit

class TestImperative extends TestCase with AssertionsForJUnit {

  val store = Map[String, Cell](
    "x" -> Cell(2),
    "y" -> Cell(3),
    "r" -> Cell(0)
  )

  val s =
    While(Variable("y"),
      Sequence(
        Assignment(Variable("r"), Plus(Variable("r"), Variable("x"))),
        Assignment(Variable("y"), Minus(Variable("y"), Constant(1)))
      )
    )

  def testMain() {
    assert(store - "x" - "y" - "r" isEmpty)
    assert(store("x").get.left.get === 2)
    assert(store("y").get.left.get === 3)
    assert(store("r").get.left.get === 0)
    Execute(store)(s)
    // Map(x -> Cell(Left(2)), y -> Cell(Left(0)), r -> Cell(Left(6)))
    assert(store - "x" - "y" - "r" isEmpty)
    assert(store("x").get.left.get === 2)
    assert(store("y").get.left.get === 0)
    assert(store("r").get.left.get === 6)
  }
}
