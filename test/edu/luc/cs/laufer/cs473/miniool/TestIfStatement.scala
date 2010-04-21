package edu.luc.cs.laufer.cs473.miniool

import junit.framework.TestCase
import org.scalatest.junit.AssertionsForJUnit

class TestIfStatement extends TestCase with AssertionsForJUnit {

  val store = Map[String, Cell](
    "x" -> Cell(2),
    "y" -> Cell(3),
    "r" -> Cell(0)
  )

  val s =
	Sequence(
	  Assignment(Variable("x"), If(Plus(Variable("y"), Constant(3)), Constant(7), Constant(8))), 
	  Assignment(Variable("y"), If(Variable("r"), Constant(5), Constant(6))) 
    )

  def testMain() {
    Execute(store)(s)
    assert(store - "x" - "y" - "r" isEmpty)
    assert(store("x").get.left.get === 7)
    assert(store("y").get.left.get === 6)
    assert(store("r").get.left.get === 0)
  }
}
