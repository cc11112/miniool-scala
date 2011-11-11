package edu.luc.cs.laufer.cs473.miniool

import junit.framework.TestCase
import org.scalatest.junit.AssertionsForJUnit

class TestWithInheritedFields extends TestCase with AssertionsForJUnit {

  /*
    class C {
        var value
        def set(v) { this.value = v; }
        def get() { this.value; }
    }
    class D extends C {
    	var value // shadows but does not override the one from C
        def set2(v) { this.value = v; }
        def get2() { this.value; }
        def set3(v) { super.value = v; }
        def get3() { super.value; }
    }
  */

  val C = new Clazz(
    Seq("value"),
    Seq(
      "set" -> (Seq("v"), Assignment(Selection(Variable("this"), "value"), Variable("0"))),
      "get" -> (Seq(), Selection(Variable("this"), "value"))
    )
  )

  val D = new Clazz(
    C,
    Seq("value"),
    Seq(
      "set2" -> (Seq("v"), Assignment(Selection(Variable("this"), "value"), Variable("0"))),
      "get2" -> (Seq(), Selection(Variable("this"), "value")),
      "set3" -> (Seq("v"), Assignment(Selection(Variable("super"), "value"), Variable("0"))),
      "get3" -> (Seq(), Selection(Variable("super"), "value"))
    )
  )

  /*
   * var a, b, d, e, f, g;
   */
  val store = Map[String, Cell](
    "c" -> Cell(0),
    "d" -> Cell(0),
    "u" -> Cell(0),
    "v" -> Cell(0),
    "w" -> Cell(0),
    "x" -> Cell(0),
    "y" -> Cell(0),
    "z" -> Cell(0)
  )

  val c = Sequence(
    Assignment(Variable("c"), New(C)),
    Assignment(Variable("d"), New(D)),
    Message(Variable("c"), "set", Constant(7)),
    Assignment(Variable("u"), Message(Variable("c"), "get")),
    Message(Variable("d"), "set", Constant(8)),
    Message(Variable("d"), "set2", Constant(9)),
    Assignment(Variable("v"), Message(Variable("d"), "get")),
    Assignment(Variable("w"), Message(Variable("d"), "get2")),
    Message(Variable("d"), "set", Constant(11)),
    Message(Variable("d"), "set2", Constant(12)),
    Message(Variable("d"), "set3", Constant(13)),
    Assignment(Variable("x"), Message(Variable("d"), "get")),
    Assignment(Variable("y"), Message(Variable("d"), "get2")),
    Assignment(Variable("z"), Message(Variable("d"), "get3"))
  )

  def testMain() {
    Execute(store)(c)
    assert(store("u").get.left.get === 7)
    assert(store("v").get.left.get === 8)
    assert(store("w").get.left.get === 9)
    assert(store("x").get.left.get === 13)
    assert(store("y").get.left.get === 12)
    assert(store("z").get.left.get === 13)
  }
}
