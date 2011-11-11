package edu.luc.cs.laufer.cs473.miniool

import junit.framework.TestCase
import org.scalatest.junit.AssertionsForJUnit

class TestWithMethods extends TestCase with AssertionsForJUnit {

/*
  class MyInt {
    public Object value;
    public Object set(Object arg0) {
      this.value = arg0;
    }
    public Object times(Object arg0) {
		// ...
    }
    // ...
  }
 */

val MyInt: Clazz = new Clazz(
  Seq("value"),
  Seq(
    "echo" ->
      (Seq(), Variable("0")),
    "always7" ->
      (Seq(), Message(Variable("this"), "echo", Constant(7))),
    "set" ->
      (Seq(), Assignment(Selection(Variable("this"), "value"), Variable("0"))),
    "get" ->
      (Seq(), Selection(Variable("this"), "value")),
    "times" ->
      (Seq("result"), Sequence(
        Assignment(Variable("result"), Constant(0)),
        While(Variable("0"), Sequence(
          Assignment(Variable("0"), Minus(Variable("0"), Constant(1))),
          Assignment(Variable("result"), Plus(Variable("result"), Selection(Variable("this"), "value")))
        )),
        Variable("result")
      )),
    "fact" ->
      (Seq("result", "recurse", "aux"), Sequence(
        Assignment(Variable("recurse"), Selection(Variable("this"), "value")),
        Assignment(Variable("result"), Constant(1)),
        While(Variable("recurse"), Sequence(
          Assignment(Variable("recurse"), Constant(0)),
          Assignment(Variable("aux"), New(MyInt)),
          Message(Variable("aux"), "set", Minus(Selection(Variable("this"), "value"), Constant(1))),
          Assignment(Variable("result"), Message(Variable("this"), "times", Message(Variable("aux"), "fact")))
        )),
        Variable("result")
      ))
  ))

  /*
   * var a, b, d, e, f, g;
   */
  val store = Map[String, Cell](
    "a" -> Cell(0),
    "b" -> Cell(0),
    "d" -> Cell(0),
    "e" -> Cell(0),
    "f" -> Cell(0),
    "g" -> Cell(0),
    "h" -> Cell(0),
    "i" -> Cell(0),
    "j" -> Cell(0)
  )

  /*
   * var a, b, d, e, f, g;
   * a = new MyInt();
   * a.set(5);
   * b = a.times(7);
   * d = a.fact();
   * e = new MyInt();
   * e.set(6);
   * f = e.times(5);
   * g = e.fact();
   */
  val c = Sequence(
    Assignment(Variable("a"), New(MyInt)),
    Message(Variable("a"), "set", Constant(5)),
    Assignment(Variable("b"), Message(Variable("a"), "times", Constant(7))),
    Assignment(Variable("d"), Message(Variable("a"), "fact")),
    Assignment(Variable("e"), New(MyInt)),
    Message(Variable("e"), "set", Constant(6)),
    Assignment(Variable("f"), Message(Variable("e"), "times", Constant(5))),
    Assignment(Variable("g"), Message(Variable("e"), "fact")),
    Assignment(Variable("h"), Message(Variable("e"), "echo", Constant(12))),
    Assignment(Variable("i"), Message(Variable("e"), "always7")),
    Assignment(Variable("j"), Message(Variable("e"), "get"))
  )

  def testMain() {
    Execute(store)(c)
    assert(store("b").get.left.get === 35)
    assert(store("d").get.left.get === 120)
    assert(store("f").get.left.get === 30)
    assert(store("g").get.left.get === 720)
    assert(store("h").get.left.get === 12)
    assert(store("i").get.left.get === 7)
    assert(store("j").get.left.get === 6)
  }
}
