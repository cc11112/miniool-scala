package edu.luc.cs.laufer.cs473.miniool

import junit.framework.TestCase
import org.scalatest.junit.AssertionsForJUnit

class TestRecursive extends TestCase with AssertionsForJUnit {

  val store = Map[String, Cell](
    "n" -> Cell(0),
    "h" -> Cell(0),
    "s" -> Cell(0)
  )

  val ListNode = new Clazz("value", "next")

  /*
   * n = new ListNode
   * h = n
   * n.value = 2
   * n.next = new ListNode
   * n = n.next
   * ...
   * n.value = 7
   * n.next = null
   * n = h
   * while (n != null) {
   *   s = s + n.value
   *   n = n.next
   * }
   */


  val s =
    Sequence(
      Assignment(Variable("n"), New(ListNode)),
      Assignment(Variable("h"), Variable("n")),
      Assignment(Selection(Variable("n"), "value"), Constant(2)),
      Assignment(Selection(Variable("n"), "next"), New(ListNode)),
      Assignment(Variable("n"), Selection(Variable("n"), "next")),
      Assignment(Selection(Variable("n"), "value"), Constant(3)),
      Assignment(Selection(Variable("n"), "next"), New(ListNode)),
      Assignment(Variable("n"), Selection(Variable("n"), "next")),
      Assignment(Selection(Variable("n"), "value"), Constant(5)),
      Assignment(Selection(Variable("n"), "next"), New(ListNode)),
      Assignment(Variable("n"), Selection(Variable("n"), "next")),
      Assignment(Selection(Variable("n"), "value"), Constant(7)),
      Assignment(Selection(Variable("n"), "next"), Constant(0)),
      Assignment(Variable("n"), Variable("h")),
      While(Variable("n"),
        Sequence(
          Assignment(Variable("s"), Plus(Variable("s"), Selection(Variable("n"), "value"))),
          Assignment(Variable("n"), Selection(Variable("n"), "next"))
        )
      )
    )

  def testMain() {
    Execute(store)(s)
    // Map(n -> Cell(Left(0)),
    //     h -> Cell(Right(Instance(None,Map(value -> Cell(Left(2)), next -> Cell(Right(Instance(None,Map(value -> Cell(Left(3)), next -> Cell(Right(Instance(None,Map(value -> Cell(Left(5)), next -> Cell(Right(Instance(None,Map(value -> Cell(Left(7)), next -> Cell(Left(0))),Map())))),Map())))),Map())))),Map()))),
    //     s -> Cell(Left(17)))
    assert(store - "n" - "h" - "s" isEmpty)
    assert(store("n").get.left.get === 0)
    assert(store("s").get.left.get === 17)
    assert(store("h").
    		get.right.get.fields("value").get.left.get === 2)
    assert(store("h").
    		get.right.get.fields("next").
    		get.right.get.fields("value").get.left.get === 3)
    assert(store("h").
    		get.right.get.fields("next").
    		get.right.get.fields("next").
    		get.right.get.fields("value").get.left.get === 5)
    assert(store("h").
    		get.right.get.fields("next").
    		get.right.get.fields("next").
    		get.right.get.fields("next").
    		get.right.get.fields("value").get.left.get === 7)
  }
}
