package edu.luc.cs.laufer.cs473.miniool

import junit.framework.TestCase
import org.scalatest.junit.AssertionsForJUnit

class TestFibonacci extends TestCase with AssertionsForJUnit {

/*
 * class myFib {
 *     value;
 *     init(that) {
 *         value = that;
 *     }
 *     int helpFib(int k, int fibk1, int fibk2) {
 *       if (value == 0)
 *         return 0;
 *       if (value == k)
 *         return fibk1;
 *       else
 *         return helpFib(k+1, fibk1+fibk2, fibk1);
 *     }
 *     int fib() {
 *       return helpFib(1, 1, 0);
 *    }
 * }
 */

val MyFib = new Clazz(
  Seq("value"),
  Seq(
    "init" -> (Seq(),
      Assignment(Selection(Variable("this"), "value"), Variable("0"))),
    "helpFib" -> (Seq(),
    // TODO your job
    Constant(1)),
    "fib" -> (Seq(), Message(Variable("this"), "helpFib", Constant(1), Constant(1), Constant(0)))
  ))

  /*
   * var u, v, w, x, y, z;
   */
  val store = Map[String, Cell](
    "u" -> Cell(0),
    "v" -> Cell(0),
    "w" -> Cell(0),
    "x" -> Cell(0),
    "y" -> Cell(0),
    "z" -> Cell(0)
  )

  /*
   * x = new myFib;
   * x.init(5);
   * v = x.fib();
   * z = new myFib;
   * z.init(8);
   * w = z.fib();
   * y = new myFib;
   * y.init(12);
   * u = u.fib();
   */
  val c = Sequence(
    Assignment(Variable("x"), New(MyFib)),
    Message(Variable("x"), "init", Constant(5)),
    Assignment(Variable("v"), Message(Variable("x"), "fib")),
    Assignment(Variable("z"), New(MyFib)),
    Message(Variable("z"), "init", Constant(8)),
    Assignment(Variable("w"), Message(Variable("z"), "fib")),
    Assignment(Variable("y"), New(MyFib)),
    Message(Variable("y"), "init", Constant(18)),
    Assignment(Variable("u"), Message(Variable("y"), "fib"))
  )

  def testMain() {
    Execute(store)(c)
    assert(store("v").get.left.get === 5)
    assert(store("w").get.left.get === 21)
    assert(store("u").get.left.get === 2584)
  }
}
