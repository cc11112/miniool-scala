package edu.luc.cs.laufer.cs473.miniool

import junit.framework.TestCase
import org.scalatest.junit.AssertionsForJUnit

class TestMyInt extends TestCase with AssertionsForJUnit {

  /*
 * class myInt {
 *     var value;
 *     init(that) { value = that; }
 *     itimes(that) { // returns an Integer
 *         if (that) {
 *             return value + this.itimes(that - 1);
 *         } else {
 *             return 0;
 *         }
 *     }
 *     plus(that) { // returns a myInt
 *         local result;
 *         result = new myInt;
 *         result.init(value + that);
 *         return result;
 *     }
 *     // TODO your job: implement the missing methods (minus, uminus, times, intValue)
 * }
 */

  val MyInt: Clazz = new Clazz(
    Seq("value"),
    Seq(
      "init" -> (Seq(),
        Assignment(Selection(Variable("this"), "value"), Variable("0"))),
      "itimes" -> (Seq(),
        If(Variable("0"),
          Plus(
            Selection(Variable("this"), "value"),
            Message(Variable("this"), "itimes", Minus(Variable("0"), Constant(1)))),
          Constant(0))),
      "plus" -> (Seq("result"),
        Sequence(
          Assignment(Variable("result"), New(MyInt)),
          Message(Variable("result"), "init", Plus(Selection(Variable("this"), "value"), Variable("0"))),
          Variable("result"))),
      // TODO your job: implement the remaining methods
      // hint: use "itimes" to implement "times"
      "minus" -> (Seq("result"),
        Sequence(
          Assignment(Variable("result"), New(MyInt)),
          Message(Variable("result"), "init", Minus(Selection(Variable("this"), "value"), Variable("0"))),
          Variable("result"))),
      "uminus" -> (Seq("result"),
        Sequence(
          Assignment(Variable("result"), New(MyInt)),
          Message(Variable("result"), "init", Minus(Constant(0),Selection(Variable("this"), "value"))),
          Variable("result"))),
      "times" -> (Seq("result"),
        Sequence(
          Assignment(Variable("result"), New(MyInt)),
          //Message(Variable("result"), "init", Selection(Variable("this"), "value")),
          Message(Variable("result"), "init", Message(Variable("this"), "itimes", Variable("0"))),
          Variable("result"))),
        "intValue" -> (Seq("result"),
        	Selection(Variable("this"), "value"))
        )
 )

  /*
   * var u, v, x, y, z;
   */
  val store = Map[String, Cell](
    "u" -> Cell(0),
    "v" -> Cell(0),
    "x" -> Cell(0),
    "y" -> Cell(0),
    "z" -> Cell(0),
    "r" -> Cell(0),
    "t" -> Cell(0),
    "k" -> Cell(0)
    )

  /*
   * x = new myInt;
   * x.init(5);
   * y = x.itimes(7);
   * z = new myInt;
   * z.init(6);
   * u = z.itimes(8);
   * v = z.minus(10).times(4).uminus().times(3).minus(7).intValue(); // your job: see below
   */
  val c = Sequence(
    Assignment(Variable("x"), New(MyInt)),
    Message(Variable("x"), "init", Constant(5)),
    Assignment(Variable("y"), Message(Variable("x"), "itimes", Constant(7))),
    Assignment(Variable("z"), New(MyInt)),
    Message(Variable("z"), "init", Constant(6)),
    Assignment(Variable("u"), Message(Variable("z"), "itimes", Constant(8))),
    // TODO your job: replace the assignment to v by the following one:
    // v = z.minus(10).times(4).uminus().times(3).minus(7).intValue();
    //Assignment(Variable("v"), Constant(0)))
    
    Assignment(Variable("v"), 
        Message(
            Message(Message(
                Message(Message(Message(Variable("z"), "minus", Constant(10)), "times", Constant(4)), "uminus"),
                "times", Constant(3)), "minus", Constant(7)),
            "intValue")
        )
    
	)
  /* x = new myInt
	 * x.init(5)
	 * r = x.uminus()
	 * y = new myInt
	 * y.init(-9)
	 * t = y.uminus()
	 */

  val d = Sequence(
    Assignment(Variable("x"), New(MyInt)),
    Message(Variable("x"), "init", Constant(5)),
    Assignment(Variable("r"), Message(Message(Variable("x"), "uminus"),"intValue")),
    Assignment(Variable("y"), New(MyInt)),
    Message(Variable("y"), "init", Constant(-9)),
    Assignment(Variable("t"), Message(Message(Variable("y"), "uminus"),"intValue"))
    )
    
   val test = Sequence(
	Assignment(Variable("x"), New(MyInt)),
	Message(Variable("x"), "init", Constant(5)),
	Assignment(Variable("y"), New(MyInt)),
	Message(Variable("y"), "init", Constant(-9)),
	Assignment(Variable("k"), Message(Message(Variable("y"), "plus", Constant(4)), "intValue")),
	Assignment(Variable("t"), Message(Message(Variable("x"), "times", Constant(3)), "intValue")),
	Assignment(Variable("u"), Message(Message(Variable("x"), "minus", Constant(3)), "intValue"))
   )
    
  def testMain() {
    Execute(store)(c)
    assert(store("y").get.left.get === 35)
    assert(store("u").get.left.get === 48)
    println(store("v"))
    assert(store("v").get.left.get === 41)
    //only for my unit test
    Execute(store)(d)
    assert(store("r").get.left.get === -5)
    assert(store("t").get.left.get === 9)
    Execute(store)(test)
    assert(store("k").get.left.get === -5)
    assert(store("t").get.left.get === 15)
    assert(store("u").get.left.get === 2)
  }
}
