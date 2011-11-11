package edu.luc.cs.laufer.cs473.miniool

import junit.framework.TestCase
import org.scalatest.junit.AssertionsForJUnit

class TestSimple extends TestCase with AssertionsForJUnit {

  val studentCourseRecord = new Clazz("firstExamScore", "secondExamScore", "totalScore")
  val studentSemRecord = new Clazz("course1", "course2")

  val store = Map[String, Cell](
    "q" -> Cell(0),
    "r" -> Cell(0)
  )

  val s =
	  Sequence(
	    Assignment(Variable("r"), New(studentSemRecord)),
	    Assignment(Selection(Variable("r"), "course1"), New(studentCourseRecord)),
	    Assignment(Selection(Selection(Variable("r"), "course1"), "firstExamScore"), Constant(25)),
	    Assignment(Selection(Selection(Variable("r"), "course1"), "secondExamScore"), Constant(35)),
	    Assignment(Selection(Selection(Variable("r"), "course1"), "totalScore"),
	                       Plus(Selection(Selection(Variable("r"), "course1"), "firstExamScore"),
	                                Selection(Selection(Variable("r"), "course1"), "secondExamScore"))),
	    Assignment(Selection(Variable("r"), "course2"), Selection(Variable("r"), "course1")),
	    Assignment(Variable("q"), Selection(Selection(Variable("r"), "course2"), "totalScore")),
	    Assignment(Selection(Selection(Variable("r"), "course1"), "firstExamScore"), Constant(45))
	  )

//  def testMain() {
//    assert(store - "q" - "r" isEmpty)
//    assert(store("q").get.left.get === 0)
//    assert(store("r").get.left.get === 0)
//    Execute(store)(s)
//    println(store)
//    assert(store - "q" - "r" isEmpty)
//    assert(store("q").get.left.get === 60)
//    assert(store("r").get.right.get.getField("course1").get.right.get.getField("firstExamScore").get.left.get === 45)
//  }

  def testMain2() {
	val store2 = Map[String, Cell](
	  "q" -> Cell(60),
	  "r" -> Cell(Right(Instance(None,Map(
	 		  "course1" -> Cell(Right(Instance(None,Map(
	 		 		  "firstExamScore" -> Cell(Left(45)),
	 		 		  "secondExamScore" -> Cell(Left(35)),
	 		 		  "totalScore" -> Cell(Left(60))
	 		  ),Map()))),
	 		  "course2" -> Cell(Right(Instance(None,Map(
	 		 		  "firstExamScore" -> Cell(Left(45)),
	 		 		  "secondExamScore" -> Cell(Left(35)),
	 		 		  "totalScore" -> Cell(Left(60))
	 		  ),Map())))),Map()))
	  )
	)
	Execute(store)(s)
	assert(store === store2)
  }
}
