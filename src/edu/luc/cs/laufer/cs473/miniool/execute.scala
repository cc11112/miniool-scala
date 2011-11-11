package edu.luc.cs.laufer.cs473.miniool

// TODO your job: implement the cases for Minus, Times, Div, and If below
// TODO in addition, implement field and method lookup in Instance in
//      the presence of inheritance

import SyntacticTypes.Method
import RuntimeTypes._

object RuntimeTypes {
  /**
   * A run-time value is either a number or an Instance (object).
   */
  type Value = Either[Int, Instance]
  /**
   * A memory store is a mapping from variable names to storage Cells.
   */
  type Store = Map[String, Cell]
  /**
   * A scoped method is an object in which the method is defined
   * along with the method itself.
   */
  type ScopedMethod = (Instance, (Seq[String], Statement))
}

/**
 * A cell for storing a value (either a number or an object).
 */
case class Cell(var value: Value) {
  def get = value
  def set(value: Value) = { this.value = value ; this }
}

/**
 * A companion object defining a useful Cell instance.
 */
object Cell {
  def apply(i: Int): Cell = Cell(Left(i)) // Left -> number, Right -> object
  val NULL = Cell(0)
}

/**
 * An object (instance) has an optional superclass instance, a map from field
 * names to variables, and a map from method names to methods.
 */
case class Instance(zuper: Option[Instance], fields: Map[String, Cell], methods: Map[String, Method]) {
  require(zuper != null)
  require(fields != null)
  require(! fields.contains(null))
  require(methods != null)
  require(! methods.contains(null))

  def getField(name: String): Cell =
  // TODO: your job: replace this result with a meaningful field lookup
	Cell(0)

  def getScopedMethod(name: String): ScopedMethod =
  // TODO: your job: replace this result with a meaningful method lookup
	(Instance(None, Map(), Map()), (Seq(), Constant(0)))
}

/**
 * An interpreter for expressions and statements.
 * It evaluates a Statement down to a Cell (l-value) containing the result.
 * If desired, the result (r-value) can be accessed in an additional step.
 */
object Execute {

  def apply(store: Store)(s: Statement): Cell = s match {
    case Constant(value) => Cell(Left(value))
    case Plus(left, right) => binaryOperation(store, left, right, _+_)
    case Variable(name) => store(name)
    case Assignment(left, right) => {
      val lvalue = apply(store)(left)
      val rvalue = apply(store)(right)
      lvalue.set(rvalue.get)
    }
    case Sequence(statements @ _*) =>
      statements.foldLeft(Cell.NULL)((c, s) => apply(store)(s))
    case While(guard, body) => {
      var gvalue = apply(store)(guard)
      while (gvalue.get.isRight || gvalue.get.left.get != 0) {
        apply(store)(body)
        gvalue = apply(store)(guard)
      }
      Cell.NULL
    }
    case New(Clazz(zuper, fields, methods)) => {
      // create an object based on the list of field names and methods
      val fs = Map(fields.map(field => (field, Cell(0))): _*)
      val ms = Map(methods: _*)
      val z = zuper map ((z: Clazz) => apply(Map.empty)(New(z)).get.right.get)
      Cell(Right(Instance(z, fs, ms)))
    }
    case Selection(receiver, field) => {
      // assume the expression evaluates to a record (.right)
      // and choose the desired field
      val rec = apply(store)(receiver)
      // if this selection is on the receiver of the current method,
      // then look for the field in the static scope of the method
      if (! store.contains("this") || rec.get.right.get != store("this").get.right.get) {
    	  rec.get.right.get.getField(field)
      } else {
    	  store("scope").get.right.get.getField(field)
      }
    }
    case Message(receiver, method, arguments @ _*) => {
      // evaluate receiver expression to a Cell containing an Instance
      val rec = apply(store)(receiver)
      // look up method in the Instance's second component (method table)
      // this gives you the subobject of the Instance corresponding to
      // the static scope of the method, as well as the method's arguments and body
      val (scope, (vars, meth)) = rec.get.right.get.getScopedMethod(method)
      // set up static superclass scope for method
      val zup = if (scope.zuper.isDefined) Cell(Right(scope.zuper.get)) else Cell.NULL
      // evaluate the arguments
      val args = arguments.map(apply(store))
      // create argument bindings "0" -> arg(0), "1" -> arg(1), etc.
      val argBindings = (0 until args.length) map (_.toString) zip args
      // create bindings for the local variables
      val localBindings = vars map (field => (field, Cell(0)))
      // augment the store with these new bindings
      // plus bindings for the pseudo-variables this, scope (static this), and super
      val storeWithBindings = store +
      	("this" -> rec) + ("scope" -> Cell(Right(scope))) + ("super" -> zup) ++
      	argBindings ++ localBindings
      // finally execute the resulting Statement in the augmented store
      // (note that this automatically returns the result if there is one)
      apply(storeWithBindings)(meth)
    }
  }

  def binaryOperation(store: Store, left: Statement, right: Statement, operator: (Int, Int) => Int): Cell = {
    val l: Int = apply(store)(left).get.left.get
    val r: Int = apply(store)(right).get.left.get
    Cell(Left(operator(l, r)))
  }
}
