package u06lab.solution

/**
  * 1) Implement trait Functions with an object FunctionsImpl such that the code
  * in TryFunctions works correctly.
 */

trait Functions {
  def sum(a: List[Double]): Double
  def concat(a: Seq[String]): String
  def max(a: List[Int]): Int // gives Int.MinValue if a is empty
}

trait Combiner[A] {
  def unit: A
  def combine(a: A, b: A): A
}

object Combiner {
  def apply[A](base: A, c: (A, A) => A): Combiner[A] = new Combiner[A] {
    override def unit: A = base
    override def combine(a: A, b: A): A = c(a,b)
  }

  implicit val sumCombiner: Combiner[Double] = Combiner(0.0, _+_)
  implicit val concatCombiner: Combiner[String] = Combiner("", _+_)
  implicit val maxCombiner:Combiner[Int] = Combiner(Int.MinValue, Math.max)
}

/*
// VERSIONE ALTENRATIVA
object Combiner {

  implicit object SumCombiner extends Combiner[Double] {
    override def unit:Double = 0.0
    override def combine(a: Double, b: Double): Double = a + b
  }

  implicit object ConcatCombiner extends Combiner[String] {
    override def unit:String = ""
    override def combine(a: String, b: String): String = a + b
  }

  implicit object MaxCombiner extends Combiner[Int] {
    override def unit:Int = Int.MinValue
    override def combine(a: Int, b: Int): Int = Math.max(a, b)
  }
}
*/

object FunctionsImpl extends Functions {

  override def sum(a: List[Double]): Double = combine(a)

  override def concat(a: Seq[String]): String = combine(a)

  override def max(a: List[Int]): Int = combine(a)

  private def combine[A](a:Iterable[A])(implicit c:Combiner[A]):A = a.foldLeft(c.unit)(c.combine)
}