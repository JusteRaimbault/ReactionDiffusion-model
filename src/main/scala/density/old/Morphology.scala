package density.old

import org.apache.commons.math3.stat.regression.SimpleRegression
import org.apache.commons.math3.util.MathArrays

import scala.math._

case class Morphology(
                     slope: Double,
                     slopeRsquared: Double,
                     distance: Double,
                     entropy: Double,
                     moran: Double
                     )


object Morphology {

  /**
    * Compute morphological indicators
    * @param world
    */
  def apply(world: World): Morphology = {
    val s = slope(world)
    Morphology(s._1,s._2,distance(world),entropy(world),moran(world))
  }

  def apply(mat:Array[Array[Cell]]): Morphology = Morphology(Seq.tabulate(mat.length, mat(0).length) { (i: Int, j: Int) => new Cell(mat(i)(j).population) })


  /**
    * Euclidian distance between two configurations
    * @param world1
    * @param world2
    * @return
    */
  def worldDist(world1: World,world2: World): Double = math.sqrt(world1.flatten.zip(world2.flatten).map{case (c1,c2) => (c1.population - c2.population)*(c1.population - c2.population)}.sum)



  /**
   * Rank-size slope
   *
   * @param matrix
   * @return
   */
  def slope(matrix: Seq[Seq[Cell]]) = {
    def distribution = matrix.flatten.map(_.population).sorted(Ordering.Double.reverse).filter(_ > 1)
    def distributionLog = distribution.zipWithIndex.map { case (q: Double, i: Int) => Array(log(i.toDouble + 1), log(q)) }.toArray
    val simpleRegression = new SimpleRegression(true)
    simpleRegression.addData(distributionLog)
    (simpleRegression.getSlope(), simpleRegression.getRSquare())
  }

  /**
   * Mean distance between individuals.
   *
   * @param matrix
   * @return
   */
  def distanceMean(matrix: Seq[Seq[Cell]]) = {

    def totalQuantity = matrix.flatten.map(_.population).sum

    def numerator =
      (for {
        (c1, p1) <- zipWithPosition(matrix)
        (c2, p2) <- zipWithPosition(matrix)
      } yield distance(p1, p2) * c1.population * c2.population).sum

    def normalisation = matrix.length / math.sqrt(math.Pi)

    println(numerator)

    (numerator / (totalQuantity * (totalQuantity))) / normalisation
  }

  /**
   * Distance between two positions.
   *
   * @param p1
   * @param p2
   * @return
   */
  def distance(p1: (Int, Int), p2: (Int, Int)): Double = {
    val (i1, j1) = p1
    val (i2, j2) = p2
    val a = i2 - i1
    val b = j2 - j1
    math.sqrt(a * a + b * b)
  }

  def zipWithPosition(m: Seq[Seq[Cell]]): Seq[(Cell, (Int, Int))] = {
    for {
      (row, i) <- m.zipWithIndex
      (content, j) <- row.zipWithIndex
    } yield content -> (i, j)
  }

  def zipDoubleArrayWithPosition(m: Array[Array[Double]]): Array[(Double, (Int, Int))] = {
    for {
      (row, i) <- m.zipWithIndex
      (d, j) <- row.zipWithIndex
    } yield d -> (i, j)
  }

  /**
   * Entropy of population distribution.
   *
   * @param matrix
   * @return
   */
  def entropy(matrix: Seq[Seq[Cell]]) = {
    val totalQuantity = matrix.flatten.map(_.population).sum
    //assert(totalQuantity > 0)
    totalQuantity match {
      case 0.0 => 1.0
      case _ => matrix.flatten.map {
        p =>
          val quantityRatio = p.population / totalQuantity
          val localEntropy = if (quantityRatio == 0.0) 0.0 else quantityRatio * math.log(quantityRatio)
          //assert(!localEntropy.isNaN, s"${quantityRatio} ${math.log(quantityRatio)}")
          localEntropy
      }.sum * (-1 / math.log(matrix.flatten.length))
    }
  }

  /**
   * Moran Index.
   *  (in O(N4))
   *
   * @param matrix
   * @return
   */
  def moran_classic(matrix: Seq[Seq[Cell]]): Double = {
    def flatCells = matrix.flatten
    val totalPop = flatCells.map(_.population).sum
    val averagePop = totalPop / matrix.flatten.length

    def vals =
      for {
        (c1, p1) <- zipWithPosition(matrix)
        (c2, p2) <- zipWithPosition(matrix)
      } yield (decay(p1, p2) * (c1.population - averagePop) * (c2.population - averagePop), decay(p1, p2))

    def numerator: Double = vals.map { case (n, _) => n }.sum
    def totalWeight: Double = vals.map { case (_, w) => w }.sum

    def denominator =
      flatCells.map {
        cell =>
          (cell.population - averagePop) * (cell.population - averagePop)
      }.sum

    if (denominator == 0) 0
    else (matrix.flatten.length / totalWeight) * (numerator / denominator)
  }

  /**
   * Decay distance for Moran.
   *
   * @param p1
   * @param p2
   * @return
   */
  def decay(p1: (Int, Int), p2: (Int, Int)) = {
    if (p1 == p2) 0.0
    else 1 / distance(p1, p2)
  }

  /**
   * Moran index using fast convolution.
   *
   * @param matrix
   * @return
   */
  def moran(matrix: Seq[Seq[Cell]]): Double = {
    val conf = matrix.map { row => row.map { _.population }.toArray }.toArray
    val n = conf.length
    val flatConf = conf.flatten
    val popMean = flatConf.sum / flatConf.length
    val centeredConf = conf.map { r => r.map { d => d - popMean } }
    val variance = MathArrays.ebeMultiply(centeredConf.flatten, centeredConf.flatten).sum
    val weights = spatialWeights(2 * n - 1)
    val totWeight = Convolution.convolution2D(Array.fill(n, n) { 1.0 }, weights).flatten.sum
    flatConf.length / (totWeight * variance) * MathArrays.ebeMultiply(centeredConf.flatten, Convolution.convolution2D(centeredConf, weights).flatten).sum
  }

  def spatialWeights(n: Int): Array[Array[Double]] = {
    Array.tabulate(n, n) { (i, j) => if (i == n / 2 && j == n / 2) 0.0 else 1 / Math.sqrt((i - n / 2) * (i - n / 2) + (j - n / 2) * (j - n / 2)) }
  }

  /**
   * Mean distance using fast convolution.
   *
   * @param matrix
   * @return
   */
  def distance(matrix: Seq[Seq[Cell]]): Double = {
    val conf = matrix.map { row => row.map { _.population }.toArray }.toArray
    val totPop = conf.flatten.sum
    val dmat = distanceMatrix(2 * conf.length - 1)
    val conv = Convolution.convolution2D(conf, dmat)
    math.sqrt(math.Pi) / (conf.length * totPop * totPop) * MathArrays.ebeMultiply(conv.flatten, conf.flatten).sum
  }

  /**
   * Distance kernel
   *
   * @param n
   * @return
   */
  def distanceMatrix(n: Int): Array[Array[Double]] = {
    Array.tabulate(n, n) { (i, j) => Math.sqrt((i - n / 2) * (i - n / 2) + (j - n / 2) * (j - n / 2)) }
  }

  def printMat(m: Array[Array[Double]]) = {
    m.map { r => println(r.mkString(" ")) }; println("\n")
  }

}
