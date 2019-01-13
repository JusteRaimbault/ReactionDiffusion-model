package density

import scala.util.Random

trait ExpMixtureGenerator extends Generator {

  /** maximal capacity C_m */
  def maxPopulation: Double

  /** Size of exponential kernels, of the form C_m*exp(-||x-x_0||/r_0) */
  def kernelRadius: Double

  /** Number of exponential kernels */
  def centersNumber: Int

  /** Fixed coordinates for centers **/
  def centersCoords:Seq[(Int,Int)] = Seq.empty

  def world(implicit rng: Random): Seq[Seq[Cell]] = {
    val arrayVals = Array.fill[Cell](size, size) {
      new Cell(0)
    }

    // generate random center positions
    val centers: Array[Array[Int]] = centersCoords.size match {
      case n if n == 0 => Array.fill[Int](centersNumber, 2) {rng.nextInt(size)}
      case _ => centersCoords.map{case c => Array(c._1,c._2)}.toArray
    }


    for (i <- 0 to size - 1; j <- 0 to size - 1) {
      for (c <- 0 to centersNumber - 1) {
        arrayVals(i)(j).population = arrayVals(i)(j).population + maxPopulation * math.exp(-math.sqrt(math.pow((i - centers(c)(0)), 2) + math.pow((j - centers(c)(1)), 2)) / kernelRadius)
      }
    }

    Seq.tabulate(size, size) { (i: Int, j: Int) => new Cell(arrayVals(i)(j).population) }

  }

}
