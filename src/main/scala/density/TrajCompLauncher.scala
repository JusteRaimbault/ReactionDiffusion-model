package density

import scala.util.Random


class TrajCompLauncher {

  var distances: Array[Double] = Array.empty

  def main(worldwidth: Int,initMaxPop:Double, initKernelRadius: Double,
           pop: Double, diff: Double, diffSteps: Double, growth: Double, alpha: Double,
           replication: Int): Unit = {

    println("Params : " + pop + " ; " + diff + " ; " + diffSteps + " ; " + growth + " ; " + alpha + " ; " + replication)

    var t = System.currentTimeMillis()

    implicit val rng = new Random

    val initConf = new ExpMixtureGenerator {
      override def maxPopulation: Double = initMaxPop
      override def kernelRadius: Double = initKernelRadius
      override def centersNumber: Int = 0
      override def size: Int = worldwidth
      override def centersCoords:Seq[(Int,Int)] = Seq((worldwidth/4,worldwidth/4),(3*worldwidth/4,worldwidth/4),(worldwidth/4,3*worldwidth/4),(3*worldwidth/4,3*worldwidth/4))
    }

    val gen = new PrefAttDiffusionGenerator {
      override def size: Int = worldwidth
      override def totalPopulation: Double = pop
      override def diffusion: Double = diff
      override def diffusionSteps: Int = diffSteps.toInt
      override def growthRate: Double = growth
      override def alphaAtt: Double = alpha
      override def initialConfiguration: World = initConf.world
    }

    // compute
    val world1 = gen.world(rng)
    val trajs1 = gen.worldHistory
    val world2 = gen.world(rng)
    val trajs2 = gen.worldHistory

    val distsSeq = trajs1.zip(trajs2).map{case (w1,w2) => Morphology.worldDist(w1,w2)}
    distances = (distsSeq++Seq.fill(210 - distsSeq.size){-1.0}).toArray
    println("Ellapsed Time : " + (System.currentTimeMillis() - t) / 1000.0 + "\n")

  }
}
