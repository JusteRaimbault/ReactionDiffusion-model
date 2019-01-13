package density

import java.io.File

import scala.util.Random

class PADGeneratorLauncher {

  // static mutable fields to get indicators

  var moran: Array[Double] = Array.empty
  var distance: Array[Double] = Array.empty
  var entropy: Array[Double] = Array.empty
  var slope: Array[Double] = Array.empty
  var rsquared: Array[Double] = Array.empty

  def main(worldwidth: Int, pop: Double, diff: Double, diffSteps: Double, growth: Double, alpha: Double, replication: Int, morphoTimeStep: Int) = {

    println("Params : " + pop + " ; " + diff + " ; " + diffSteps + " ; " + growth + " ; " + alpha + " ; " + replication)

    // replication and diffsteps should be enough in 'small' explorations,
    // easier to retrieve later (exact int value)
    //val UIR = replication.toString + diffSteps.toString

    var t = System.currentTimeMillis()

    implicit val rng = new Random

    val gen = new PrefAttDiffusionGenerator {
      override def size: Int = worldwidth
      override def totalPopulation: Double = pop
      override def diffusion: Double = diff
      override def diffusionSteps: Int = diffSteps.toInt
      override def growthRate: Double = growth
      override def alphaAtt: Double = alpha
      //override def temp_file: String = "tmp/pop_" + UIR + ".csv"
      //override def export_file: File = f
      override def computeMorphoSteps: Int = morphoTimeStep
    }

    // compute
    val world = gen.world(rng)

    // export to file variable, created by openmole
    //gen.export_static(world, gen.export_file)

    moran = gen.morphoHistory.map(_.moran).toArray
    distance = gen.morphoHistory.map(_.distance).toArray
    entropy = gen.morphoHistory.map(_.entropy).toArray
    slope = gen.morphoHistory.map(_.slope).toArray
    rsquared = gen.morphoHistory.map(_.slopeRsquared).toArray

    println("Indicators : Moran = " + moran + " ; D = " + distance + " ; E = " + entropy + " ; alpha = " + slope + " ; R2 = " + rsquared)
    println("Ellapsed Time : " + (System.currentTimeMillis() - t) / 1000.0 + "\n")

  }

}
