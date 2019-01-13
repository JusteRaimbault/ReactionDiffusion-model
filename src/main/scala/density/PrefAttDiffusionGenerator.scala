package density

import scala.util.Random

trait PrefAttDiffusionGenerator extends Generator {

  /** sum of all capacities */
  def totalPopulation: Double

  /** diffusion parameters */
  def diffusion: Double
  def diffusionSteps: Int

  /** Growth rate */
  def growthRate: Double

  /** Preferential attachment parameter */
  def alphaAtt: Double

  /** Initial configuration, empty by default **/
  def initialConfiguration: World = Seq.fill[Cell](size, size) { new Cell(0) }

  /**
    * Steps at which morphology should be computed - not done if -1
    * @return
    */
  def computeMorphoSteps: Int = -1

  /**
    * historical trajectories
    */
  var worldHistory: Seq[World] = Seq.empty

  var morphoHistory: Seq[Morphology] = Seq.empty

  /**
   * Compute the final world and fills history
   * @param rng
   * @return
   */
  def world(implicit rng: Random): World = {
    val initWorld: World=initialConfiguration
    worldHistory = Seq(initWorld)
    if (computeMorphoSteps>0){morphoHistory=Seq(Morphology(initWorld))}
    var arrayVals = initWorld.map(_.toArray).toArray
    var population: Double = 0
    var t=0

    while (population < totalPopulation) {

      // add new population following pref att rule
      if (population == 0) {
        //choose random patch
        for (_ <- 1 to growthRate.toInt) { val i = rng.nextInt(size); val j = rng.nextInt(size); arrayVals(i)(j).population = arrayVals(i)(j).population + 1 }
      } else {
        val oldPop = arrayVals.map((a: Array[Cell]) => a.map((c: Cell) => math.pow(c.population / population, alphaAtt)))
        val ptot = oldPop.flatten.sum

        for (_ <- 1 to growthRate.toInt) {
          var s = 0.0; val r = rng.nextDouble(); var i = 0; var j = 0
          //println("r : "+r)
          //draw the cell from cumulative distrib
          while (s < r) {
            s = s + (oldPop(i)(j) / ptot)
            j = j + 1
            if (j == size) { j = 0; i = i + 1 }
          }
          //println("   s : "+s+" ij :"+i+","+j);
          //rectify j
          if (j == 0) { j = size - 1; i = i - 1 } else { j = j - 1 };
          arrayVals(i)(j).population = arrayVals(i)(j).population + 1
        }
      }

      // diffuse
      for (_ <- 1 to diffusionSteps) {
        arrayVals = diffuse(arrayVals, diffusion)
      }

      // update total population
      population = arrayVals.flatten.map(_.population).sum
      t=t+1

      // update history
      worldHistory = worldHistory++Seq(Seq.tabulate(size, size) { (i: Int, j: Int) => new Cell(arrayVals(i)(j).population) })
      if(computeMorphoSteps>0&&t%computeMorphoSteps==0){morphoHistory=morphoHistory++Seq(Morphology(arrayVals))}

    }

    worldHistory.last
  }

  /**
   * Diffuse to neighbors proportion alpha of capacities
   *
   *  TODO : check if bias in diffusion process (bord cells should loose as much as inside cells)
   *
   * @param a
   */
  def diffuse(a: Array[Array[Cell]], alpha: Double): Array[Array[Cell]] = {
    val newVals = a.clone()
    for (i <- 0 to size - 1; j <- 0 to size - 1) {
      // diffuse in neigh cells
      if (i >= 1) { newVals(i - 1)(j).population = newVals(i - 1)(j).population + (alpha / 8) * a(i)(j).population }
      if (i < size - 1) { newVals(i + 1)(j).population = newVals(i + 1)(j).population + (alpha / 8) * a(i)(j).population }
      if (j >= 1) { newVals(i)(j - 1).population = newVals(i)(j - 1).population + (alpha / 8) * a(i)(j).population }
      if (j < size - 1) { newVals(i)(j + 1).population = newVals(i)(j + 1).population + (alpha / 8) * a(i)(j).population }
      if (i >= 1 && j >= 1) { newVals(i - 1)(j - 1).population = newVals(i - 1)(j - 1).population + (alpha / 8) * a(i)(j).population }
      if (i >= 1 && j < size - 1) { newVals(i - 1)(j + 1).population = newVals(i - 1)(j + 1).population + (alpha / 8) * a(i)(j).population }
      if (i < size - 1 && j >= 1) { newVals(i + 1)(j - 1).population = newVals(i + 1)(j - 1).population + (alpha / 8) * a(i)(j).population }
      if (i < size - 1 && j < size - 1) { newVals(i + 1)(j + 1).population = newVals(i + 1)(j + 1).population + (alpha / 8) * a(i)(j).population }
      //delete in the cell (¡ bord effect : lost portion is the same even for bord cells !)
      // to implement diffuse as in NL, put deletion inside boundary conditions checking
      newVals(i)(j).population = newVals(i)(j).population - alpha * a(i)(j).population
    }
    newVals
  }

}
