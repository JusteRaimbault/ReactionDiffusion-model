package density.reactiondiffusion

import scala.util.Random

case class CSVGridGenerator(
                             file: String,
                             separator: String = ","
                           ) extends GridGenerator {

  override def generateGrid(implicit rng: Random): RasterLayerData[Double] = CSV.readMat(file,separator,naformat = "NA")

}
