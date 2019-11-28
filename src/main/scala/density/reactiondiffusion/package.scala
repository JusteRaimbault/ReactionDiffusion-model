package density

import scala.util.Random


package object reactiondiffusion {

  type RasterDim = Either[Int,(Int,Int)]

  implicit def rasterDimConversion(i:Int): RasterDim = Left(i)
  implicit def rasterDimConversion(c:(Int,Int)): RasterDim = Right(c)

  type RasterLayerData[N] = Array[Array[N]]


  implicit class TraversableDecorator[T](s: Traversable[T]){

    def sampleWithReplacement(samples: Int)(implicit rng: Random): Vector[T] = Stochastic.sampleWithReplacement(s, samples)

    def sampleWithoutReplacement(samples: Int)(implicit rng: Random): Vector[T] = Stochastic.sampleWithoutReplacement(s, samples)

    def shuffle(implicit rng: Random): Seq[T] = rng.shuffle(s.toSeq)

  }


}
