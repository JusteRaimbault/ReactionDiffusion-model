package density.reactiondiffusion

import org.apache.commons.math3.linear.{MatrixUtils, RealMatrix}

/**
  * matrix utilities (not linear algebra operations which are in [[Linear]]
  *
  * TODO test spark matrices https://spark.apache.org/docs/2.1.2/api/java/org/apache/spark/mllib/linalg/Matrix.html
  *
  */
object Matrix {


  def fromVector(vector: Vector[Vector[Double]]): RealMatrix = MatrixUtils.createRealMatrix(vector.map{_.toArray}.toArray)

  def fromColumnVector(vector: Vector[Double]): RealMatrix = fromColumnArray(vector.toArray)

  def toFlatVector(realMatrix: RealMatrix): Vector[Double] = realMatrix.getData.flatten.toVector

  def fromArray(a: Array[Array[Double]]): RealMatrix = MatrixUtils.createRealMatrix(a)

  def fromColumnArray(a: Array[Double]): RealMatrix = MatrixUtils.createColumnRealMatrix(a)

  def toFlatArray(realMatrix: RealMatrix): Array[Double] = realMatrix.getData.flatten

}
