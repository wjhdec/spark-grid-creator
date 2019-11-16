package pers.wjh.spark.grid

import org.apache.spark.sql.SparkSession
import org.specs2.mutable.Specification

class ExtentSparkRunnerTest extends Specification {
  implicit val spark: SparkSession = SparkSession.builder().master("local").getOrCreate()
  import spark.implicits._
  "分离计算" should {
    "DataSet分离" in {
      val extentDs = Seq(Extent("s", -8, -8, 8, 8)).toDS()
      val sepDtDs = ExtentSparkRunner(1, 2, 2).separate(extentDs).cache()
      val headExtent = sepDtDs.head
      val extCount = sepDtDs.count()
      sepDtDs.show()
      sepDtDs.unpersist()
      (headExtent.xMax - headExtent.xMin) must_=== 1
      (headExtent.yMax - headExtent.yMin) must_=== 1
      extCount must_===  16* 16
    }
  }
}
