package pers.wjh.spark.grid

import org.apache.spark.sql.SparkSession
import org.specs2.mutable.Specification

class ExtentByLengthSparkRunnerTest extends Specification {
  implicit val spark: SparkSession = SparkSession.builder().master("local").getOrCreate()
  import spark.implicits._
  "按距离划分" should {
    "需用最大数量的划分" in {
      val extentDs = Seq(Extent("s", 0, 0, 1000, 15000)).toDS()
      ExtentByLengthSparkRunner.getLevelsValue(extentDs, 0.01, 0.01)._2 must_===
        Seq((10000.0,10000.0), (100.0,100.0), (1.0,1.0), (0.01,0.01))
    }

    "DataSet分离" in {
      val extentDs = Seq(Extent("s", 0, 0, 1000, 1000)).toDS()
      val sepDtDs = ExtentByLengthSparkRunner(0.1, 0.1).separate(extentDs)
      sepDtDs.show()
      ok
    }
  }
}
