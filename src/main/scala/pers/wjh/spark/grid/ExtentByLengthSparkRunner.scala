package pers.wjh.spark.grid

import org.apache.spark.sql.{Dataset, SparkSession}

case class ExtentByLengthSparkRunner(xLength: Double, yLength: Double)
                                    (implicit spark:SparkSession){
  import spark.implicits._
  def separate(extentDs: Dataset[Extent]): Dataset[Extent] = {
    val lv = ExtentByLengthSparkRunner.getLevelsValue(extentDs, xLength, yLength)
    println(s"共分 ${lv._1} 层进行处理")
    var extLevelDs: Dataset[Extent] = extentDs
    lv._2.foreach(v => extLevelDs = extLevelDs.flatMap(e => e.separate(v._1, v._2)).repartition($"id"))
    extLevelDs
  }
}
object ExtentByLengthSparkRunner {
  /**
   * 每次处理每个边的最大分区数
   */
  val CE_COUNT = 100

  private[grid] def getLevelsValue(extentDs: Dataset[Extent],
                                   xLength: Double,
                                   yLength: Double): (Int, Seq[(Double,Double)]) = {
    val maxExtent = extentDs.reduce((e1, e2) =>
      Extent("s", math.min(e1.xMin, e2.xMin), math.min(e1.yMin, e2.yMin),
        math.max(e1.xMax, e2.xMax), math.max(e1.yMax, e2.yMax)))
    var level = 0
    val value =
      Iterator.continually{ level += 1; (xLength * math.pow(CE_COUNT, level), yLength * math.pow(CE_COUNT, level))}
        .takeWhile(v => v._1 <= maxExtent.xMax || v._2 <= maxExtent.yMax).toSeq.reverse :+ (xLength, yLength)
    (level, value)
  }
}