package pers.wjh.spark.grid

import org.apache.spark.sql.{Dataset, SparkSession}

/**
 * spark 建立分区
 * @param maxEdge 粒度的边界最大值
 * @param xCount 每次 x 分隔数
 * @param yCount 每次 y 分隔数
 * @param spark sparkSession
 */
case class ExtentSparkRunner(maxEdge: Double, xCount: Int, yCount: Int)
                            (implicit spark:SparkSession){
  require(maxEdge > 0)
  require(xCount > 1)
  require(yCount > 1)

  import spark.implicits._
  def separate(extentDs: Dataset[Extent]): Dataset[Extent] = {
    val exa = extentDs.head()
    if(exa.xMax - exa.xMin > maxEdge || exa.yMax - exa.yMin > maxEdge){
      separate(extentDs.flatMap(e => e.separate(xCount, yCount))).repartition($"id")
    } else {
      extentDs
    }
  }
}
