package pers.wjh.spark.grid

/**
 * 范围
 * @param id 标识
 * @param xMin 左侧边界
 * @param yMin 下侧边界
 * @param xMax 右侧边界
 * @param yMax 上侧边界
 */
case class Extent(id:String, xMin: Double, yMin: Double, xMax: Double, yMax: Double) {
  def separate(xCount: Int, yCount: Int): Seq[Extent] = {
    val xSep = Extent.separate(xMin, xMax, xCount)
    val ySep = Extent.separate(yMin, yMax, yCount)
    for(x <- 0 until xSep.length - 1; y <- 0 until ySep.length - 1) yield {
      Extent(s"$id-${x * (ySep.length - 1) + y}",xSep(x), ySep(y), xSep(x + 1), ySep(y + 1))
    }
  }
}

object Extent{
  private[grid] def separate(start: Double, end: Double, count: Int): Seq[Double] = {
    require(end > start)
    require(count > 1)
    val step = (end - start) / count
    (for (i <- 0 until count) yield start + i * step) :+ end
  }
}