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
  /**
   * 按数量均分
   * @param xCount x 方向均分数量
   * @param yCount y 方向均分数量
   * @return 均分后的网格
   */
  def separate(xCount: Int, yCount: Int): Seq[Extent] = {
    createExtents(Extent.separate(xMin, xMax, xCount), Extent.separate(yMin, yMax, yCount))
  }

  /**
   * 按长度划分
   * @param xLength x 方向每块长度
   * @param yLength y 方向每块长度
   * @return 均分后的网格
   */
  def separate(xLength: Double, yLength: Double): Seq[Extent] = {
    createExtents(Extent.separate(xMin, xMax, xLength), Extent.separate(yMin, yMax, yLength))
  }

  /**
   * 按数量均分
   * @param count x 以及 y 方向上的数量
   * @return 均分后的网格
   */
  def separate(count: Int): Seq[Extent] = separate(count, count)

  /**
   * 按长度划分
   * @param length x 以及 y 方向上的长度
   * @return
   */
  def separate(length: Double): Seq[Extent] = separate(length, length)

  private def createExtents(xSep: Seq[Double], ySep: Seq[Double]): Seq[Extent] ={
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

  private[grid] def separate(start: Double, end: Double, step: Double): Seq[Double] = {
    require(end > start)
    var pt = start
    start +: Iterator.continually{pt += step; pt}.takeWhile(_ < end).toSeq :+ end
  }
}