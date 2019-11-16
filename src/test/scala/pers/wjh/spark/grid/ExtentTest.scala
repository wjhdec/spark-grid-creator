package pers.wjh.spark.grid

import org.specs2.mutable.Specification

class ExtentTest extends Specification {
  "均分数据" should {
    "数据分离" in {
      Extent.separate(5, 10, 5) must_=== Seq(5.0, 6.0, 7.0, 8.0, 9.0, 10.0)
    }

    "分离Extent" in {
      Extent(5, 2, 10, 4).separate(5, 2) must containTheSameElementsAs(
        Seq(Extent(5.0, 2.0, 6.0, 3.0), Extent(5.0, 3.0, 6.0, 4.0),
          Extent(6.0, 2.0, 7.0, 3.0), Extent(6.0, 3.0, 7.0, 4.0),
          Extent(7.0, 2.0, 8.0, 3.0), Extent(7.0, 3.0, 8.0, 4.0),
          Extent(8.0, 2.0, 9.0, 3.0), Extent(8.0, 3.0, 9.0, 4.0),
          Extent(9.0, 2.0, 10.0, 3.0), Extent(9.0, 3.0, 10.0, 4.0))
      )
    }
  }
}
