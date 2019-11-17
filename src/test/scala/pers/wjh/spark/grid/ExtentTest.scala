package pers.wjh.spark.grid

import org.specs2.mutable.Specification

class ExtentTest extends Specification {
  "均分数据" should {
    "数据分离" in {
      Extent.separate(5, 10, 5) must_=== Seq(5.0, 6.0, 7.0, 8.0, 9.0, 10.0)
    }

    "分离Extent" in {
      Extent("s", 5, 2, 10, 4).separate(5, 2) must containTheSameElementsAs(
        Seq(Extent("s-0",5.0,2.0,6.0,3.0),
          Extent("s-1",5.0,3.0,6.0,4.0),
          Extent("s-2",6.0,2.0,7.0,3.0),
          Extent("s-3",6.0,3.0,7.0,4.0),
          Extent("s-4",7.0,2.0,8.0,3.0),
          Extent("s-5",7.0,3.0,8.0,4.0),
          Extent("s-6",8.0,2.0,9.0,3.0),
          Extent("s-7",8.0,3.0,9.0,4.0),
          Extent("s-8",9.0,2.0,10.0,3.0),
          Extent("s-9",9.0,3.0,10.0,4.0))
      )
    }
  }
  "长度划分" should {
    "最后一个值保留" in {
      Extent.separate(5,11,2.5) must_=== Seq(5.0, 7.5, 10.0, 11.0)
    }


    "正常分隔Extent" in {
      val baseExt = Extent("s", 5, 2, 10, 10)
      val xLength = 1.5
      val yLength = 2.5
      baseExt.separate(xLength, yLength).map{ e =>
        (e.xMax - e.xMin must be_<=(xLength) when(e.xMax == baseExt.xMax))
          .and(e.yMax - e.yMin must be_<=(yLength) when(e.yMax == baseExt.yMax))
          .and((e.xMax - e.xMin must_=== xLength) when(e.xMax < baseExt.xMax))
          .and((e.yMax - e.yMin must_=== yLength) when(e.yMax < baseExt.yMax))
      }.reduce(_ and _)
    }
  }
}
