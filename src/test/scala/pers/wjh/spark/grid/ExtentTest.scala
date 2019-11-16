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

      Extent("s", 0, 0, 10, 10).separate(2, 2).foreach(println)

      ok
    }
  }
}
