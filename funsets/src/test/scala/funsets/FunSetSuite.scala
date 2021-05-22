package funsets

import funsets.FunSets._
import org.junit._

class FunSetSuite {

  trait TestSets {
    val s1: FunSet = singletonSet(1)
    val s2: FunSet = singletonSet(2)
    val s3: FunSet = singletonSet(3)
  }

  @Test def `contains is implemented`(): Unit = {
    assert(contains(_ => true, 100))
    assert(!contains((e: Int) => e > 0, -1))
  }

  @Test def `singleton set one contains one`(): Unit = {
    new TestSets {
      assert(contains(s1, 1))
    }
  }

  @Test def `union contains all elements of each set`(): Unit = {
    new TestSets {
      val s12: FunSet = union(s1, s2)
      assert(contains(s12, 1))
      assert(contains(s12, 2))
      assert(!contains(s12, 3))

      val s123: FunSet = union(s12, s3)
      assert(contains(s123, 1))
      assert(contains(s123, 2))
      assert(contains(s123, 3))
    }
  }

  @Test def `intersect contains mutual elements of sets`(): Unit = {
    new TestSets {
      val s12: FunSet = union(s1, s2)
      val s23: FunSet = union(s2, s3)

      val interSet: FunSet = intersect(s12, s23)
      assert(!contains(interSet, 1))
      assert(contains(interSet, 2))
      assert(!contains(interSet, 3))
    }
  }

  @Test def `diff of sets`(): Unit = {
    new TestSets {
      val s12: FunSet = union(s1, s2)
      val s23: FunSet = union(s2, s3)

      val diffSet: FunSet = diff(s12, s23)
      assert(contains(diffSet, 1))
      assert(!contains(diffSet, 2))
      assert(!contains(diffSet, 3))
    }
  }

  @Test def `filter on sets`(): Unit = {
    new TestSets {
      val s12: FunSet = union(s1, s2)
      val s123: FunSet = union(s12, s3)

      val filterSet: FunSet = filter(s123, s => s > 2)
      assert(!contains(filterSet, 1))
      assert(!contains(filterSet, 2))
      assert(contains(filterSet, 3))
    }
  }

  @Test def `forall on sets`(): Unit = {
    new TestSets {
      val s12: FunSet = union(s1, s2)
      val s123: FunSet = union(s12, s3)

      assert(forall(s12, s => s < 3))
      assert(!forall(s12, s => s == 0))
      assert(forall(s123, s => s > 0))
      assert(!forall(s123, s => s < 3))
    }
  }

  @Test def `exists on sets`(): Unit = {
    new TestSets {
      val s12: FunSet = union(s1, s2)
      val s123: FunSet = union(s12, s3)

      assert(exists(s12, s => s > 1))
      assert(!exists(s12, s => s == 0))
      assert(exists(s123, s => s > 2))
      assert(!exists(s123, s => s > 3))
    }
  }

  @Test def `map on sets`(): Unit = {
    new TestSets {
      val res: FunSet = map(union(s1, s2), el => el * el)

      assert(contains(res, 1))
      assert(!contains(res, 2))
      assert(contains(res, 4))
    }
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
