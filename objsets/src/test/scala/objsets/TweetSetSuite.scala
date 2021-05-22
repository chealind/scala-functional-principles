package objsets

import org.junit.Assert.assertEquals
import org.junit._

class TweetSetSuite {

  trait TestSets {
    val set1 = new Empty
    val set2: TweetSet = set1.incl(new Tweet("a", "a body", 20))
    val set3: TweetSet = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val e = new Tweet("e", "e body", 31)
    val set4c: TweetSet = set3.incl(c)
    val set4d: TweetSet = set3.incl(d)
    val set5: TweetSet = set4c.incl(d)
    val set3cd: TweetSet = set4c.incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  @Test def `filter: on empty set`(): Unit =
    new TestSets {
      assertEquals(0, size(set1.filter(tw => tw.user == "a")))
    }

  @Test def `filter: a on set5`(): Unit =
    new TestSets {
      assertEquals(1, size(set5.filter(tw => tw.user == "a")))
    }

  @Test def `filter: twenty on set5`(): Unit =
    new TestSets {
      assertEquals(2, size(set5.filter(tw => tw.retweets == 20)))
    }

  @Test def `union: set4c and set4d`(): Unit =
    new TestSets {
      assertEquals(4, size(set4c.union(set4d)))
    }

  @Test def `union: with empty set1`(): Unit =
    new TestSets {
      assertEquals(4, size(set5.union(set1)))
    }

  @Test def `union: with empty set2`(): Unit =
    new TestSets {
      assertEquals(4, size(set1.union(set5)))
    }

  @Test def `mostRetweeted: on empty set`(): Unit =
    new TestSets {
      try {
        set1.mostRetweeted
        Assert.fail("Expecting exception")
      } catch {
        case _: NoSuchElementException => ()
      }
    }

  @Test def `mostRetweeted: on c and d`(): Unit =
    new TestSets {
      assertEquals(d, set1.incl(c).incl(d).mostRetweeted)
    }

  @Test def `mostRetweeted: on set5`(): Unit =
    new TestSets {
      assertEquals(20, set5.mostRetweeted.retweets)
    }

  @Test def `descending: empty set`(): Unit =
    new TestSets {
      val res: TweetList = set1.descendingByRetweet
      assert(res.isEmpty)
    }

  @Test def `descending: on set2`(): Unit =
    new TestSets {
      val res: TweetList = set2.descendingByRetweet
      assert(!res.isEmpty)
      assert(res.head.user == "a")
    }

  @Test def `descending: set3cd`(): Unit =
    new TestSets {
      val trends: TweetList = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a")
    }

  @Test def `descending: set5`(): Unit =
    new TestSets {
      val trends: TweetList = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }

  @Test def `filter and union`(): Unit =
    new TestSets {
      val tweetSet1: TweetSet = set2.incl(c) // 20,7
      val tweetSet2: TweetSet = set2.incl(d).incl(e) // 20,9,31

      val res: TweetSet = tweetSet1.filter(t => t.user == "a") union tweetSet2.filter(t => t.retweets > 20)
      val trends: TweetList = res.descendingByRetweet

      assertEquals(2, size(res))
      assert(trends.head.user == "e")
    }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
