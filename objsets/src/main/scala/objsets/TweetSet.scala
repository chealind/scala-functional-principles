package objsets

class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
      "Text: " + text + " [" + retweets + "]"
}

abstract class TweetSet extends TweetSetInterface {

  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty)

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  def union(that: TweetSet): TweetSet

  def mostRetweeted: Tweet

  def mostRetweeted(tweet: Tweet): Tweet

  def descendingByRetweet: TweetList

  def incl(tweet: Tweet): TweetSet

  def remove(tweet: Tweet): TweetSet

  def contains(tweet: Tweet): Boolean

  def isEmpty: Boolean

  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  def union(that: TweetSet): TweetSet = that

  def mostRetweeted: Tweet = throw new java.util.NoSuchElementException("EmptySet")

  def mostRetweeted(tweet: Tweet): Tweet = tweet

  def descendingByRetweet: TweetList = Nil

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def isEmpty: Boolean = true

  def foreach(f: Tweet => Unit): Unit = ()
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    val computeAcc = if (p(elem)) acc incl elem else acc
    left.filterAcc(p, right.filterAcc(p, computeAcc))
  }

  def union(that: TweetSet): TweetSet = left union (right union (that incl elem))

  def mostRetweeted: Tweet = mostRetweeted(elem)

  def mostRetweeted(tweet: Tweet): Tweet = {
    val most = if (elem.retweets > tweet.retweets) elem else tweet
    left.mostRetweeted(right.mostRetweeted(most))
  }

  def descendingByRetweet: TweetList = {
    new Cons(mostRetweeted, remove(mostRetweeted).descendingByRetweet)
  }

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def isEmpty: Boolean = false

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
}

trait TweetList {

  def head: Tweet

  def tail: TweetList

  def isEmpty: Boolean

  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {

  def head = throw new java.util.NoSuchElementException("head of EmptyList")

  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")

  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}

object GoogleVsApple {

  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet = extractTweets(google)
  lazy val appleTweets: TweetSet = extractTweets(apple)

  def extractTweets(keywords: List[String]): TweetSet = {
    TweetReader.allTweets.filter(tweet => keywords.exists(s => tweet.text.contains(s)))
  }

  lazy val trending: TweetList = {
    val allTweets = googleTweets.union(appleTweets)
    allTweets.descendingByRetweet
  }
}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}
