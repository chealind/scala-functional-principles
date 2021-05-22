package patmat

import org.junit._
import org.junit.Assert.assertEquals

class HuffmanSuite {

  import Huffman._

  trait TestTrees {
    val t1: Fork = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2: Fork = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  @Test def `weight of a larger tree (10pts)`(): Unit =
    new TestTrees {
      assertEquals(5, weight(t1))
    }

  @Test def `chars of a larger tree (10pts)`(): Unit =
    new TestTrees {
      assertEquals(List('a', 'b', 'd'), chars(t2))
    }

  @Test def `string2chars hello world`(): Unit =
    assertEquals(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'), string2Chars("hello, world"))

  @Test def `times for empty list`(): Unit =
    new TestTrees {
      assertEquals(List(), times(List()))
    }

  @Test def `times for single character`(): Unit =
    new TestTrees {
      assertEquals(List(('a', 1)), times(List('a')))
    }

  @Test def `times for two characters`(): Unit =
    new TestTrees {
      assertEquals(List(('a', 2), ('b', 1)), times(List('a', 'b', 'a')))
    }

  @Test def `times for hello`(): Unit =
    new TestTrees {
      assertEquals(List(('h', 1), ('e', 1), ('l', 2), ('o', 1)), times(List('h', 'e', 'l', 'l', 'o')))
    }

  @Test def `make ordered leaf list for some frequency table (15pts)`(): Unit =
    new TestTrees {
      assertEquals(List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3), Leaf('n', 4)), makeOrderedLeafList(List(('t', 2), ('n', 4), ('e', 1), ('x', 3))))
    }

  @Test def `singleton on empty list`(): Unit =
    new TestTrees {
      assertEquals(false, singleton(List()))
    }

  @Test def `singleton on list with single tree`(): Unit =
    new TestTrees {
      assertEquals(true, singleton(List(t1)))
    }

  @Test def `singleton on list with two trees`(): Unit =
    new TestTrees {
      assertEquals(false, singleton(List(t1, t2)))
    }

  @Test def `combine of singleton list`(): Unit = {
    val leaflist = List(Leaf('e', 1))
    assertEquals(List(Leaf('e', 1)), combine(leaflist))
  }

  @Test def `combine of some leaf list (15pts)`(): Unit = {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)), combine(leaflist))
  }

  @Test def `combine of leaf list reordering`(): Unit = {
    val leaflist = List(Leaf('e', 4), Leaf('t', 2), Leaf('x', 4))
    assertEquals(List(Leaf('x', 4), Fork(Leaf('e', 4), Leaf('t', 2), List('e', 't'), 6)), combine(leaflist))
  }

  @Test def `combine of leaf and for list`(): Unit = {
    val list = List(Leaf('e', 4), Fork(Leaf('a', 8), Leaf('b', 4), List('a', 'b'), 12), Leaf('x', 18))
    assertEquals(List(Fork(Leaf('e', 4), Fork(Leaf('a', 8), Leaf('b', 4), List('a', 'b'), 12), List('e', 'a', 'b'), 16), Leaf('x', 18)), combine(list))
  }

  @Test def `until of some leaf list`(): Unit = {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(List(Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7)), until(singleton, combine)(leaflist))
  }

  @Test def `createCodeTree for hello`(): Unit = {
    val chars = string2Chars("hello")
    val tree = Fork(Leaf('l', 2), Fork(Leaf('o', 1), Fork(Leaf('h', 1), Leaf('e', 1), List('h', 'e'), 2), List('o', 'h', 'e'), 3), List('l', 'o', 'h', 'e'), 5)
    assertEquals(tree, createCodeTree(chars))
  }

  @Test def `decode hello tree`(): Unit = {
    val bits: List[Bit] = List(1, 1, 0, 1, 1, 1, 0, 0, 1, 0)
    val tree = Fork(Leaf('l', 2), Fork(Leaf('o', 1), Fork(Leaf('h', 1), Leaf('e', 1), List('h', 'e'), 2), List('o', 'h', 'e'), 3), List('l', 'o', 'h', 'e'), 5)
    assertEquals(List('h', 'e', 'l', 'l', 'o'), decode(tree, bits))
  }

  @Test def `decode secret`(): Unit = {
    assertEquals(List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'), decodedSecret)
  }

  @Test def `encode hello tree`(): Unit = {
    val bits: List[Bit] = List(1, 1, 0, 1, 1, 1, 0, 0, 1, 0)
    val tree = Fork(Leaf('l', 2), Fork(Leaf('o', 1), Fork(Leaf('h', 1), Leaf('e', 1), List('h', 'e'), 2), List('o', 'h', 'e'), 3), List('l', 'o', 'h', 'e'), 5)
    assertEquals(bits, encode(tree)("hello".toList))
  }

  @Test def `decode and encode a very short text should be identity (10pts)`(): Unit =
    new TestTrees {
      assertEquals("ab".toList, decode(t1, encode(t1)("ab".toList)))
    }

  @Test def `code bits for hello tree`(): Unit = {
    val table = List(('e', List(1, 1, 1)), ('h', List(1, 1, 0)), ('o', List(1, 0)), ('l', List(0)))
    assertEquals(List(1, 1, 0), codeBits(table)('h'))
    assertEquals(List(1, 1, 1), codeBits(table)('e'))
    assertEquals(List(0), codeBits(table)('l'))
    assertEquals(List(1, 0), codeBits(table)('o'))
  }

  @Test def `convert hello tree`(): Unit = {
    val tree = Fork(Leaf('l', 2), Fork(Leaf('o', 1), Fork(Leaf('h', 1), Leaf('e', 1), List('h', 'e'), 2), List('o', 'h', 'e'), 3), List('l', 'o', 'h', 'e'), 5)
    assertEquals(List(('e', List(1, 1, 1)), ('h', List(1, 1, 0)), ('o', List(1, 0)), ('l', List(0))), convert(tree))
  }

  @Test def `quickEncode hello tree`(): Unit = {
    val bits: List[Bit] = List(1, 1, 0, 1, 1, 1, 0, 0, 1, 0)
    val tree = Fork(Leaf('l', 2), Fork(Leaf('o', 1), Fork(Leaf('h', 1), Leaf('e', 1), List('h', 'e'), 2), List('o', 'h', 'e'), 3), List('l', 'o', 'h', 'e'), 5)
    assertEquals(bits, quickEncode(tree)("hello".toList))
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
