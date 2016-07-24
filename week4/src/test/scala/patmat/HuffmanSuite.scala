package patmat

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("weight of a smaller sub-tree") {
    new TestTrees {
      assert(weight(t2.left) === 5)
    }
  }

  test("weight of a leaf nodes") {
    new TestTrees {
      assert(weight(t2.right) === 4)
      assert(weight(t1.left) === 2)
      assert(weight(t1.right) === 3)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("chars of a smaller sub-tree") {
    new TestTrees {
      assert(chars(t2.left) === List('a', 'b'))
    }
  }

  test("chars of a leaf node") {
    new TestTrees {
      assert(chars(t1.left) === List('a'))
      assert(chars(t1.right) === List('b'))
    }
  }

  test("times") {
    assert(times(List('a', 'b', 'a', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c')) === List(('a', 2), ('b', 1), ('c', 9)))
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("combine of another leaf list - test ordering") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 2), Leaf('x', 2), Leaf('s', 3))
    assert(combine(leaflist) === List(Leaf('x', 2), Leaf('s',3), Fork(Leaf('e', 2), Leaf('t', 2), List('e', 't'), 4)))
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("encode french secret") {
    assert(encode(frenchCode)(decodedSecret) === secret)
  }

  test("quickEncode french secret") {
    assert(quickEncode(frenchCode)(decodedSecret) === secret)
  }

  test("convert code tree to code table") {
    assert(codeBits(convert(Fork(Fork(Leaf('A',1),Leaf('C',3),List('A','C'),4
    ),Leaf('B',1),List('A','B','C'),5)))('C') === List(0,1))
  }

//  test("test until") {
//    val x: List[CodeTree] = List(Leaf('H',1),Leaf('G',1),Leaf('F',1),Leaf('E',1),Leaf('D',1),Leaf('C',1),Leaf('B',3),Leaf('A',8))
//    assert(until(singleton,combine)(x) === x)
//  }
//
//  test("test until 2") {
//    val x: List[CodeTree] = List(Leaf('H',1),Leaf('G',1),Leaf('F',1),Leaf('E',1),Leaf('D',1),Leaf('C',1),Leaf('B',3))
//    assert(until(singleton,combine)(x) === x)
//  }


}
