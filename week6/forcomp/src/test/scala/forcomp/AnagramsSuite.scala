package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite  {

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }


  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }

  test("sentenceOccurrences: mummy daddy") {
    assert(sentenceOccurrences(List("mummy", "daddy")) === List(('a', 1), ('d', 3), ('m', 3), ('u', 1), ('y', 2)))
  }


  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }


  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }


  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }

  test("subtract: laaard - ra") {
    val laaard = List(('a', 3), ('d', 1), ('l', 1), ('r', 1))
    val ra = List(('a',1),('r', 1))
    val laad = List(('a', 2), ('d', 1), ('l', 1))
    assert(subtract(laaard, ra) === laad)
  }

  test("subtract: ehimr - e") {
    val lard = List(('e',2),('h',1),('i',1),('m',1),('r',1))
    val r = List(('e', 1), ('r', 1))
    val lad = List(('e', 1), ('h', 1), ('i', 1),('m', 1))
    assert(subtract(lard, r) === lad)
  }


  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: daddy") {
    val daddy: Occurrences = List(('a', 1), ('d', 3), ('y', 1))
    val daddyComb = List(
      List(),
      List(('a',1)),
      List(('a',1),('d',1),('y',1)),
      List(('a',1),('d',1)),
      List(('a',1),('d',2),('y',1)),
      List(('a',1),('d',2)),
      List(('a',1),('d',3),('y',1)),
      List(('a',1),('d',3)),
      List(('a',1),('y',1)),
      List(('d',1),('y',1)),
      List(('d',2),('y',1)),
      List(('d',3),('y',1)),
      List(('d',1)),
      List(('d',2)),
      List(('d',3)),
      List(('y',1))
    )
    assert(combinations(daddy).toSet === daddyComb.toSet)
  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assert(combinations(abba).toSet === abbacomb.toSet)
  }


//  test("sentence anagrams: []") {
//    val sentence = List()
//    assert(sentenceAnagrams(sentence) === List(Nil))
//  }

//  test("word combinations") {
//    println(wordCombinations(List(('e',1),('m',1),('i',1),('h',1)), List(
//      List(('e',1),('m',1),('i',1),('h',1)),
//    List(('e',1),('m',1),('i',1)),
//    List(('e',1),('m',1),('h',1)),
//    List(('e',1),('m',1)),
//    List(('e',1),('i',1),('h',1)),
//    List(('e',1),('i',1),('h',1)),
//    List(('e',1),('i',1)),
//    List(('e',1),('h',1)),
//    List(('e',1)),
//    List(('m',1),('i',1),('h',1)),
//    List(('m',1),('i',1)),
//    List(('m',1),('h',1)),
//    List(('m',1)),
//    List(('i',1),('h',1)),
//    List(('i',1)),
//    List()
//    )))
//  }

//  test("sentence anagrams: Im here") {
//    println(sentenceAnagrams(List("Im","here")))
//  }

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    println(sentenceAnagrams(sentence))
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }

}
