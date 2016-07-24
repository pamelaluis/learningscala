package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains elements common to each set"){
    new TestSets {
      val a = union(s1,s2)
      val b = union(s2, s3)
      assert(contains(intersect(a,b), 2))
      assert(!contains(intersect(a,b), 1))
      assert(!contains(intersect(a,b), 3))
    }
  }

  test("diff contains elements in s that are not in t"){
    new TestSets {
      val a = union(s1,s2)
      val b = union(s2, s3)
      assert(contains(diff(a,b), 1))
      assert(!contains(diff(a,b), 2))
      assert(!contains(diff(a,b), 3))
    }
  }

  test("filter out negative elements"){
    new TestSets {
      val a = union(union(union(s1,s2), s3),singletonSet(-1))
      private val filter1: Set = filter(a, x => x > 0)
      assert(contains(filter1, 1))
      assert(contains(filter1, 2))
      assert(contains(filter1, 3))
      assert(!contains(filter1, -1))
      private val filter2: Set = x => x < 0
      assert(!contains(filter2, 1))
      assert(!contains(filter2, 2))
      assert(!contains(filter2, 3))
      assert(contains(filter2, -1))
    }
  }

  test("verify forall"){
    new TestSets {
      val set1 = union(union(union(s1, s2), s3), singletonSet(-4))
      assert(!forall(set1, p => p > 0))
      assert(!forall(set1, p => p < 0))
      val filteredSet = filter(set1, x => x > 0)
      assert(forall(filteredSet, p => p > 0))
    }
  }

  test("exists") {
    new TestSets {
      val set1 = union(union(union(s1, s2), s3), singletonSet(-4))
      assert(exists(set1, p => p < 0), "failed to find a negative value")
      val filteredSet = filter(set1, x => x > 0)
      assert(!exists(filteredSet, p => p < 0), "failed - found a negative value")
      val set2 = union(s => s >= 0 && s <= 100, t => t == -4)
      assert(!exists(set2, p => p == 250), "failed found 250 in the set")
      assert(exists(set2, p => p == -4), "failed to find -4")
      assert(!exists(set2, p => p == -999), "failed - found -999")
      assert(!exists(set2, p => p == 999), "failed - found 999")
      assert(!exists(set2, p => p == -1), "failed - found -1")
    }
  }

  test("map") {
    new TestSets {
      val zeroToHundredAndNegFour = union(s => s >= 0 && s <= 100, t => t == -4)
      val mappedSet = map(zeroToHundredAndNegFour, x => x * 2)
      assert(exists(mappedSet, singletonSet(200)), "200 is not found in the mappedSet")
      assert(!exists(mappedSet, singletonSet(99)), "failed 99 is found in the mappedSet")
      assert(!exists(mappedSet, singletonSet(250)), "failed found 250 in the set")
      assert(!exists(mappedSet, singletonSet(-4)), "failed found -4")
      assert(!exists(mappedSet, singletonSet(-999)), "failed - found -999")
      assert(!exists(mappedSet, singletonSet(999)), "failed - found 999")
      assert(!exists(mappedSet, singletonSet(-1)), "failed - found -1")
      assert(exists(mappedSet,singletonSet(102)),"failed to find 102 in the set")
      assert(exists(mappedSet,singletonSet(-8)),"failed to find -8 in the set")
      assert(!exists(mappedSet,singletonSet(1)),"failed found 1 in the set")
    }
  }

}
