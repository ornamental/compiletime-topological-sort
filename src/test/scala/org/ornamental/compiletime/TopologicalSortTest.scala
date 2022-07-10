package org.ornamental.compiletime

import shapeless.::
import shapeless.HNil

object TopologicalSortTest {

  def main(args: Array[String]): Unit = {

    val functions =
      // Int => Double
      ((i: Int) => 2.5 * i) ::
        // () => Char
        (() => 'a') ::
        // (Char, List[Int]) => String
        ((ch: Char, lst: List[Int]) => lst.map(shift => (ch + shift).toChar).mkString) ::
        // () => List[Int]
        (() => List(-24, -65, 0, 12, -65, 0, -65, 1, 20, 12, 1, 11, 4, -52, 1, 4, 4)) ::
        // (Int, String) => List[String]
        ((i: Int, s: String) => s.split(' ').drop(i).toList) ::
        // Char => Int
        ((ch: Char) => 'c' - ch) :: HNil

    // any desired subset of functions' return types, in any order
    type Out = Int :: Char :: List[String] :: String :: Double :: HNil

    val sorted = TopologicalSort.sort[Out](functions)
    val expected = List(2, 'a', List("a", "bumble-bee"), "I am a bumble-bee", 5.0)

    println(s"Sorted:   ${sorted.toList.mkString(", ")}")
    println(s"Expected: ${expected.mkString(", ")}")

    assert(sorted.toList == expected)
  }
}
