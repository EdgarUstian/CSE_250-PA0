/**
 * Functions.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT:
 * Person#:
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa0.objects

object Functions {
  def genNum(n: Int): Int = {
    n + 1
  }

  def genSeq(n: Int): Seq[Int] = {
    var seq: Seq[Int] = Seq()
    for (i <- 0 until n){
      seq = seq :+ i*2
    }
    seq
  }

  def funThree(n: Int): Int = {
    n + 1
  }

  def compSum(n: Int): Long = {
    var adder: Long = 0
    for (i <- 0 to n){
      adder += i
    }
    adder
  }
}
