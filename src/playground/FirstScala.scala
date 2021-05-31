package org.scala.practice
package playground

import scala.annotation.tailrec

object FirstScala extends App {
  println("Hello Scala")

  def sum(a: Int, b: Int): Int = a + b

  /**
   * Factorial program
   */
  @tailrec
  def factorial(n: Int, acc: BigInt = 1): BigInt = {
    if (n <= 1)
      acc
    else
      factorial(n - 1, n * acc)
  }

  println(sum(2, 3))
  println(factorial(5000))
}
