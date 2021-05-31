package org.scala.practice
package playground

import scala.annotation.tailrec

object FirstScala extends App {
  println("Hello Scala")

  /*
  Addition
   */
  def sum(a: Int, b: Int): Int = a + b

  /*
  Recursion
   */
  @tailrec
  def factorial(n: Int, acc: BigInt = 1): BigInt = {
    if (n <= 1)
      acc
    else
      factorial(n - 1, n * acc)
  }

  /*
  Repeating string
   */
  @tailrec
  def repeatedFunction(s: String, n: Int, ans: String): String =
    if (n <= 1) ans
    else repeatedFunction(s, n - 1, s + ans)

  /*
  Fibonacci Number
   */
  @tailrec
  def fibonacci(n: Int, a: Int, b: Int): Int =
    if (n <= 2) b
    else fibonacci(n - 1, b, a + b)

  /*
  Prime Number
   */
  def isPrime(n: Int): Boolean =
    true


  println(sum(2, 3))
  println(factorial(5000))
  println(repeatedFunction("Hello", 3, "Hello"))
  println(fibonacci(3, 1, 1))
}
