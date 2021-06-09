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
  def isPrime(n: Int): Boolean = {
    @tailrec
    def isPrimeUntil(t: Int, acc: Boolean): Boolean =
      if(t<=1) acc
      else isPrimeUntil(t-1,acc && n%t!=0)

    isPrimeUntil(n/2,true)
  }

  val concatenate:(String,String)=>String = new Function2[String,String,String] {
    override def apply(v1: String, v2: String): String = v1+v2
  }



//  println(sum(2, 3))
//  println(factorial(5000))
//  println(repeatedFunction("Hello", 3, "Hello"))
//  println(fibonacci(3, 1, 1))
  println(isPrime(2003))
  println(isPrime(629))
  println(concatenate("Hello ","Scala"))
}
