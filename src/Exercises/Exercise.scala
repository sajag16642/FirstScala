package org.scala.practice
package Exercises

import java.util.Dictionary
import scala.annotation.tailrec

/**
 * @author Sajag Agrawal
 *
 *  Solution to problems given by SME(Rounak Saha)
 */
object Exercise extends App{

  /* Problem - A */
  def sum(n:Int): BigInt ={
    @tailrec
    def _sum(n:Int, ans:BigInt):BigInt={
      if(n==0) ans
      else _sum(n-1,ans+n)
    }
    _sum(n,0)
  }

  /* Problem - B */
  def factorial(n:Int):BigInt = {
    @tailrec
    def _factorial(n:Int, ans:BigInt): BigInt = {
      if(n<=1) ans
      else _factorial(n-1, ans*n)
    }
    _factorial(n,1)
  }

  /*Problem - C */
  def nthFibonacci(n:Int):Int = {
    @tailrec
    def _nthFibonacci(n:Int, a:Int, b:Int):Int = {
      if(n<=2) b
      else _nthFibonacci(n-1,b,a+b)
    }
    _nthFibonacci(n,1,1)
  }

  /*Problem - D */
  def product(list: List[Int]):BigInt = {
    @tailrec
    def _product(list: List[Int], ans:BigInt):BigInt = list match {
      case h::Nil => h*ans
      case h::tail => _product(tail,h*ans)
    }
    _product(list,1)
  }

  /*Problem - E */
  def isPalindrome(string: String):Boolean = {
    def _isPalindrome(string: String, a:Int, b:Int, ans: Boolean): Boolean = {
      if(a>=b) ans
      else _isPalindrome(string,a+1,b-1,string.charAt(a) == string.charAt(b)&&ans)
    }
    _isPalindrome(string,0,string.length-1, true)
  }

  /*Problem - F */
  def reverse(string: String):String = {
    @tailrec
    def _reverse(string: String, ans:String): String = string match {
      case "" => ""
      case _ => _reverse(string.tail,string.head+ans)
    }
    _reverse(string,"")
  }

  /*Problem - G */
  def flatten(array: List[Any]) : List[Any] = {
    array.flatMap(e => {
      e match {
        case t: Any => List(t)
        case a: List[Any] => flatten(a)
        case _ => List()
      }
    })
  }

  /* Problem - H*/
  def capitalize(array: Array[String]): Array[String] =  {
    @tailrec
    def _capitalize(array: Array[String], i:Int): Array[String] = {
      if(array.length>=i) array
      else {
        array(i).toUpperCase
        _capitalize(array,i+1)
      }
    }
    _capitalize(array,0)
  }

  /* Problem - I */
  def  ProblemI[T](array: Array[T], func: T=>Boolean): Boolean = {
    def _ProblemI(array: Array[T],func: T=> Boolean, i: Int): Boolean = {
      if(i>=array.length) false
      if(func(array(i))) true
      else _ProblemI(array,func,i+1)
    }
    _ProblemI(array,func,0)
  }

  /* Problem - J*/

  def ProblemJ(map: Map[Any, Any]): Int = {
    map.foldLeft(0){
      case (a,(key,value: Int))  if(value >0) => a+value
      case (a, (key, value:Map[Any,Any])) => a + ProblemJ(value)
    }
  }

  /*Problem - K*/
  def ProblemK(string: String): Seq[Seq[String]] = {
    def _ProblemK(i:Int): Seq[Seq[String]] = {
      if(i==string.length-1) Seq(Seq())
      else for{
        j<- i to string.length-1
        if isPalindrome(string.substring(i,j))
        partition<-_ProblemK(j+1)
      }yield string.substring(i,j+1)+:partition
    }
    _ProblemK(0)
  }

  /*Problem - L*/

  def ProblemL(x: BigInt): Boolean = {
    @tailrec
    def reverse(x:BigInt, ans: BigInt): BigInt = {
      if(x == 0) ans
      else reverse(x/10, ans*10+x%10)
    }
    x == reverse(x,0)
  }

  /*Problem - M*/
  def ProblemM(set: Set[Char], k: Int) = {
    def _ProblemM(set: Set[Char], string: String, k:Int, ans: Seq[String]) : Unit= {
      if(string.length == k) println(string)
      else{
          for (e<-set){
            _ProblemM(set, string+e, k, ans)
          }
      }
    }
    _ProblemM(set, "", k, Seq())
  }

  /*Problem - N*/
  def ProblemN(string: String): Int = {
    def _ProblemM(string: String, a: Int, b: Int, n:Int):Int = {
      if(n==1) 1
      else{
        val count = _ProblemM(string,a+1, b, n-1) + _ProblemM(string, a, b-1, n-1) - _ProblemM(string, a+1, b-1, n-2)
        if(string.charAt(a) == string.charAt(b))
          count+1
        else count
      }
    }
    _ProblemM(string, 0, string.length-1, string.length)
  }

  println(ProblemJ(Map(("a"->1), ("b"->2),("c"->Map("d"->3)))))
}
