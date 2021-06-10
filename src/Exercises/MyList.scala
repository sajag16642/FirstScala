package org.scala.practice
package Exercises

import sun.tools.jstat.Operator

/**
 * @author sajag16642
 */
abstract class MyList[+A] {
  def head: A

  def tail: MyList[A]

  def isEmpty: Boolean

  def add[B >: A](e: B): MyList[B]

  def ++[B >: A](list: MyList[B]): MyList[B]

  def map[B](transformer: (A) => B): MyList[B]

  def filter(predicate: (A) => Boolean): MyList[A]

  def flatmap[B](transformer: (A) => MyList[B]): MyList[B]

  def foreach(f: A => Unit): Unit

  def sort(compare: (A, A) => Int): MyList[A]

  def zipWith[B,C](list: MyList[B], zip: (A,B)=>C): MyList[C]

  def fold[B](start:B)(operator: (B,A) =>B): B

  def printElements: String

  override def toString: String = "[" + printElements + "]"
}

object Empty extends MyList[Nothing] {
  def head: Nothing = throw new NoSuchElementException

  def tail: MyList[Nothing] = throw new NoSuchElementException

  def isEmpty: Boolean = true

  def add[B >: Nothing](e: B): MyList[B] = new Cons(e, Empty)

  def ++[B >: Nothing](list: MyList[B]): MyList[B] = list

  def map[B](transformer: (Nothing) => B): MyList[B] = Empty

  def filter(predicate: (Nothing) => Boolean): MyList[Nothing] = Empty

  def flatmap[B](transformer: (Nothing) => MyList[B]): MyList[B] = Empty

  def printElements: String = ""

  override def foreach(f: Nothing => Unit): Unit = ()

  override def sort(compare: (Nothing, Nothing) => Int): MyList[Nothing] = Empty

  override def zipWith[B, C](list: MyList[B], zip: (Nothing, B) => C): MyList[C] = {
    if(!list.isEmpty) throw new RuntimeException("List are not of same size")
    else Empty
  }

  override def fold[B](start: B)(operator: (B, Nothing) => B): B = start
}


class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {
  def head: A = h

  def tail: MyList[A] = t

  def isEmpty: Boolean = false

  def add[B >: A](e: B): MyList[B] = new Cons(e, this)

  def ++[B >: A](list: MyList[B]) = new Cons(h, t ++ list)

  def map[B](transformer: (A) => B): MyList[B] =
    new Cons(transformer(h), t.map(transformer))

  def filter(predicate: (A) => Boolean): MyList[A] =
    if (predicate(h)) new Cons(h, t.filter(predicate))
    else t.filter(predicate)

  def flatmap[B](transformer: (A) => MyList[B]): MyList[B] =
    transformer(h) ++ t.flatmap(transformer)

  override def foreach(f: A => Unit): Unit = {
    f(h)
    t.foreach(f)
  }

  def printElements: String = {
    if (t.isEmpty) "" + h
    else h + " " + t.printElements
  }

  override def sort(compare: (A, A) => Int): MyList[A] = {
    def insert(a: A, list: MyList[A]):MyList[A] = {
      if(list.isEmpty) new Cons(a, Empty)
      else if(compare(a,list.head) < 0) new Cons(a,list)
      else new Cons(list.head,insert(a,list.tail))
    }
    val sorted = t.sort(compare)
    insert(h,sorted)
  }

  override def zipWith[B, C](list: MyList[B], zip: (A, B) => C): MyList[C] = {
    if(list.isEmpty) throw new RuntimeException("list are not of same length")
    else new Cons(zip(head,list.head), tail.zipWith(list.tail,zip))
  }

  override def fold[B](start: B)(operator: (B, A) => B): B = {
    tail.fold(operator(start,head))(operator)
  }
}

//trait MyTransformer[-A, B] {
//  def transform(e: A): B
//}
//
//trait MyPredicate[-T] {
//  def test(e: T): Boolean
//}

object Test2 extends App {
  val listOfIntegers: MyList[Int] = new Cons(1, new Cons(2, new Cons(3, Empty)))
  val listOfStrings: MyList[String] = new Cons("Hello", new Cons("World", new Cons[String]("Scala",Empty)))
  println(listOfStrings)
  println(listOfIntegers)

  println(listOfIntegers.filter((e: Int) => e % 2 == 0))

  println(listOfIntegers.map((e: Int) => e * 2))

  println(listOfIntegers.flatmap((e: Int) => new Cons(e, new Cons[Int](e + 1, Empty))))

  println(listOfIntegers.sort((x,y)=>y-x))

  println(listOfIntegers.zipWith(listOfStrings,(x:Int, y:String)=>x+"-"+y))

  println(listOfIntegers.fold(0)(_+_))
}