package org.scala.practice
package Exercises

/**
 * @author sajag16642
 */
abstract class MyList[+A] {
  def head: A

  def tail: MyList[A]

  def isEmpty: Boolean

  def add[B >: A](e: B): MyList[B]

  def ++[B >: A](list: MyList[B]): MyList[B]

  def map[B](transformer: MyTransformer[A, B]): MyList[B]

  def filter(predicate: MyPredicate[A]): MyList[A]

  def flatmap[B](transformer: MyTransformer[A, MyList[B]]): MyList[B]

  def printElements: String

  override def toString: String = "[" + printElements + "]"
}

object Empty extends MyList[Nothing] {
  def head: Nothing = throw new NoSuchElementException

  def tail: MyList[Nothing] = throw new NoSuchElementException

  def isEmpty: Boolean = true

  def add[B >: Nothing](e: B): MyList[B] = new Cons(e, Empty)

  def ++[B >: Nothing](list: MyList[B]) = list

  def map[B](transformer: MyTransformer[Nothing, B]): MyList[B] = Empty

  def filter(predicate: MyPredicate[Nothing]): MyList[Nothing] = Empty

  def flatmap[B](transformer: MyTransformer[Nothing, MyList[B]]): MyList[B] = Empty

  def printElements: String = ""
}


class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {
  def head: A = h

  def tail: MyList[A] = t

  def isEmpty: Boolean = false

  def add[B >: A](e: B): MyList[B] = new Cons(e, this)

  def ++[B >: A](list: MyList[B]) = new Cons(h, t ++ list)

  def map[B](transformer: MyTransformer[A, B]): MyList[B] =
    new Cons(transformer.transform(h), t.map(transformer))

  def filter(predicate: MyPredicate[A]): MyList[A] =
    if (predicate.test(h)) new Cons(h, t.filter(predicate))
    else t.filter(predicate)

  def flatmap[B](transformer: MyTransformer[A, MyList[B]]): MyList[B] =
    transformer.transform(h) ++ t.flatmap(transformer)

  def printElements: String = {
    if (t.isEmpty) "" + h
    else h + " " + t.printElements
  }
}

trait MyTransformer[-A, B] {
  def transform(e: A): B
}

trait MyPredicate[-T] {
  def test(e: T): Boolean
}

object Test extends App {
  val listOfIntegers: MyList[Int] = new Cons(1, new Cons(2, new Cons(3, Empty)))
  val listOfStrings: MyList[String] = new Cons("Hello", new Cons("World", Empty))
  println(listOfStrings)
  println(listOfIntegers)

  println(listOfIntegers.filter(new MyPredicate[Int] {
    override def test(e: Int): Boolean = e%2==0
  }))

  println(listOfIntegers.map(new MyTransformer[Int,Int] {
    override def transform(e: Int): Int = e*2
  }))

  println(listOfIntegers.flatmap(new MyTransformer[Int, MyList[Int]] {
    override def transform(e: Int): MyList[Int] = new Cons[Int](e,new Cons[Int](e+1, Empty))
  }))
}