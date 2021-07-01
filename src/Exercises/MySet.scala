package org.scala.practice
package Exercises

/**
 * @author sajag16642
 */
trait MySet[A] extends (A=>Boolean){

  def apply(elem: A): Boolean =
    contains(elem)

  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]

  def map[B](f: A=> B): MySet[B]
  def flatMap[B](f:A=>MySet[B]): MySet[B]
  def filter(predicate: A=>Boolean): MySet[A]
  def foreach(f: A=> Unit): Unit

  def -(elem: A): MySet[A]
  def --(anotherSet: MySet[A]): MySet[A]
  def &(anotherSet: MySet[A]): MySet[A]

  def unary_! :MySet[A]
}

class EmptySet[A] extends MySet[A]{
  override def contains(elem: A): Boolean = false

  override def +(elem: A): MySet[A] = new NonEmptySet[A](elem, this)

  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  override def map[B](f: A => B): MySet[B] = new EmptySet[B]

  override def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]

  override def filter(predicate: A => Boolean): MySet[A] = this

  override def foreach(f: A => Unit): Unit = ()

  override def apply(v1: A): Boolean = ???

  override def -(elem: A): MySet[A] = this

  override def --(anotherSet: MySet[A]): MySet[A] = this

  override def &(anotherSet: MySet[A]): MySet[A] = this

  override def unary_! : MySet[A] = new PropertyBasedSet[A](_ =>true)
}

class PropertyBasedSet[A](property: A=>Boolean) extends MySet[A] {
  override def contains(elem: A): Boolean = property(elem)

  override def +(elem: A): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || x==elem)

  override def ++(anotherSet: MySet[A]): MySet[A] =
    new PropertyBasedSet[A](x=>property(x)||anotherSet(x))

  override def map[B](f: A => B): MySet[B] = throw new IllegalArgumentException

  override def flatMap[B](f: A => MySet[B]): MySet[B] = throw new IllegalArgumentException

  override def filter(predicate: A => Boolean): MySet[A] =
    new PropertyBasedSet[A](x=>property(x)&&predicate(x))

  override def foreach(f: A => Unit): Unit = throw new IllegalArgumentException

  override def -(elem: A): MySet[A] = filter(x=> x!=elem)

  override def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)

  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)


  override def unary_! : MySet[A] =
    new PropertyBasedSet[A](x=> !property(x))
}
class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  override def contains(elem: A): Boolean =
    head == elem || tail.contains(elem)

  override def +(elem: A): MySet[A] = {
    if(this.contains(elem)) this
    else new NonEmptySet[A](elem, this)
  }

  override def ++(anotherSet: MySet[A]): MySet[A] =
    tail ++ anotherSet+head

  override def map[B](f: A => B): MySet[B] =
    tail.map(f)+f(head)

  override def flatMap[B](f: A => MySet[B]): MySet[B] =
    tail.flatMap(f)++f(head)

  override def filter(predicate: A => Boolean): MySet[A] = {
    if(predicate(head)) tail.filter(predicate)+head
    else tail.filter(predicate)
  }

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  override def -(elem: A): MySet[A] =
    if(head == elem) tail
    else (tail - elem) + head

  override def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)

  override def &(anotherSet: MySet[A]): MySet[A] = filter(x=>anotherSet(x))

  override def unary_! : MySet[A] = new PropertyBasedSet[A](x=> !this.contains(x))
}

object MySet {
  def apply[A](values: A*): MySet[A] = {
    def buildSet(seq: Seq[A],acc: MySet[A]): MySet[A] = {
      if(seq.isEmpty) acc
      else buildSet(seq.tail, acc+seq.head)
    }
    buildSet(values.toSeq, new EmptySet[A])
  }
}

object Test3 extends App{
  val mySet = MySet(1,2,3,4)
  mySet.foreach(println)
}