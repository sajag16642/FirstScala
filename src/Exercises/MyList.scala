package org.scala.practice
package Exercises

/**
 * @author sajag16642
 */
abstract class MyList {
  def head: Int

  def tail: MyList

  def isEmpty: Boolean

  def add(e: Int): MyList
}

object Empty extends MyList {
  override def head: Int = throw new NoSuchElementException

  override def tail: MyList = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def add(e: Int): MyList = new Cons(e, Empty)
}


class Cons(head: Int, tail: MyList) extends MyList {
  override def head: Int = head

  override def tail: MyList = tail

  override def isEmpty: Boolean = false

  override def add(e: Int): MyList = new Cons(e, this)
}