package org.scala.practice
package Exercises

/**
 * @author sajag16642
 */
abstract class Maybe[+T] {
  def map[A](f: T => A): Maybe[A]

  def flatmap[A](f: T => Maybe[A]): Maybe[A]

  def filter(p: T => Boolean): Maybe[T]
}

case object MaybeNot extends Maybe[Nothing] {
  override def map[A](f: Nothing => A): Maybe[A] = MaybeNot

  override def flatmap[A](f: Nothing => Maybe[A]): Maybe[A] = MaybeNot

  override def filter(p: Nothing => Boolean): Maybe[Nothing] = MaybeNot
}

case class Just[+T](value: T) extends Maybe[T] {
  override def map[A](f: T => A): Maybe[A] = Just(f(value))

  override def flatmap[A](f: T => Maybe[A]): Maybe[A] = f(value)

  override def filter(p: T => Boolean): Maybe[T] = {
    if(p(value)) this
    else MaybeNot
  }
}

object Test extends App{
  val just = Just(3)
  println(just)
  println(just.map(_*3))
  println(just.filter(_%2==0))
  println(just.flatmap(Just(_)))
}