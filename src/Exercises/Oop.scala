package org.scala.practice
package Exercises

/**
 * @author sajag16642
 */
object Oop extends App {
  val author = new Writer("Charles", "Dickens", 1812)
  val Novel = new Novel("Great Expectations", 1861, author)
  println(author.fullName())
  println(Novel.authorAge())
  println(Novel.isWrittenBy(author))
  println(Novel.copy(1913))
}

class Writer(firstName: String, surName: String, val year: Int) {
  def fullName(): String = firstName + " " + surName
}

class Novel(val name: String, val year: Int, val author: Writer) {
  def authorAge(): Int = this.year - author.year

  def isWrittenBy(author: Writer): Boolean = this.author == author

  def copy(year: Int): Novel = new Novel(this.name, year, this.author)
}
