package org.scala.practice
package Exercises

/**
 * @author sajag16642
 */
object ChatBot extends App{

  val chatbot: PartialFunction[String, String] = {
    case "hello" => "Hi, I am wanda!"
    case "goodbye" => "Please talk to me"
    case _ =>"I can't understand"
  }
  scala.io.Source.stdin.getLines().map(chatbot).foreach(println)
}
