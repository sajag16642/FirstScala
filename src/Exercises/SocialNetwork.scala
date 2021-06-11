package org.scala.practice
package Exercises

/**
 * @author sajag16642
 */
object SocialNetwork extends App {
  def add(network: Map[String, Set[String]], person: String): Map[String, Set[String]] = {
    network + (person -> Set())
  }

  def remove(network: Map[String,Set[String]], person: String): Map[String,Set[String]]={
      def unfriendEach(remFriends: Set[String], remNetwork: Map[String,Set[String]]): Map[String,Set[String]] = {
        if(remFriends.isEmpty) remNetwork
        else unfriendEach(remFriends.tail,unfriend(remNetwork,person,remFriends.head))
      }
    unfriendEach(network(person),network) - person
  }

  def friend(network: Map[String, Set[String]], A: String, B: String): Map[String, Set[String]] = {
    val friendsA = network(A)
    val friendsB = network(B)
    network + (A -> (friendsA + B)) + (B -> (friendsB + A))
  }

  def unfriend(network: Map[String, Set[String]], A: String, B: String): Map[String, Set[String]] = {
    val friendsA = network(A)
    val friendsB = network(B)
    network + (A -> (friendsA - B)) + (B -> (friendsB - A))
  }

  val network: Map[String, Set[String]] = add(add(Map(),"Bob"), "Alice")
  println(network)
  println(friend(network,"Bob","Alice"))
  println(unfriend(friend(network,"Bob","Alice"),"Bob","Alice"))
  println(remove(friend(network,"Bob","Alice"),"Bob"))

}

