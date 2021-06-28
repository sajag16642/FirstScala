package org.scala.practice
package Exercises

import sun.rmi.transport.Target

import scala.annotation.tailrec

/**
 * @author sajag16642
 */
object SocialNetwork extends App {
  def add(network: Map[String, Set[String]], person: String): Map[String, Set[String]] = {
    network + (person -> Set())
  }

  def remove(network: Map[String, Set[String]], person: String): Map[String, Set[String]] = {
    @tailrec
    def unfriendEach(remFriends: Set[String], remNetwork: Map[String, Set[String]]): Map[String, Set[String]] = {
      if (remFriends.isEmpty) remNetwork
      else unfriendEach(remFriends.tail, unfriend(remNetwork, person, remFriends.head))
    }

    unfriendEach(network(person), network) - person
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

  def nFriend(network: Map[String, Set[String]], person:String):Int = {
    if(!network.contains(person)) 0
    else network(person).size
  }

  def mostFriends(network: Map[String, Set[String]]): String = {
    network.maxBy(pair => pair._2.size)._1
  }

  def nPeopleWithNoFriends(network: Map[String, Set[String]]): Int = {
    network.count(pair=>pair._2.isEmpty)
  }

  def socialConnection(network: Map[String, Set[String]], a: String, b: String): Boolean = {
    def bfs(target: String, consideredPeople: Set[String], discoverablePeople: Set[String]): Boolean = {
      if(discoverablePeople.isEmpty) false
      else{
        val person = discoverablePeople.head
        if(person == target) true
        else if (consideredPeople.contains(person)) bfs(target, consideredPeople, discoverablePeople.tail)
        else bfs(target, consideredPeople+person, discoverablePeople.tail ++ network(person))
      }
    }
    bfs(b,Set(), network(a)+a)
  }

  val network: Map[String, Set[String]] = add(add(Map(), "Bob"), "Alice")
  println(network)
  println(friend(network, "Bob", "Alice"))
  println(unfriend(friend(network, "Bob", "Alice"), "Bob", "Alice"))
  println(remove(friend(network, "Bob", "Alice"), "Bob"))

}

