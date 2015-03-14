package com.akhileshs.Stack

class Stack[+A](s: List[A]) {
  def top: A = s.head

  def rest: Stack[A] = new Stack(s.tail)

  def isEmpty: Boolean = s.isEmpty

  def pop: (A, Stack[A]) = (top, rest)

  def push[B >: A](x: B): Stack[B] = new Stack(x :: s)
}

object Stack {
  def empty[A]: Stack[A] = new Stack(Nil)

  def apply[A](xs: A*): Stack[A] = {
    var y: Stack[A] = Stack.empty
    for (x <- xs) y = y.push(x)
    y
  }
}
