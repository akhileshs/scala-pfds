package com.akhileshs.ll

sealed trait List[+A] {
  def head: A
  
  def tail: List[A]
  
  def isEmpty: Boolean
  
  def append[B >: A](x: B): List[B] = {
    if (isEmpty) List.make(x)
    else List.make(head, tail.append(x))
  }
  
  def prepend[B >: A](x: B): List[B] = List.make(x, this)

  def concat[B >: A](xs: List[B]): List[B] = {
    if (isEmpty) xs
    else tail.concat(xs).prepend(head)
  }

  def reverse: List[A] = {
    def loop(s: List[A], d: List[A]): List[A] = {
      if (s.isEmpty) d
      else loop(s.tail, d.prepend(s.head))
    }
    loop(this, List.empty)
  }


}

case object Nil extends List[Nothing] {
  def head: Nothing = throw new NoSuchElementException("Empty list.") // type safety fail
  def tail: Nothing = throw new NoSuchElementException("Empty list.")
  def isEmpty: Boolean = true
}

case class Cons[A](head: A, tail: List[A] = Nil) extends List[A] {
  def isEmpty: Boolean = false
}

object List {
  def empty[A]: List[A] = Nil

  def make[A](x: A, t: List[A] = Nil): List[A] = Cons(x, t)

  def apply[A](xs: A*): List[A] = {
    var y: List[A] = List.empty
    for (x <- xs.reverse) y = y.prepend(x)
    y
  }
}
