package com.akhileshs.binaryTree

sealed trait Tree[+A <% Ordered[A]] {     // view bounds are awesome!
  def value: A

  def left: Tree[A]       // left sub-tree

  def right: Tree[A]      // right sub-tree

  def size: Int           // no of nodes

  def isEmpty: Boolean

  def isValid: Boolean = {
    if (isEmpty) true
    else if (left.isEmpty && right.isEmpty) true
    else if (left.isEmpty) right.value >= value && right.isValid
    else if (right.isEmpty) left.value <= value && left.isValid
    else left.value <= value && right.value >= value && left.isValid && right.isValid
  }

  def add[B >: A <% Ordered[B]](x: B): Tree[B] = {
    if (isEmpty) Tree.make(x)
    else if (x < value) Tree.make(value, left.add(x), right)
    else if (x > value) Tree.make(value, left, right.add(x))
    else this
  }

  def remove[B >: A <% Ordered[B]](x: B): Tree[B] = {
    if (isEmpty) throw new Exception("Can't find value in this tree")
    else if (x < value) Tree.make(value, left.remove(x), right)
    else if (x > value) Tree.make(value, left, right.remove(x))
    else {
      if (left.isEmpty && right.isEmpty) Tree.empty
      else if (left.isEmpty) right
      else if (right.isEmpty) left
      else {
        val successor = right.min
        Tree.make(successor, left, right.remove(successor))
      }
    }
  }

  def min: A = {
    def loop(t: Tree[A], m: A): A = {
      if (t.isEmpty) m
      else loop(t.left, t.value)
    }

    if (isEmpty) throw new Exception("An empty tree.")
    else loop(left, value)
  }

  def max: A = {
    def loop(t: Tree[A], m: A): A = {
      if (t.isEmpty) m
      else loop(t.right, t.value)
    }
    if (isEmpty) throw new Exception("An empty tree.")
    else loop(right, value)
  }

  def height: Int = {
    if (isEmpty) 0
    else 1 + math.max(left.height, right.height)
  }

  def dfs: List[A] = {
    def loop(s: List[Tree[A]]): List[A] = {
      if (s.isEmpty) Nil
      else if (s.head.isEmpty) loop(s.tail)
      else s.head.value :: loop(s.head.right :: s.head.left :: s.tail)
    }
    loop(List(this))
  }

  def bfs: List[A] = {
    import scala.collection.immutable.Queue
    def loop(q: Queue[Tree[A]]): List[A] = {
      if (q.isEmpty) Nil
      else if (q.head.isEmpty) loop(q.tail)
      else q.head.value :: loop(q.tail :+ q.head.left :+ q.head.right)
    }

    loop(Queue(this))
  }
}

case object Leaf extends Tree[Nothing] {
  def value: Nothing = throw new Exception("An empty tree.")
  def left: Tree[Nothing] = throw new Exception("An empty tree.")
  def right: Tree[Nothing] = throw new Exception("An empty tree.")
  def size: Int = 0
  def isEmpty: Boolean = true
}

case class Branch[A <% Ordered[A]](value: A, left: Tree[A], right: Tree[A], size: Int) extends Tree[A] {
  def isEmpty: Boolean = false
}

object Tree {
  def empty[A]: Tree[A] = Leaf

  def make[A <% Ordered[A]](x: A, l: Tree[A] = Leaf, r: Tree[A] = Leaf): Tree[A] = 
    Branch(x, l, r, l.size + r.size + 1)

  def apply[A <% Ordered[A]](xs: A*): Tree[A] = {
    var r: Tree[A] = Tree.empty
    for (x <- xs) r.add(x) 
    r
  }
}

