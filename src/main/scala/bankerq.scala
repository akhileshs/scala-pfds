package com.akhileshs.bq

class Queue[+A](in: List[A] = Nil, out: List[A] = Nil) {
  def isEmpty: Boolean = (in, out) match {
    case (Nil, Nil) => true
    case (_, _)     => false
  }

  def dequeue: (A, Queue[A]) = out match {
    case hd :: tail => (hd, new Queue(in, tail))
    case Nil        => in.reverse match {
      case hd :: tail   => (hd, new Queue(Nil, tail))
      case Nil          => throw new NoSuchElementException("Empty Q")
    }
  }

  def enqueue[B >: A](x: B): Queue[B] = new Queue(x :: in, out)

  def front: A = dequeue match {
    case (a, _) => a
  }

  def rear: Queue[A] = dequeue match {
    case (_, q) => q
  }
}

object Queue {
  def empty[A]: Queue[A] = new Queue()

  def apply[A](xs: A*) = 
    xs.foldLeft(Queue.empty[A]) {
      case (acc, x) => acc.enqueue(x)
    }
}
