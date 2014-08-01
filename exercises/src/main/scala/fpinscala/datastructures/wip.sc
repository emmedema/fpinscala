import scala.annotation.tailrec

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

val t = Branch("A", Branch("B",Leaf("D"),Leaf("E")), Branch("C",Leaf("F"),Leaf("G")))

def printVal[A](v: A) = println(s"Val: $v")

def inOrder[A](t: Tree[A]) : Unit = t match {
  case Leaf(v) => printVal(v)
  case Branch(v,l,r) => {
    inOrder(l)
    printVal(v)
    inOrder(r)
  }
}

def preOrder[A](r: Tree[A]) : Unit = {
  @tailrec
  def go(q: List[Tree[A]]) : List[Tree[A]] = q match {
    case Nil => Nil
    case x :: xs => x match {
      case Leaf(v) => printVal(v); go(xs)
      case Branch(v,l,r) => printVal(v); go(l :: r :: xs)
    }
  }
  r match {
    case Leaf(v) => printVal(v)
    case Branch(_,_,_) => go(List(r))
  }
}

def postOrder[A](t: Tree[A]) : Unit = t match {
  case Leaf(v) => printVal(v)
  case Branch(v,l,r) => {
    postOrder(l)
    postOrder(r)
    printVal(v)
  }
}

def dfs[A](t: Tree[A]) : Unit = ???

def bfs[A](r: Tree[A]) : Unit = {
  @tailrec
  def go(q: List[Tree[A]]) : List[Tree[A]] = {println(q); q match {
    case Nil => Nil
    case x :: xs => x match {
      case Leaf(v) => printVal(v); go(xs)
      case Branch(v,l,r) => printVal(v); go(l :: r :: xs)
    }
  }}
  r match {
    case Leaf(v) => printVal(v)
    case Branch(_,_,_) => go(List(r))
  }

}

//inOrder(t)
preOrder(t)
//postOrder(t)
bfs(t)
