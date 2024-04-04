import scala.annotation.tailrec

//tail recursion
def fib(n: Int): Int = {
  @tailrec
  def loop(n: Int, curr: Int, next: Int): Int =
    if (n <= 1) curr
    else loop(n - 1, next, curr + next)

  loop(n, 0, 1)
}

fib(7)


def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  @tailrec
  def isSorted(currIndex: Int, prevIndex: Int): Boolean = {
    if (currIndex >= as.length) true
    else if (!ordered(as(currIndex), as(prevIndex))) false
    else isSorted(currIndex + 1, currIndex)
  }

  isSorted(1, 0)
}

isSorted(Array(4, 3 ,2), (x: Int, y: Int) => x < y)
isSorted(Array("A", "B", "C"), (x: String, y: String) => x > y)


def curry[A,B,C](f: (A, B) => C): A => (B => C) =
  (a: A) => (b: B) => f(a, b)

curry((x: Int, y: Int) => x + y)(1)(2)
//curry((x: Int, y: String) => x + y)(1)("Me")

def uncurry[A,B,C](f: A => B => C): (A, B) => C =
  (a: A, b: B) => f(a)(b)

uncurry((x: Int) => (y: Int) => x + y)(1, 5)

def compose[A,B,C](f: B => C, g: A => B): A => C =
  (a: A) => f(g(a))

compose((x: Int) => x * 3, (y: Int) => y + 2)(5)