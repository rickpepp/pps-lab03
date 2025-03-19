package u03

import u03.Sequences.Sequence
import Sequence.*
import u02.AlgebraicDataTypes.Person
import u02.AlgebraicDataTypes.Person.Teacher

object Lab03:

  // Task 1
  def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match
    case Cons(h, t) if n > 0 => skip(t)(n - 1)
    case _ => s

  def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
    case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
    case _ => Nil()

  def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = (s1, s2) match
    case (Cons(h, t), _) => Cons(h, concat(t, s2))
    case _ => s2

  def reverse[A](s: Sequence[A]): Sequence[A] =
    def reverseWithAccumulator(s: Sequence[A], acc: Sequence[A]): Sequence[A] = s match
      case Cons(h, t) => reverseWithAccumulator(t, Cons(h, acc))
      case _ => acc
    reverseWithAccumulator(s, Nil())

  def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
    case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
    case _ => Nil()

  // Task 2
  def coursesFunction(s: Sequence[Person]): Sequence[String] = flatMap(s)(p => p match
    case Person.Teacher(_, c) => Cons(c, Nil())
    case _ => Nil()
  )

  def foldLeft[A](s: Sequence[A])(default: A)(binFunc: (A, A) => A): A = s match
    case Cons(h1, t) => foldLeft(t)(binFunc(default, h1))(binFunc)
    case _ => default

  def countNumberOfCourses(s: Sequence[Person]): Int =
    val isTeacher: Person => Boolean = p => p match
      case Teacher(_, _) => true
      case _ => false
    foldLeft(map(filter(s)(isTeacher))(_ => 1))(0)(_ + _)

  // Task 3 (Dentro Streams.scala)
  def takeWhile[A](stream: Stream[A])(predicate: A => Boolean): Stream[A] = stream match
    case Cons(head, tail) if predicate(head()) => cons(head(), takeWhile(tail())(predicate))
    case _ => Empty()

  def fill[A](n: Int)(value: A): Stream[A] = n match
    case n if n > 0 => cons(value, fill(n - 1)(value))
    case _ => Empty()

  def fibonacciStream: Stream[Int] =
    def fibonacciCalc(a: Int)(b: Int): Stream[Int] =
      cons(a + b, fibonacciCalc(b)(a + b))
    cons(0, cons(1, fibonacciCalc(0)(1)))


