package u03

import u03.Sequences.Sequence
import Sequence.*
import u02.AlgebraicDataTypes.Person

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

