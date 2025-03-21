package u03

import org.junit.*
import org.junit.Assert.*
import u03.Lab03.*
import u03.Sequences.Sequence.Nil
import u03.Sequences.Sequence.Cons
import u03.Sequences.*
import u02.AlgebraicDataTypes.Person.*
import u02.AlgebraicDataTypes.Person
import Streams.*

class Task1Test:
  val sequence: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSkip() =
    assertEquals(Cons(30, Nil()), skip(sequence)(2))
    assertEquals(Nil(), skip(sequence)(3))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), skip(sequence)(0))
    assertEquals(Nil(), skip(Nil())(2))

  @Test def testZip() =
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), zip(sequence, l2))
    assertEquals(Nil(), zip(sequence, Nil()))
    assertEquals(Nil(), zip(Nil(), l2))
    assertEquals(Nil(), zip(Nil(), Nil()))

  @Test def testConcat() =
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), concat(sequence, l2))
    assertEquals(Cons(40, Cons(50, Nil())), concat(Nil(), l2))

  @Test def testReverse() =
    assertEquals(Cons(30, Cons(20, Cons(10, Nil()))), reverse(sequence))
    assertEquals(Nil(), reverse(Nil()))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(sequence)(v => Cons(v + 1, Nil())))
    assertEquals(Nil(), flatMap(Nil())(v => Cons(v, Nil())))

class Task2Test:
  val courseTeacher1 = "Analisi"
  val courseTeacher2 = "Probabilità"
  val courseTeacher3 = "Algebra"
  val courseTeacher4 = "Matematica discreta"
  val inputSequence = Cons(Teacher("Alessandro Rossi", courseTeacher1), 
    Cons(Teacher("Carlo Bianchi", courseTeacher2), 
      Cons(Teacher("Carlo Giallo", courseTeacher3), 
        Cons(Student("Pippo Rossi", 2015), 
          Cons(Teacher("Franco", courseTeacher4), Nil())))))

  @Test def testCoursesOfTeacherFunction() =
    assertEquals(Cons(courseTeacher1, Cons(courseTeacher2, Cons(courseTeacher3, Cons(courseTeacher4, Nil())))),
      coursesFunction(inputSequence))

  @Test def testFoldLeft() =
    assertEquals(-16, foldLeft(Cons(3, Cons(7, Cons(1, Cons(5, Nil())))))(0)(_ - _))
    assertEquals("Ciao sono Giovanni", foldLeft(Cons("Ciao", Cons(" sono ", Cons("Giovanni", Nil()))))("")(_ + _))

  @Test def testCountNumberOfCourses() =
    assertEquals(4, countNumberOfCourses(inputSequence))

class Task3Test:
  @Test def testTakeWhileStream() =
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))),
      Stream.toList(Stream.takeWhile(Stream.iterate(0)( _ + 1))(_ < 5)))

  @Test def testFillStream() =
    assertEquals(Cons("a", Cons("a", Cons("a", Nil()))), Stream.toList(Stream.fill(3)("a")))

  @Test def testFibonacciStream() =
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Nil()))))),
      Stream.toList(Stream.take(Stream.fibonacciStream)(5)))