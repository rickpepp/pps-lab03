package u03

import org.junit.*
import org.junit.Assert.*
import u03.Lab03.*
import u03.Sequences.Sequence.Nil
import u03.Sequences.Sequence.Cons
import u03.Sequences.*
import u02.AlgebraicDataTypes.Person.*
import u02.AlgebraicDataTypes.Person

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
  @Test def testCoursesOfTeacherFunction() =
    val teacher1 = Teacher("Alessandro Rossi", "Algebra")
    val teacher2 = Teacher("Alessandro Rossi", "Analisi")
    val teacher3 = Teacher("Alessandro Rossi", "Matematica discreta")
    val teacher4 = Teacher("Carlo Bianchi", "Probabilità")
    val student = Student("Pippo Rossi", 2015)
    val inputSequence = Cons(teacher1, Cons(teacher2, Cons(teacher3, Cons(teacher4, Cons(student, Nil())))))

    assertEquals(Cons("Algebra", Cons("Analisi", Cons("Matematica discreta", Cons("Probabilità", Nil())))),
      coursesFunction(inputSequence))

  @Test def testFoldLeft() =
    assertEquals(-16, foldLeft(Cons(3, Cons(7, Cons(1, Cons(5, Nil())))))(0)(_ - _))
    assertEquals("Ciao sono Giovanni", foldLeft(Cons("Ciao", Cons(" sono ", Cons("Giovanni", Nil()))))("")(_ + _))