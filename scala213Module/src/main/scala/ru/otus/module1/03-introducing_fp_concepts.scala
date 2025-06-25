package ru.otus.module1

import ru.otus.module1.opt.{Animal, Cat}

import scala.annotation.tailrec
import scala.language.postfixOps


/**
 * referential transparency
 */


// recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n) {
      _n *= i
      i += 1
    }
    _n
  }


  def factRec(n: Int): Int = if (n <= 0) 1 else n * factRec(n - 1)


  def factTailRec(n: Int, accum: Int): Int = {
    @tailrec
    def loop(n: Int, accum: Int): Int =
      if (n <= 0) accum
      else loop(n - 1, n * accum)

    loop(n, 1)
  }


  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   */


}


object hof {

  def dumb(string: String): Unit = {
    Thread.sleep(1000)
    println(string)
  }

  // обертки

  def logRunningTime[A, B](f: A => B): A => B = a => {
    val start = System.currentTimeMillis()
    val result = f(a)
    val end = System.currentTimeMillis()
    println(s"Running time: ${end - start}")
    result
  }



  // изменение поведения ф-ции

  def isOdd(i: Int): Boolean = i % 2 > 0

  def not[A](f: A => Boolean): A => Boolean = a => !f(a)

  val isEven: Int => Boolean = not(isOdd)

  isOdd(5) // true
  isEven(5) // false


  // изменение самой функции

  def sum(x: Int, y: Int): Int = x + y

  def partial[A, B, C](a: A)(f: (A, B) => C): B => C =
    f.curried(a)


}


/**
 * Реализуем тип Option
 */


object opt extends App {

  trait Animal

  case class Cat() extends Animal

  case class Dog() extends Animal

  /**
   *
   * Реализовать структуру данных Option, который будет указывать на присутствие либо отсутствие результата
   */

  // Invariance
  // + Covariance Если А является подтипом В, то Option[A] является подтипом Option[B]
  // - Contravariance Если А является подтипом В, то Option[A] является супер типом Option[B]

  // Function1[-R, +T]
  //  val f1: String => Unit = ???
  //  val f2: Any => Unit = ???

  def foo(f: String => Unit) = f("Hello")

  //  foo(f2)
  //  foo(f1)

  sealed trait Option[+T] {
    def isEmpty: Boolean = isInstanceOf[None.type]

    def map[B](f: T => B): Option[B] = flatMap(v => Option(f(v)))

    def flatMap[B](f: T => Option[B]): Option[B]

    /**
     * Реализовать метод filter, который будет возвращать не пустой Option
     * в случае, если исходный не пуст и предикат от значения = true
     */
    def filter(f: T => Boolean): Option[T]

    /**
     * Реализовать метод printIfAny, который будет печатать значение, если оно есть
     */
    def printIfAny(): Unit

    /**
     * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
     */
    def zip[U](that: Option[U]): Option[(T, U)]
  }

  case class Some[T](v: T) extends Option[T] {
    override def flatMap[B](f: T => Option[B]): Option[B] = f(v)

    override def filter(f: T => Boolean): Option[T] = if (f(v)) this else None

    override def printIfAny(): Unit = println(v)

    override def zip[U](that: Option[U]): Option[(T, U)] = if (that.isInstanceOf[Some[U]]) that.map((v, _)) else None
  }

  case object None extends Option[Nothing] {
    override def flatMap[B](f: Nothing => Option[B]): Option[B] = this

    override def filter(f: Nothing => Boolean): Option[Nothing] = this

    override def printIfAny(): Unit = println("None")

    override def zip[U](that: Option[U]): Option[(Nothing, U)] = this
  }

  object Option {
    def apply[T](v: T): Option[T] =
      if (v == null) None else Some(v)
  }

  val opt1: Option[Int] = Option(5)
  opt1.printIfAny() // 5
  opt1.map(i => Option(i + 1)).printIfAny() // Some(6)
  opt1.flatMap(i => Option(i + 1)).printIfAny() // 6
  opt1.filter(_ > 5).printIfAny() // None
  Option(9).zip(opt1).printIfAny() // (9, 5)
}

object list extends App {

  def treat(a: Option[Animal]) = ???

  /**
   * Реализовать одно-связанный иммутабельный список List.
   * Список имеет два случая:
   * Nil - пустой список
   * Cons - непустой, содержит первый элемент (голову) и хвост (оставшийся список)
   */
  sealed trait List[+T] {

    // prepend
    def ::[TT >: T](elem: TT): List[TT] = new::(elem, this)

    def :+[TT >: T](elem: TT): List[TT] //= new ::(elem, Nil)

    def mkString: String

    /**
     * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
     */
    def reverse: List[T]

    /**
     *
     * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
     */
    def map[U](f: T => U): List[U]

    def flatMap[U](f: T => List[U]): List[U]

    /**
     *
     * Реализовать метод filter для списка который будет фильтровать список по некому условию
     */
    def filter(f: T => Boolean): List[T]

    def :::[TT >: T](that: List[TT]): List[TT]
  }

  case class ::[T](head: T, tail: List[T]) extends List[T] {
    override def mkString = s"${head.toString}, ${tail.mkString}"
    override def :+[TT >: T](elem: TT) = head :: (tail :+ elem)
    override def reverse = tail.reverse :+ head
    override def map[U](f: T => U) = f(head) :: tail.map(f)
    override def :::[TT >: T](that: List[TT]) = head :: that ::: tail
    override def flatMap[U](f: T => List[U]) = tail.flatMap(f) ::: f(head)
    override def filter(f: T => Boolean) = if (f(head)) head :: tail.filter(f) else tail.filter(f)
  }

  case object Nil extends List[Nothing] {
    override def mkString = "Nil"
    override def :+[T](elem: T) = Nil.::(elem)
    override def reverse: List[Nothing] = this
    override def map[U](f: Nothing => U): List[U] = this
    override def :::[T >: Nothing](that: List[T]) = that
    override def flatMap[U](f: Nothing => List[U]): List[U] = this
    override def filter(f: Nothing => Boolean): List[Nothing] = this
  }

  object List {
    /**
     * Конструктор, позволяющий создать список из N - го числа аргументов
     * Для этого можно воспользоваться *
     *
     * Например, вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
     * def printArgs(args: Int*) = args.foreach(println(_))
     */
    def apply[A](v: A*): List[A] =
      if (v.isEmpty) Nil
      else ::(v.head, apply(v.tail: _*))
  }

  /**
   * Написать функцию shoutString которая будет принимать список String и возвращать список,
   * где к каждому элементу будет добавлен префикс в виде '!'
   */
  def shoutString(strings: List[String]) = strings.map(string => s"$string!")

  /**
   * Написать функцию incList которая будет принимать список Int и возвращать список,
   * где каждый элемент будет увеличен на 1
   */
  def incList(list: List[Int]) = list.map(_ + 1)


  // tests:

  val l1 = List(1, 2, 3)
  println(s"mkString: ${l1.mkString}") // mkString: 1, 2, 3, Nil
  println(s"reverse: ${l1.reverse.mkString}") // reverse: 3, 2, 1, Nil
  println(s"map: ${l1.map(_ + 10).mkString}") // map: 11, 12, 13, Nil
  println(s"::: : ${l1.:::(List(5, 6, 7)).mkString}") // ::: : 1, 2, 3, 5, 6, 7, Nil
  println(s"flatMap: ${l1.flatMap(elem => List(elem - 1, elem + 1)).mkString}") // flatMap: 0, 2, 1, 3, 2, 4, Nil
  println(s"filter: ${l1.filter(_ % 2 == 0).mkString}") // filter: 2, Nil

  val l2: List[Cat] = List(Cat())
  println(l2.mkString) // Cat(), Nil

  println(s"shoutString: ${shoutString(List("мама мыла раму".split(" "): _*)).mkString}"); // shoutString: мама!, мыла!, раму!, Nil

  println(s"incList: ${incList(l1).mkString}") // incList: 2, 3, 4, Nil
}
