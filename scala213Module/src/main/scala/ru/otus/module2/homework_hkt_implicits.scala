package ru.otus.module2

import ru.otus.module2.higher_kinded_types.Bindable

import scala.language.implicitConversions

object homework_hkt_implicits extends App {

  private def tupleF[F[_], A, B](fa: Bindable[F, A], fb: Bindable[F, B]) =
      fa.flatMap(a => fb.map(b => (a, b)))

  // test for Option:
  import higher_kinded_types.optBindable
  val optA= Some(1)
  val optB = Some(2)
  println(tupleF(optA, optB)) // "Some((1,2))"

  // test for List:
  import higher_kinded_types.listBindable
  val list1 = List(1, 2, 3)
  val list2 = List(4, 5, 6)
  println(tupleF(list1, list2)) // "List((1,4), (1,5), (1,6), (2,4), (2,5), (2,6), (3,4), (3,5), (3,6))"

  // Either:
  //noinspection ScalaWeakerAccess
  type RightEither[A] = Either[Any, A]

  implicit def eitherBindable[A](either: RightEither[A]): Bindable[RightEither, A] = new Bindable[RightEither, A] {
    override def map[B](f: A => B): RightEither[B] = either.map(f)
    override def flatMap[B](f: A => RightEither[B]): RightEither[B] = either.flatMap(f)
  }

  // test for Either:
  private val either1 = Right[String, Int](7)
  private val either2 = Right[String, Int](8)
  println(tupleF(either1, either2)) // "Right((7,8))"
}
