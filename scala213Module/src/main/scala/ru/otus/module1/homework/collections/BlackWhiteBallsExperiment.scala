package ru.otus.module1.homework.collections

import scala.util.Random

/**
 * Домашнее задание №2 "Коллекции данных" (после занятия "Библиотека коллекций")
 *
 * Цель:
 * Научиться практическому использованию наиболее употребляемых методов работы с
 * данными и немного вспомнить математику (теорию вероятности), в виду того, что
 * скалу очень часто применяют для анализа данных, то это будет полезно.
 *
 * Описание/Пошаговая инструкция выполнения домашнего задания:
 * Необходимо будет "проверить" формулу вероятности
 * https://www.matburo.ru/tvbook_sub.php?p=par15
 *
 * Если кратко:
 * В урне 3 белых и 3 черных шара. Из урны дважды вынимают по одному шару, не
 * возвращая их обратно. Найти вероятность появления белого шара.
 *
 * Как будем делать:
 *
 * 1. Создать класс с моделированием эксперимента, в нем должна быть коллекция
 * (List) моделирующая урну с шариками (1 - белый шарик, 0 - черный шарик) и
 * функция случайного выбора 2х шариков без возвращения (scala.util.Random),
 * возвращать эта функция должна true (если был выбран белый шар) и false (в
 * противном случае)
 * 2. создать коллекцию обьектов этих классов, скажем 10000 элементов, и
 * провести этот эксперимент (функция map)
 * 3. посчитать количество элементов массива из пункта 2 где функция вернула
 * true, это количество поделенное на общее количество элементов массива
 *
 * PS: чем больше будет количество опытов в пункте 2, тем ближе будет результат
 * моделирования к аналитическому решению.
 *
 *
 * Критерии оценки:
 * Результат должен быть близок к ожидаемому
 * Использовать как можно меньше явных циклов, отдавая предпочтение
 * высокоуровневым функциям, как то map, foreach, fold, filter
 * Используйте только стандартные библиотеки из базового набора
 */
object BlackWhiteBallsExperiment extends App {

  /**
   * Получение коллекции с исключением из неё одного элемента по индексу
   */
  def without[A](elements: Seq[A], index: Int) = elements.take(index) ++ elements.drop(index + 1)

  // Почему-то не получается сделать более универсально - что бы работало для любых Iterable - не пойму, в чём ошибка?:
//  def without[A, I <: Iterable[A]](elements: I, index: Int): I = elements.take(index) ++ elements.drop(index + 1)

  class Urn(
    /**
     * Коллекция, моделирующая урну с шариками (1 - белый шарик, 0 - черный шарик)
     */
    private val balls: Seq[Int]) {

    def getBall: (Boolean, Urn) = {
      val index = Random.nextInt(balls.length)
      val isWhite = balls(index) == 1
      (isWhite, new Urn(without(balls, index)))
    }
  }

  object Urn {
    def isWhiteTaken(): Boolean = {
      val (isFirstWhite, urn) = new Urn(Vector(1, 1, 1, 0, 0, 0)).getBall
      isFirstWhite || urn.getBall._1
    }
  }

  private val NumberOfExperiments = 10_000

  private val percents: Double = LazyList.range(0, NumberOfExperiments)
    .map(_ => Urn.isWhiteTaken())
    .count(_ == true) * 100.0 / NumberOfExperiments

  println(s"Based on $NumberOfExperiments experiments, we can conclude that the probability of getting a white ball is $percents%")
}
