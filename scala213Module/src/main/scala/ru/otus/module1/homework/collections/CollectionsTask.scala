package ru.otus.module1.homework.collections

import scala.annotation.tailrec

object CollectionsTask {

  private def isASCIIString(str: String): Boolean = str.matches("[A-Za-z]+")

  /**
   * Реализуйте метод который первый элемент списка не изменяет, а для последующих алгоритм следующий:
   * если isASCIIString is TRUE тогда пусть каждый элемент строки будет в ВЕРХНЕМ регистре
   * если isASCIIString is FALSE тогда пусть каждый элемент строки будет в нижнем регистре
   * Пример:
   * capitalizeIgnoringASCII(List("Lorem", "ipsum", "dolor", "sit", "amet")) -> List("Lorem", "IPSUM", "DOLOR", "SIT", "AMET")
   * capitalizeIgnoringASCII(List("Оказывается", "," "ЗвУк", "КЛАВИШЬ", "печатной", "Машинки", "не", "СТАЛ", "ограничивающим", "фактором")) ->
   * List("Оказывается", "," "звук", "КЛАВИШЬ", "печатной", "машинки", "не", "стал", "ограничивающим", "фактором")
   * HINT: Тут удобно использовать collect и zipWithIndex
   *
   * * */
  def capitalizeIgnoringASCII(text: List[String]): List[String] = text.head :: (
    for (str <- text.tail) yield
      if (isASCIIString(str)) str.toUpperCase()
      else str.toLowerCase
    )

  /**
   * Компьютер сгенерировал текст, используя вместо прописных чисел, числа в виде цифр, помогите компьютеру заменить цифры на числа.
   * В тексте встречаются числа от 0 до 9
   *
   * Реализуйте метод, который цифровые значения в строке заменяет на числа: 1 -> one, 2 -> two
   *
   * HINT: Для всех возможных комбинаций чисел стоит использовать Map
   * * */

  // (head +: tail)-ike pattern-matching for String
  private object s_+: {
    def unapply(s: String): Option[(Char, String)] = s.headOption.map {
      (_, s.tail)
    }
  }

  @tailrec
  def numbersToNumericString(text: String, continuationString: String = ""): String =
    text match {
      case '0' s_+: otherString => numbersToNumericString(otherString, continuationString + "zero")
      case '1' s_+: otherString => numbersToNumericString(otherString, continuationString + "one")
      case '2' s_+: otherString => numbersToNumericString(otherString, continuationString + "two")
      case '3' s_+: otherString => numbersToNumericString(otherString, continuationString + "three")
      case '4' s_+: otherString => numbersToNumericString(otherString, continuationString + "four")
      case '5' s_+: otherString => numbersToNumericString(otherString, continuationString + "five")
      case '6' s_+: otherString => numbersToNumericString(otherString, continuationString + "six")
      case '7' s_+: otherString => numbersToNumericString(otherString, continuationString + "seven")
      case '8' s_+: otherString => numbersToNumericString(otherString, continuationString + "eight")
      case '9' s_+: otherString => numbersToNumericString(otherString, continuationString + "nine")
      case nonDigitChar s_+: otherString => numbersToNumericString(otherString, continuationString + nonDigitChar)
      case _ => continuationString
    }

  /**
   * У нас есть два дилера со списками машин, которые они обслуживают и продают (case class Auto(mark: String, model: String)).
   * Базы данных дилеров содержат тысячи и больше записей.
   * Нет гарантии, что записи уникальные и не имеют повторений.
   * HINT: Set
   * HINT2: Iterable стоит изменить
   * * */
  case class Auto(mark: String, model: String)

  /**
   * Хотим узнать, какие машины можно обслужить, учитывая этих двух дилеров.
   * Реализуйте метод, который примет две коллекции (два источника) и вернёт объединенный список уникальных значений
   * */
  def intersectionAuto(dealerOne: Iterable[Auto], dealerTwo: Iterable[Auto]): Iterable[Auto] = dealerOne.toSet ++ dealerTwo

  /**
   * Хотим узнать, какие машины обслуживается в первом дилерском центре, но не обслуживаются во втором.
   * Реализуйте метод, который примет две коллекции (два источника)
   * и вернёт уникальный список машин, обслуживающихся в первом дилерском центре и не обслуживающимся во втором
   * */
  def filterAllLeftDealerAutoWithoutRight(dealerOne: Iterable[Auto], dealerTwo: Iterable[Auto]): Iterable[Auto] =
    dealerOne.toSet.filter(auto => !dealerTwo.exists(_ == auto))
}
