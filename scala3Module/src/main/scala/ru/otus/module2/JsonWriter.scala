package ru.otus.module2

import ru.otus.module2.JsValue.{JsArray, JsBoolean, JsNull, JsNumber, JsObject, JsString}
import ru.otus.module2.JsonWriter.from
import ru.otus.module2.jsonInstances.given

enum JsValue:
  case JsObject(get: Map[String, JsValue])
  case JsArray(get: List[JsValue])
  case JsString(get: String)
  case JsNumber(get: Double)
  case JsBoolean(get: Boolean)
  case JsNull

trait JsonWriter[T]:
  def write(v: T): JsValue

object JsonWriter:
  def apply[T](implicit ev: JsonWriter[T]): JsonWriter[T] = ev
  def from[T](f: T => JsValue): JsonWriter[T] = (v: T) => f(v)

object jsonInstances:
  given JsonWriter[Int] = from[Int](JsNumber(_))
  given JsonWriter[Double] = from[Double](JsNumber(_))
  given JsonWriter[Boolean] = from[Boolean](JsBoolean(_))
  given JsonWriter[String] = from[String](JsString(_))
  given [T](using ev: JsonWriter[T]): JsonWriter[List[T]] = from[List[T]]{ list => JsArray(list.map(ev.write)) }
  given [T](using ev: JsonWriter[T]): JsonWriter[Map[String, T]] = from[Map[String, T]] { it =>
    JsObject(it.map { case (k, v) => (k, ev.write(v)) })
  }
  given [T](using ev: JsonWriter[T]): JsonWriter[Option[T]] = from[Option[T]] {
    case Some(value) => ev.write(value)
    case None => JsNull
  }

extension[T](v: T)(using ev: JsonWriter[T])
  def toJson: JsValue = ev.write(v)

// пришлось назвать метод по-другому из-за конфликта имён — toJson уже определён в данном пакете в модуле scala213
def jsonize[T: JsonWriter](v: T): JsValue = JsonWriter[T].write(v)

@main
def test(): Unit =
  println(jsonize("fdvvbfbv")) // "JsString(fdvvbfbv)"
  println(jsonize(10)) // "JsNumber(10.0)"
  println(jsonize(Option(10))) // "JsNumber(10.0)"
  println(jsonize(Option("dfbgfgnhg"))) // "JsString(dfbgfgnhg)"

  println("bghbbgfrbgbngf".toJson) // "JsString(bghbbgfrbgbngf)"
  println(155.toJson) // "JsNumber(155.0)"
  println(true.toJson) // "JsBoolean(true)"
  println(List(1,2,3,4).toJson) // "JsArray(List(JsNumber(1.0), JsNumber(2.0), JsNumber(3.0), JsNumber(4.0)))"
  println(Map("q" -> 1, "w" -> 2, "e" -> 3).toJson) // "JsObject(Map(q -> JsNumber(1.0), w -> JsNumber(2.0), e -> JsNumber(3.0)))"
  println(Option(10).toJson) // "JsNumber(10.0)"
