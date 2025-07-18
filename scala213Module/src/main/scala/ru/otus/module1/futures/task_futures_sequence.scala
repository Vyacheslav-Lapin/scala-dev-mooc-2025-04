package ru.otus.module1.futures

import scala.concurrent.{ExecutionContext, Future}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличие от нее,
   * возвращающую все успешные и неуспешные результаты.
   * Возвращаемый тип функции — кортеж из двух списков:
   * <ul>
   * <li>В левом хранятся результаты успешных выполнений;</li>
   * <li>В правом — результаты неуспешных выполнений.</li>
   * </ul>
   * Не допускается использование методов объекта Await
   * и mutable'ных переменных (var).
   *
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежем из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] =
    futures.foldRight(Future.successful[(List[A], List[Throwable])]((Nil, Nil))) { (nextFuture, resultFuture) =>
      resultFuture.flatMap(result =>
        nextFuture.map(value =>
            result match {
              case (values, throwables) => (value :: values, throwables)
            }
          )
          .recover { case throwable =>
            result match {
              case (values, throwables) => (values, throwable :: throwables)
            }
          }
      )
    }
}
