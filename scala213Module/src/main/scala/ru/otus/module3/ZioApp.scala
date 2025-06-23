package ru.otus.module3

import zio.{Scope, Unsafe, ZIO, ZIOAppArgs, ZIOAppDefault}

import scala.io.StdIn

object ZioApp {

  def main(args: Array[String]): Unit = {
      Unsafe.unsafe { implicit u =>
        zio.Runtime.default.unsafe.run(ZIO.attempt(println("Hello world")))
      }
  }

}

object ZIOApp2 extends ZIOAppDefault{
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    ZIO.attempt(println("Hello world"))
}