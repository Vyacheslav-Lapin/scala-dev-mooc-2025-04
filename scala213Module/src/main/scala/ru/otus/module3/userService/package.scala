package ru.otus.module3

import ru.otus.module3.emailService.{Email, EmailAddress, EmailService, Html}
import ru.otus.module3.userDAO.UserDAO
import zio.{Console, RIO, Task, UIO, URIO, URLayer, ZIO, ZLayer}

package object userService {

  /**
   * Реализовать сервис с одним методом
   * notifyUser, принимает id пользователя в качестве аргумента и шлет ему уведомление
   * при реализации использовать UserDAO и EmailService
   */

   trait UserService {
    def notifyUser(id: UserID): Task[Unit]
   }

   class UserServiceImpl(userDAO: UserDAO, emailService: EmailService) extends UserService{
     override def notifyUser(id: UserID): Task[Unit] = for{
       user <- userDAO.findBy(id).some.mapError(_ => new Throwable((s"User not found ${id}")))
       email = Email(user.email, Html("Hello world"))
       _ <- emailService.sendEmail(email)
     } yield ()
   }

   object UserService {
     val live: ZLayer[UserDAO with EmailService, Nothing, UserService] = ZLayer(
       for{
         emailService <- ZIO.service[EmailService]
         userDAO <- ZIO.service[UserDAO]
         userService <- ZIO.succeed(new UserServiceImpl(userDAO, emailService))
       } yield userService
     )

     def notifyUser(id: UserID): RIO[UserService, Unit] = ZIO.serviceWithZIO(_.notifyUser(id))
   }

}