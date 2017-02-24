import scala.concurrent.Future
import scala.language.higherKinds

import scala.concurrent.ExecutionContext.Implicits.global


trait Inner[F] {
  type T
}

object Inner {
  def apply[F](implicit inner: Inner[F]): Inner[F] = inner

  implicit def mk[F[_], A] = new Inner[F[A]] {
    type T = A
  }
}

val inner = Inner[List[_]]

trait IsFuture[F] { // extends Inner[F]
  type T

  def apply(f: F): Future[T]
}

object IsFuture {
  def apply[F](implicit isFuture: IsFuture[F]): IsFuture[F] = isFuture

  implicit def mk[A] = new IsFuture[Future[A]] {
    type T = A

    def apply(f: Future[A]): Future[A] = f
  }
}

def logResult[Thing](thing: Thing)
  (implicit isFuture: IsFuture[Thing]): Future[isFuture.T] =
    isFuture(thing).map { x =>
      println(s"log $x")
      x
    }

logResult(Future.successful("Blah"))