package me.chuwy.otusbats

import scala.concurrent.Future

trait Monad[F[_]] extends Functor[F] { self =>
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def point[A](a: A): F[A]

  // 2. Реализовать в трэйте Monad метод flatten с помощью flatMap
  // (т.е. flatMap в трэйте остаётся нереализован, а flatten вызывает flatMap)
  def flatten[A](fa: F[F[A]]): F[A] = flatMap(fa)(a => a)
}

object Monad {
  // 3. Имплементировать инстансы Monad для а) Option, b) List, c) одного типа на выбор ученика.
  // внутри инстанса вы можете просто "перебросить" flatMap тайп класса на flatMap стандартной коллекции
  implicit val optMonad = new Monad[Option] {
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    override def point[A](a: A): Option[A] = Option(a)

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit val listMonad = new Monad[List] {
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)

    override def point[A](a: A): List[A] = List(a)

    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  implicit val futureMonad = new Monad[Future] {
    import scala.concurrent.ExecutionContext.Implicits.global
    
    override def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap(f)

    override def point[A](a: A): Future[A] = Future(a)

    override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
  }

  // 4. Реализовать оставшиеся элементы тайп класса Monad
  // Какие "оставшиеся"? Вроде, ничего не осталось.
}

object TestMonad {

  def main(args: Array[String]): Unit = {

    def inc10[F[_], A](fa: F[A])(implicit m: Monad[F], n: Numeric[A]): F[A] = {
      import n._
      m.map(fa)(x => x + n.fromInt(10))
    }

    import Monad._

    val lst = List(1, 2, 3)
    val res1 = inc10(lst)
    
    println(s"res1 = $res1")
    assert(res1 == List(11, 12, 13))

    val opt = Option(1)
    val res2 = inc10(opt)
    
    println(s"res2 = $res2")
    assert(res2 == Option(11))
  }
}