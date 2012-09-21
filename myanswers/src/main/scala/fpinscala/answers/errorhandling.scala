package fpinscala.answers

import java.util.regex._
import scala.util.control.Exception._
import scala.{Option=>_, Some=>_, None=>_}
import scala.{Either=>_, Left=>_, Right=>_}

/**
 * EXERCISE 1
 */
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = 
    this match {
      case Some(a) => Some(f(a))
      case None => None
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this map f getOrElse None


  def getOrElse[B>:A](default: => B): B =
    this match {
      case Some(a) => a
      case None => default
    }

  def orElse[B>:A](ob: Option[B]): Option[B] =
    this match {
      case Some(a) => Some(a)
      case None => ob
    }

  def filter(f: A => Boolean): Option[A] =
    this match {
      case Some(a) if(f(a)==true) => Some(a)
      case _ => None
    }
}

object Option {
  def apply[A](a: A): Option[A] = if(a != null) Some(a) else None
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


/**
 * EXERCISE 7
 */
trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }

  def flatMap[B, E1>:E](f: A => Either[E1, B]): Either[E1, B] =
    this match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }

  def orElse[B>:A, E1>:E](b: Either[E1, B]): Either[E1, B] =
    this match {
      case Right(a) => Right(a)
      case _ => b
    }

  def isLeft: Boolean = this match { case Left(e) => true; case Right(a) => false; }

  def fold[X](fe: (E) => X, fa: (A) => X): X =
    this match {
      case Left(e) => fe(e)
      case Right(a) => fa(a)
    }

  def map2[B,C, E1>:E](b: Either[E1, B])(f: (A, B) => C): Either[E1, C] =
    this.flatMap(a => b.flatMap(bx => Right(f(a, bx))))

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

/**
 * EXERCISE 9
 */
case class Person(name: Name, age: Age)
sealed class Name(val value: String) {
  // added for testing
  override def equals(that: Any) = that match { 
    case other: Name => other.value == value 
    case _ => false 
  }
}
sealed class Age(val value: Int) {
  // added for testing
  override def equals(that: Any) = that match { 
    case other: Age => other.value == value 
    case _ => false 
  }
}

object ErrorHandling {
  def mean(xs: Seq[Double]): Option[Double] = {
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  /**
   * EXERCISE 2
   * Variance can actually be computed in one pass, but for pedagocial purposes 
   * we will compute it using two passes. The first will compute the mean of 
   * the data set, and the second will compute the mean squared difference from 
   * this mean
   */
  def variance(xs: Seq[Double]): Option[Double] = 
    mean(xs) flatMap { case m => mean(xs.map(x => math.pow(x - m, 2))) }

  /**
   * EXERCISE 3
   */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(a1 => b.map(b1 => f(a1,b1)))

  /**
   * EXERCISE 4
   */
  def bothMatch(p1: String, p2: String, s: String): Option[Boolean] = {
    def mkMatcher(pat: String): Option[String => Boolean] =
      try {
        val p = Pattern.compile(pat)
        Some(s0 => p.matcher(s0).matches)
      } catch {
        case _: PatternSyntaxException => None
      }
  
    for {
      m1 <- mkMatcher(p1)
      m2 <- mkMatcher(p2)
    } yield m1(s) && m2(s)
  }

  /**
   * EXERCISE 5
   */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(Nil): Option[List[A]])((h,t) => map2(h,t)(_::_))

  /**
   * EXERCISE 6
   */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(Nil): Option[List[B]])((h,t) => map2(f(h),t)(_::_))

  /**
   * EXERCISE 8
   */
  def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] =
    a.foldRight(Right(Nil): Either[E, List[A]])((h,t) => h.flatMap(h0 => t.map(h0::_)))

  /**
   * EXERCISE 8
   */
  def traverse[E, A, B](a: List[Either[E, A]])(f: A => Either[E, B]): Either[E, List[B]] =
    a.foldRight(Right(Nil): Either[E, List[B]])((h,t) => h flatMap f flatMap(b => t.map(b::_)))

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  /**
   * EXERCISE 9
   */
  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).flatMap(n => mkAge(age).map(a => Person(n, a)))

  /**
   * EXERCISE 9
   */
  def mkPerson2(name: String, age: Int): Either[List[String], Person] = 
    (mkName(name) :: mkAge(age) :: Nil).partition(_.isLeft) match {
      case (Nil, (r0 :: r1 :: Nil)) => 
        val (n:Right[Name], a:Right[Age]) = (r0, r1)
        n.flatMap(
          n0 => 
          a.map(Person(n0,_))
            .orElse(Left(List())))
      case (errors, _) => Left(for(Left(s) <- errors) yield s)
    }
}
