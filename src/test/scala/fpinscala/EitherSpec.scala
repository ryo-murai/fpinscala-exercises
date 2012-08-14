package fpinscala

import scala.{Either=>_, Left=>_, Right=>_}
import fpinscala.answers._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class EitherSpec extends FlatSpec with ShouldMatchers {
  "map" should """return the result of f if it has a Right value, 
      otherwise return Left value""" in {
    def f: Int => String = _.toString

    Right(4000) map f should equal(Right("4000"))
    Left(30) map f should equal(Left(30))
  }
  
  "flatMap" should """return the result of f if it has a Right value,
      otherwise return Left value""" in {
    def f: Int => Either[Int, String] = i => Right(i.toString)

    Right(4000) flatMap f should equal(Right("4000"))
    Left(30) flatMap f should equal(Left(30))
    
    Right(4000) flatMap (_ => Left("left value")) should equal(Left("left value"))
  }
  
  "orElse" should """return the Right value if it has, 
      otherwise return the result of b""" in {
    
    val right = Right("4000"): Either[Int, String]
    val left = Left(4000): Either[Int, String]
    val br = Right("12000")
    val bl = Left(12000)

    right orElse br should equal(right)
    right orElse bl should equal(right)
    left  orElse br should equal(br)
    left  orElse bl should equal(bl)
  }
  
  "map2" should """return the result of f with both Right values of this and b
      if they have, otherwise return Left""" in {
    val right = Right(4000)
    val left  = Left(false)
    val br = Right("hello")
    val bl = Left(true)
    def f = (i:Int, s:String) => s + " " + i.toString
    
    right.map2(br)(f) should equal(Right("hello 4000"))
    right.map2(bl)(f) should equal(bl)
    left.map2(br)(f) should equal(left)
    left.map2(bl)(f) should equal(left)
  }
}

class HandlingOptionSpec extends FlatSpec with ShouldMatchers {
  import fpinscala.answers.ErrorHandling._
  
  "sequence" should """return the Left value if the original list contains Left even once, 
      otherwise return a list of all the Right values""" in {
    val exception = new RuntimeException("error")

    val allright = (1 to 8).map(n => Right(n)).toList
    val lastLeft = allright :+ Left(exception)
    val firstLeft = Left(exception) :: allright

    sequence(allright) should equal(Right((1 to 8)))
    sequence(lastLeft) should equal(Left(exception))
    sequence(firstLeft) should equal(Left(exception))
  }
  
  "traverse" should """return the Left value if the f returns Left even once,
      otherwise return the Right a list of all the result of f.""" in {
    val e = new RuntimeException("error")

    def fr(n:Int) = Right(n * 4)
    def fl(n:Int) = if(n%2==0) Left(e) else Right(n)
    val allright = (1 to 8) map (Right(_)) toList
    val lastleft = allright :+ Left(e)

    traverse(allright)(fr) should equal(Right(Range(4, 32+1, 4)))
    traverse(allright)(fl) should equal(Left(e))
    traverse(lastleft)(fr) should equal(Left(e))
  }

  "mkPerson" should """return either person value or a string value with error message""" in {
    val (validname, invalidname) = ("John", "")
    val (validage, invalidage) = (30, -1)

    mkPerson(validname,   validage) match {
      case Right(p) => p should equal(Person(new Name(validname), new Age(validage)))
    }
      
    mkPerson(invalidname, validage)   should have ('isLeft (true))
    mkPerson(validname,   invalidage) should have ('isLeft (true))
  }

  "mkPerson2" should """return the value as signature...""" in {
    def extract(e: Either[List[String],Person]) = e match {case Left(x) => x; case _ => Nil}
    val (validname, invalidname) = ("John", "")
    val (validage, invalidage) = (30, -1)

    mkPerson2(validname,   validage) match {
      case Right(p) => p should equal(Person(new Name(validname), new Age(validage)))
    }

    extract(mkPerson2(invalidname, validage))   should have ('length (1))
    extract(mkPerson2(validname,   invalidage)) should have ('length (1))
    extract(mkPerson2(invalidname, invalidage)) should have ('length (2))
  }
}