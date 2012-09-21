package fpinscala

import scala.{Option=>_, Some=>_, None=>_}
import fpinscala.answers._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class OptionSpec extends FlatSpec with ShouldMatchers {
  "getOrElse" should """returns the result inside the Some case of the Option, 
      or if the Option is None, returns the given default value.""" in {

     Some("have a value").getOrElse("default") should equal("have a value")

     None.getOrElse("default") should equal("default")
  }

  "orElse" should """returns the first Option if it is defined, 
      otherwise, returns the second Option""" in {
  
    Some("have a value").orElse(None) should equal(Some("have a value"))
    
    val expected = "other value"
    None.orElse(Some(expected)) match {
      case Some(a) => a should equal(expected)
      case _ => fail
    }
    
    None.orElse(None) should equal(None)
  }
  
  "filter" should """returns the first Option if it is defined and 
      function f evaluates its inside value as true, 
      otherwise, return the None""" in {
    Some("FP in scala").filter(x => x.contains("scala")) should equal(Some("FP in scala"))
    Some("FP in haskell").filter(x => x.contains("scala")) should equal(None)

    None.filter(_ => true) should equal(None)
  }
  
  "map" should """returns the given function's result with inside the Some case of the Option,
      otherwise, returns the None""" in {
    def f: Int => String = _.toString
    
    Some(4000).map(f) should equal(Some("4000"))
    None.map(f) should equal(None)
  }

  "flatMap" should """returns the given function's result with inside the Some case of the Option,
      otherwise, returns the None""" in {
    Some(4000).flatMap(i => Some(i.toString())) should equal(Some("4000"))
    Some(4000).flatMap(_ => None) should equal(None)
    
    None.flatMap(_ => Some("other")) should equal(None)
  }
}

class HandlingOptionSpec extends FlatSpec with ShouldMatchers {
  import fpinscala.answers.ErrorHandling._

  "variance" should """returns if the mean is m, variance variance
      is the mean of (x - m)^2""" in {
    
    variance(Seq(1.0, 2.0, 3.0)) should equal(Some(2.0 / 3.0))
    variance(Seq(6.0, 6.0, 6.0)) should equal(Some(0))
    variance(Seq.empty) should equal(None)
  }

  "map2" should """return combines two Options into one using a binary function. 
    If either Option value is None, then the return value is too. """ in {
    val none = None: Option[Int]
    
    map2(Some(1), Some(2))((x,y) => x+y) should equal(Some(3))
    map2(Some(1), none   )((x,y) => x+y) should equal(None)
    map2(none,    Some(2))((x,y) => x+y) should equal(None)
    map2(none,    none   )((x,y) => x+y) should equal(None)
  }

  "bothMatch" should """return true if s matches both patterns, 
      otherwise returns false.""" in {
    val s = "FP in scala"
    val matchp1 = "^FP.*"; val matchp2 = ".*scala$"
    val unmatchp1 = "^FD.*"; val unmatchp2 = ".*scale$"

    bothMatch(matchp1,   matchp2, s)   should equal(Some(true))
    bothMatch(unmatchp1, matchp2, s)   should equal(Some(false))
    bothMatch(matchp1,   unmatchp2, s) should equal(Some(false))
    bothMatch(unmatchp1, unmatchp2, s) should equal(Some(false))
  }

  it should """return Either patterns is incorrect, returns None.""" in {
    val validp = "^scala"; val invalidp = "[^s"
    bothMatch(validp,   invalidp, "any") should equal(None)
    bothMatch(invalidp, validp,   "any") should equal(None)
    bothMatch(invalidp, invalidp, "any") should equal(None)
  }

  "sequence" should """return None if the original list contains None even once, 
      otherwise return Some with a list of all the values.""" in {
    sequence(List(Some(1), Some(2), Some(3))) should equal(Some(List(1,2,3)))
    sequence(List(Some(1), None,    Some(3))) should equal(None)
  }

  "traverse" should """return None if the f returns None even once, 
      otherwise return Some with a list of all the results of f.""" in {
    val l = List(1, 2, 3, 4, 5)

    val f:Int => Int = n => n * 2
    traverse(l)(n => Some(f(n))) should equal(Some(l.map(f)))

    traverse(l)(n => if(n<5) Some(n) else None) should equal(None)
    traverse(l)(_ => None) should equal(None)
  }
}
