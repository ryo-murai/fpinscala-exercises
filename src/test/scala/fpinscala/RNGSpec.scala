package fpinscala

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import fpinscala.answers.RNG

class RNGSpec extends FlatSpec with ShouldMatchers {
  import fpinscala.answers.FunctionalStateHandling._
  
  "positiveInt" should """generate a random positive integer.""" in {
    val rng = RNG.simple(-8)
    val (n,_) = rng.nextInt

    n should be < 0   // pre-condition of this test 
    
    val (p,_) = positiveInt(rng)
    p should equal(n.abs)
  }

  "nextDouble" should """generate a Double between 0 to 1,
      not including 1.""" in {
    val rng = RNG.simple(1024)
    val (n,_) = positiveInt(rng)

    val (d,_) = nextDouble(rng)
    d should be (n.toDouble/(Int.MaxValue-1))
    d should (be >= 0.0 and be < 1.0)
  }

  "intDouble" should """generate a pair of random Int and Double values""" in {
    val rng1 = RNG.simple(1024)
    val (i0,rng2) = positiveInt(rng1)
    val (d0,_)    = nextDouble(rng2)
    
    val (i, d, _) = 
  }
  
  "doubleInt" should """generate a pair of random Int and Double values""" in {
  }
  
  "double3" should """generate a 3-tuple of random Double values""" in {
  }
}