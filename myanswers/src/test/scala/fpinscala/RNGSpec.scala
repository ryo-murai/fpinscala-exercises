package fpinscala

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import fpinscala.answers.RNG

class RNGSpec extends FlatSpec with ShouldMatchers {
  import answers.FunctionalStateHandling._
  
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
    d should be (n.toDouble/(Int.MaxValue))
    d should (be >= 0.0 and be < 1.0)
  }

  "intDouble" should """generate a pair of random Int and Double values""" in {
    val rng1 = RNG.simple(1024)
    val (i,rng2) = positiveInt(rng1)
    val (d,_)    = nextDouble(rng2)
    
    val ((a1, a2), _) = intDouble(rng1) 
    (a1, a2) should equal(i, d)
  }
  
  "doubleInt" should """generate a pair of random Int and Double values""" in {
    val rng1 = RNG.simple(1024)
    val (d,rng2) = nextDouble(rng1)
    val (i,_)    = positiveInt(rng2)
    
    val ((a1, a2), _) = doubleInt(rng1)
    (a1, a2) should equal(d, i)
  }
  
  "double3" should """generate a 3-tuple of random Double values""" in {
    val rng1 = RNG.simple(1024)
    val (d1,rng2) = nextDouble(rng1)
    val (d2,rng3) = nextDouble(rng2)
    val (d3,_)    = nextDouble(rng3)

    val ((a1, a2, a3), _) = double3(rng1)
    (a1, a2, a3) should equal(d1, d2, d3)
  }

  "ints" should """generate a list containing the given number 
      of random integers""" in {
    val rng1 = RNG.simple(1024)
    val (i0,rng2) = positiveInt(rng1)
    val (i1,rng3) = positiveInt(rng2)
    val (i2,_) =    positiveInt(rng3)
    val equivList = List(i0, i1, i2).sorted
    
    ints(3)(rng1)._1.sorted should equal(equivList)

    ints(24)(rng2)._1 should have ('size (24))
  }

  "units" should """return the same input and remains in the same state""" in {
    val rng1 = RNG.simple(1024)
    val i = 3

    def f = unit(i)
    f(rng1) should equal(i, rng1)
  }

  "map[A,B](a: RNG => (A,RNG))(f: A => B): RNG => (B,RNG)" should
    """do transforming the output of a state action map without 
        modifying the state""" in {
    val rng1 = RNG.simple(1024)
    val (i,rng2) = positiveInt(rng1)
    
    map(positiveInt(_))(_*2)(rng1)._1 should be (i*2)
  }

  "zeroToTen" should """generate an Int between 0 and 10""" in {
    val rng = RNG.simple(1024)
    zeroToTen(rng)._1 should (be >= 0 and be < 10)
  }

  "zip" should """do transforming to a function which generate a pair 
      of random values""" in {
    val rng1 = RNG.simple(1024)
    val (i1,rng2) = positiveInt(rng1)
    val (d1,rng3) = nextDouble(rng2)
    val (i2,rng4) = nextDouble(rng3)
    val (d2, _  ) = positiveInt(rng4)

    zip(r1 => positiveInt(r1), r2 => nextDouble(r2))(rng1)._1 should equal (i1, d1)
    zip(r1 => nextDouble(r1), r2 => positiveInt(r2))(rng3)._1 should equal (i2, d2)
  }

  "sequence" should """do combining a List of transitions into 
      a single transition""" in {
    val source = unit(1) :: unit(2) :: unit(3) :: Nil
    val expect = 1 :: 2 :: 3 :: Nil

    val rng = RNG.simple(1024)

    sequence(source)(rng)._1 should equal(expect)
  }

  "exercise10" should """generate a function to return a list 
      with ramdom number(up to ten) of double(0 to 1) values""" in {
    val rng = RNG.simple(1024)
    val (xs, _) = exercise10(rng)
    xs.size should be <= 10
    xs.forall(d => d >= 0 && d < 10) should be(true)
  }
  
  "flatMap" should """do generalized unlike sequence.""" in {
    val rng = RNG.simple(1024)
    flatMap(unit(5))(a => r => (a*2,r))(rng)._1 should equal(10)
  }

  "exercise10r" should """generate a function to return a list 
      with ramdom number(up to ten) of double(0 to 1) values""" in {
    val rng = RNG.simple(1024)
    val (xs, _) = exercise10r(rng)
    xs.size should be <= 10
    xs.forall(d => d >= 0 && d < 10) should be(true)
  }
}

class GeneralizedFunctionalStateHandlingSpec extends FlatSpec with ShouldMatchers {
  import answers.GeneralizedFunctionalStateHandling._

  private def atoi(s: String) = (Integer.parseInt(s), s+"0")
  
  "map[S,A](a: S => (A,S))(f: A => B): S => (B,S)" should 
    "do as a generalized map previously implemented" in {
    def f(str: String) = (Integer.parseInt(str), str+"0")
    
    val tpl = map(f)(_*3)("10") 
    tpl should equal (30,"100")
  }

  "unit[S,A](a: A): S => (A,S)" should "generate function to make tuple2" in {
    unit(8)("eight") should equal(8,"eight")
  }

  "zip[S,A,B](a: S => (A,S), b: S => (B,S)): S => ((A,B),S)" should
    "generate a generalized zip" in {
    zip(atoi, atoi)("2") should equal((2,20),"200")
  }

  "sequence[S,A](as: List[S => (A,S)]): S => (List[A], S)" should
    "generate a generalized sequence" in {
    sequence(List.fill(4)(atoi _))("1") should equal(List(1,10,100,1000), "10000")
  }
}