package fpinscala.answers

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed*0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
    }
  }
}

object FunctionalStateHandling {
  /**
    * EXERCISE 1
    */
  def positiveInt(rng: RNG): (Int, RNG) = 
    rng.nextInt match { case (i, r) => (i.abs, r) }

  /**
    * EXERCISE 2
    */
  def nextDouble(rng: RNG): (Double, RNG) =
    positiveInt(rng) match { case (i, r) => (i.toDouble/(Int.MaxValue), r) }

  /**
    * EXERCISE 3
    */
  def intDouble(rng: RNG): ((Int,Double), RNG) =
    positiveInt(rng) match { case (i,r1) => nextDouble(r1) match {case (d,r2) => ((i,d),r2)} }

  /**
    * EXERCISE 3
    */
  def doubleInt(rng: RNG): ((Double,Int), RNG) =
    nextDouble(rng) match { case (d,r1) => positiveInt(r1) match {case (i,r2) => ((d,i),r2)} }
    //sys.error("todo")

  /**
    * EXERCISE 3
    */
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    def f(l: List[Double], r:RNG, cnt:Int): (List[Double], RNG) =
      if(cnt > 0) nextDouble(r) match {case (d,r0) =>  f(d::l, r0, cnt-1)}
      else (l, r)

    // is there any utilities to convert a list to tuple-N ?
    f(Nil, rng, 3) match { case (l, r) => ((l(2),l(1),l(0)),r) }
  }

  /**
    * EXERCISE 4
    */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def f(l: List[Int], r:RNG, cnt:Int): (List[Int], RNG) =
      if(cnt > 0) positiveInt(r) match {case (i,r0) =>  f(i::l, r0, cnt-1)}
      else (l, r)

    f(Nil, rng, count)
  }
    
  /**
    * EXERCISE 5
    */
  def unit[A](a: A): RNG => (A,RNG) =
    r => (a, r)

  /**
    * EXERCISE 6
    */
  def map[A,B](a: RNG => (A,RNG))(f: A => B): RNG => (B,RNG) =
    r => (a(r) match {case(a0, r0)=>(f(a0), r0)})

  /**
    * EXERCISE 7
    * Use map to generate an Int between 0 and 10
    */
  def zeroToTen: RNG => (Int, RNG) =
    map(positiveInt(_))(_%11)

  /**
    * EXERCISE 8
    */
  def zip[A,B](a: RNG => (A,RNG), b: RNG => (B,RNG)): RNG => ((A,B),RNG) =
    sys.error("todo")

  /**
    * EXERCISE 9
    */
  def sequence[A](as: List[RNG => (A,RNG)]): RNG => (List[A], RNG) =
    sys.error("todo")

  /**
    * EXERCISE 10 (hard): Write a function to first generate an integer between 0
    * and 10, then generate a List of that length, consisting of random doubles
    * between 0 and 1. Can this be implemented by somehow composing the ints
    * function you wrote with the function you just wrote for generating values 
    * between 0 and 10?
    */
  def exercise10: RNG => (List[Int], RNG) =
    sys.error("todo")

  /**
    * EXERCISE 11
    */
  def flatMap[A,B](a: RNG => (A,RNG))(f: A => (RNG => (B,RNG))): RNG => (B,RNG) =
    sys.error("todo")
}

object GeneralizedFunctionalStateHandling {
  /**
    * EXERCISE 11
    */
  def map[S,A,B](a: S => (A,S))(f: A => B): S => (B,S) =
    sys.error("todo")

  /**
    * EXERCISE 12
    */

}