package fpinscala

import scala.{Stream => _}  // hide Scala's existing Stream type
import fpinscala.answers._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class MyStreamSpec extends FlatSpec with ShouldMatchers {
  "toList" should """convert a Stream to a List""" in {
    Stream(1,2,3,4).toList should equal(List(1,2,3,4))
    
    Stream.empty.toList should equal(List())
  }

  "take" should """return the first n elements of the Stream""" in {
    val s = Stream(1,2,3,4,5)

    s.take(3).toList should equal(List(1,2,3))
    s.take(8).toList should equal(List(1,2,3,4,5))
    s.take(-1).toList should equal(List())
  }
  
  "takeWhile" should """return all starting elements of a Stream 
      that match the given predicate""" in {
    val s = Stream(1,2,3,4,5)

    s.takeWhile(_ < 4).toList should equal(List(1,2,3))
    s.takeWhile(_ => true).toList should equal(List(1,2,3,4,5))
    s.takeWhile(_ => false).toList should equal(List())
 
    Stream[Int]().takeWhile(_ > 0).toList.size should equal(0)
  }

  "exists" should """return true if an element satisfys the given predicate
      even once, otherwise returns false""" in {
    val s = Stream(1,2,3,4,5)
    
    s.exists(_ > 0) should equal(true)
    s.exists(_ < 0) should equal(false)
    s.exists(_ == 5) should equal(true)
  }
  
  it should "return false if the stream is empty" in {
    Stream.empty[Int].exists(_ > 0) should equal(false)
  }
  
  "forall" should """return true if all elements satisfy the given predicate,
      otherwise returns false""" in {
    val s = Stream(1,2,3,4,5)
    
    s.forall(_ > 0) should equal(true)
    s.forall(_ < 0) should equal(false)
    s.forall(_ < 5) should equal(false)
  }
  
  it should """return as scala.Stream does in the case of empty Stream""" in {
    /**
    as of scala 2.9.2, scala.Stream always returns true if empty
    -----
    scala> Stream[Int]().forall(_ => true)
    res1: Boolean = true

    scala> Stream[Int]().forall(_ => false)
    res2: Boolean = true
     */
    val scalaEmptyStreamForAll = scala.Stream[Int]().forall(_ => true)
    Stream.empty[Int].forall(_ => true) should equal(scalaEmptyStreamForAll)
  }
  
  "takeWhile2" should """return all starting elements of a Stream 
      that match the given predicate""" in {
    val s = Stream(1,2,3,4,5)

    s.takeWhile2(_ < 4).toList should equal(List(1,2,3))
    s.takeWhile2(_ => true).toList should equal(List(1,2,3,4,5))
    s.takeWhile2(_ => false).toList should equal(List())

    Stream[Int]().takeWhile2(_ > 0).toList.size should equal(0)
  }
  
  "map" should """return a new Stream containing all elements converted by given f""" in {
    Stream(1,2,3,4,5).map(_*2).toList should equal(List(2,4,6,8,10))
  }
  
  "filter" should """return selected elements which satisfy a given predicate""" in {
    Stream(1,2,3,4,5).filter(_%2==0).toList should equal(List(2,4))
  }

  "++" should """append a given element to the Stream""" in {
    (Stream(1,2,3) ++ Stream(4,5)).toList should equal(List(1,2,3,4,5))
  }
  
  "flatMap" should """return a new Stream with all nested Streams are flatten""" in {
    def f(n: Int): Stream[Int] =  Stream(Array.fill(n-1)(n): _*)
    Stream(1,2,3,4,5).flatMap(f).toList should equal(List(2,3,3,4,4,4,5,5,5,5))
  }
  
  "constant" should """return an infinite stream filled with given element""" in {
    Stream.constant(8).take(5).toList should equal(List(8,8,8,8,8))
  }
  
  "from" should """return an infinite stream with incremental values 
      started from given value""" in {
    Stream.from(3).take(7).toList should equal(List(3,4,5,6,7,8,9))
  }
  
  "fibs" should """return an infinite stream with fibonacci numbers""" in {
    val fibs = List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    Stream.fibs.take(10).toList should equal(fibs)
  }
  
  "unfold" should """takes an initial state, and a function for producing the
      next state, and the next value in the generated stream""" in {
    val infinite = Stream.unfold(0)(s => Some((s,s+1)))
    val finite = Stream.unfold(5)(s => if(s>0) Some((s,s-1)) else None)

    infinite.take(5).toList should equal(List(0,1,2,3,4))
    finite.toList should equal(List(5,4,3,2,1))

    val fibs = Stream.unfold((0,1))(t => Some((t._1, (t._2, t._1+t._2))))
    fibs.take(10).toList should equal(List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
  }

  "map2" should """return a new Stream containing all elements converted by given f""" in {
    Stream(1,2,3,4,5).map2(_*2).toList should equal(List(2,4,6,8,10))
  }

  "take2" should """return the first n elements of the Stream""" in {
    val s = Stream(1,2,3,4,5)

    s.take2(3).toList should equal(List(1,2,3))
    s.take2(8).toList should equal(List(1,2,3,4,5))
    s.take2(-1).toList should equal(List())
  }
  
  "takeWhile3" should """return all starting elements of a Stream 
      that match the given predicate""" in {
    val s = Stream(1,2,3,4,5)

    s.takeWhile3(_ < 4).toList should equal(List(1,2,3))
    s.takeWhile3(_ => true).toList should equal(List(1,2,3,4,5))
    s.takeWhile3(_ => false).toList should equal(List())
 
    Stream[Int]().takeWhile3(_ > 0).toList.size should equal(0)
  }

  "zip" should """return a list formed from this list and another 
      iterable collection by combining corresponding elements
      in pairs""" in {
    val s = Stream(1,2,3,4,5)
    val longer = Stream("1","2","3","4","5","6")
    val shorter = Stream("1","2","3")
    
    s.zip(longer).toList  should equal(List((1,"1"),(2,"2"),(3,"3"),(4,"4"),(5,"5")))
    s.zip(shorter).toList should equal(List((1,"1"),(2,"2"),(3,"3")))

    val infinite = Stream.unfold(1)(s => Some((s,s+1)))
    infinite.zip(shorter).toList should equal(List((1,"1"),(2,"2"),(3,"3")))
  }

  "zipAll" should """return as zip does except continueing the traversal 
      as long as either stream has more elements. it uses Option to encode
      whether each stream has been exhausted.""" in {
    val s = Stream(1,2,3,4,5)
    val longer = Stream("1","2","3","4","5","6")
    val shorter = Stream("1","2","3")
    
    val longpair = List((Some(1),Some("1")),(Some(2),Some("2")),(Some(3),Some("3")),(Some(4),Some("4")),(Some(5),Some("5")),(None,Some("6")))
    val shortpair = List((Some(1),Some("1")),(Some(2),Some("2")),(Some(3),Some("3")),(Some(4),None),(Some(5),None))
    
    s.zipAll(longer).toList  should equal(longpair)
    s.zipAll(shorter).toList should equal(shortpair)
  }
  
  "startsWith" should """return true if one Stream is a prefix of another.""" in {
    val s = Stream(1,2,3)
    val prefix = Stream(1,2)
    val same   = s
    val notpre = Stream(1,2,4)
    val longer = Stream(1,2,3,4)
    val empty  = Stream.empty[Int]
    
    s.startsWith(prefix) should equal(true)
    s.startsWith(same)   should equal(true)
    s.startsWith(empty)  should equal(true)
    s.startsWith(notpre) should equal(false)
    s.startsWith(longer) should equal(false)
    empty.startsWith(s)  should equal(false)
  }
  
  "tails" should """return the Stream of suffixes of the input sequence, 
      starting with the original Stream""" in {
    val tails = Stream(1,2,3,4).tails

    // verification
    tails.map(_.toList).toList should equal(List(List(1,2,3,4), List(2,3,4), List(3,4), List(4), List()))
  }
  
  "scanRight" should """be a generalization of tails to the function scanRight, 
      which applies a foldRight to each value returned by tails.""" in {
    // http://www.scala-lang.org/api/current/scala/collection/immutable/Stream.html#scanRight[B, That](B)((A, B) ⇒ B)(CanBuildFrom[Stream[A], B, That]):That
    Stream(1,2,3,4).scanRight(0)(_+_).toList should equal(List(10, 9, 7, 4, 0))
  }
  
  "hasSubsequence" should """return true if the given Stream is a sub-sequence of this,
      otherwise return false.""" in {
    val s = Stream(1,2,3,4,5)
    val sub = Stream(2,3,4)
    val notsub = Stream(3,4,5,6)
    
    s.hasSubsequence(sub)    should equal(true)
    s.hasSubsequence(notsub) should equal(false)
  }
}
