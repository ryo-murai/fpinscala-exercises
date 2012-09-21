package fpinscala.answers

// hide Scala's existing type
import scala.{Stream => _}
import scala.{Option=>_, Some=>_, None=>_}

trait Stream[A] {
  def uncons: Option[(A, Stream[A])]

  /**
    * Excercise 5.3
    */
  def toList: List[A] =
    uncons match {
      case None => List()
      case Some((head, tail)) => head :: tail.toList
    }

  /**
    * Excercise 5.3
    */
  def take(n: Int): Stream[A] =
    uncons match {
      case Some((head, tail)) if (n>0) => Stream.cons(head, tail.take(n-1))
      case _ => Stream.empty
    }

  /**
    * Excercise 5.3
    */
  def takeWhile(f: A => Boolean): Stream[A] =
    uncons match {
      case Some((head, tail)) if (f(head)) => Stream.cons(head, tail.takeWhile(f)) 
      case _ => Stream.empty
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons.
      map { case (h,t) => f(h, t.foldRight(z)(f)) }.
      getOrElse(z)

  def exists(f: A => Boolean): Boolean =
    foldRight(false)((h,t) => f(h) || t)

  /**
    * Excercise 5.3.1
    */
  def forall(f: A => Boolean): Boolean =
    foldRight(true)((h,t) => f(h) && t)

  /**
    * Excercise 5.3.1
    *
    * Exercise: For functions that return a Stream, we can also use laziness to
    * construct the result incrementally, and only if the values are demanded. Use
    * foldRight to implement takeWhile.
    */
  def takeWhile2(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h,t) => if(f(h)) Stream.cons(h, t) else Stream.empty[A])

  /**
    * Excercise 5.3.1
    * implement using foldRight
    */
  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((h,t) => Stream.cons(f(h), t))

  /**
    * Excercise 5.3.1
    * implement using foldRight
    */
  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h,t) => if(f(h)) Stream.cons(h, t) else t)

  /**
    * Excercise 5.3.1
    * implement using foldRight
    */
  def ++(s2: Stream[A]): Stream[A] =
    foldRight(s2)((h,t) => Stream.cons(h, t))

  /**
    * Excercise 5.3.1
    * implement using foldRight
    */
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((h,t) => f(h) ++ t )

  /**
    * Excercise 5.3.2
    * implement using unfold
    */
  def map2[B](f: A => B): Stream[B] =
    Stream.unfold(uncons)(_.map(x => Some((f(x._1), x._2.uncons))).getOrElse(None))

  /**
    * Excercise 5.3.2
    * implement using unfold
    */
  def take2(n: Int): Stream[A] =
    Stream.unfold((uncons,n))(x => x._1 match {case Some((h,t)) if x._2 > 0 => Some(h, (t.uncons,x._2 - 1)); case _ => None})

  /**
    * Excercise 5.3.2
    * implement using unfold
    */
  def takeWhile3(f: A => Boolean): Stream[A] =
    Stream.unfold(uncons)(_ match {case Some((h,t)) if(f(h)) => Some(h, t.uncons); case _ => None})

  /**
    * Excercise 5.3.2
    * implement using unfold
    */
  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    Stream.unfold((uncons,s2.uncons))(x => x._1.map(a => x._2.map(b => ((a._1,b._1), (a._2.uncons,b._2.uncons))))
        .getOrElse(None))

  /**
    * Excercise 5.3.2
    * implement using unfold
    */
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    Stream.unfold((uncons,s2.uncons))(_ match {
      case (Some((h,t)), Some((h2,t2))) => Some((Some(h),Some(h2)), (t.uncons,t2.uncons))
      case (Some((h,t)), None)          => Some((Some(h),None),     (t.uncons,None))
      case (None,        Some((h2,t2))) => Some((None,   Some(h2)), (None,    t2.uncons))
      case _ => None
    })

  /**
    * Excercise 5.3.2
    * implement using existing functions I've written
    */
  def startsWith(s: Stream[A]): Boolean =
    zipAll(s).forall(_ match {
      case (Some(a), Some(b)) => a == b
      case (Some(a), None) => true
      case _ => false
    })

  /**
    * Excercise 5.3.2
    * implement using unfold
    */
  def tails: Stream[Stream[A]] =
    Stream.unfold(Option(this))(_.map(s => (s, s.uncons.map(_._2))))

  /**
    * Excercise 5.3.2
    * can it be implemented using unfold ? Why or why not ?
    * could it be implemented using another function I've written ?
    */
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    // implemented using another functions
    tails.map(_.foldRight(z)(f))

  /**
    * Excercise 5.3.2
    * implement using unfold
    */
  def hasSubsequence(s: Stream[A]): Boolean =
    tails exists (_ startsWith s)
}

object Stream {
  def empty[A]: Stream[A] =
    new Stream[A] {
      def uncons = None
    }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val uncons = Some((hd, tl))
    }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  /**
    * Excercise 5.3.2
    */
  def constant[A](a: A): Stream[A] = {
    lazy val const: Stream[A] = cons(a, const)
    const
  }

  /**
    * Excercise 5.3.2
    */
  def from(i: Int): Stream[Int] =
    cons(i, from(i+1))

  /**
    * Excercise 5.3.2
    *
    * Exercise: write a function, fibs, that generates the infinite stream of Fibonacci
    * numbers: 0,1,1,2,3,5,8... Can fibs be defined in terms of a more general function?
    */
  def fibs: Stream[Int] = {
    // dont know if this is enough general function...
    def ifib(i0: Int, i1: => Int): Stream[Int] = {
      Stream.cons(i0, ifib(i1, i0+i1))
    }

    ifib(0, 1)
  }

  /**
    * Excercise 5.3.2
    */
  def unfold[A,S](s: S)(f: S => Option[(A,S)]): Stream[A] = {
    lazy val strm: Stream[A] = 
      f(s)
        .map { case (a0, s0) => Stream.cons(a0, unfold(s0)(f)) }
        .getOrElse(Stream.empty[A])

    strm
  }
}