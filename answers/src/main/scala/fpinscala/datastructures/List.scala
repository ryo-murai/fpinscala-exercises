package fpinscala.datastructures

sealed trait List[+A] // `List` data type
case object Nil extends List[Nothing] // data constructor for `List`
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object
  def sum(ints: List[Int]): Int = ints match { // Pattern matching example
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  
  val example = Cons(1, Cons(2, Cons(3, Nil))) // Creating lists
  val example2 = List(1,2,3)
  val total = sum(example)

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](list: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    list match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ints: List[Int]) = 
    foldRight(ints, 0.0)(_ + _)
  
  def product2(ints: List[Double]) = 
    foldRight(ints, 1.0)(_ * _)


  /* 
  3. The third case is the first that matches, with `x` bound to 1 and `y` bound to 2. 
  */

  /*
  Although we could return `Nil` when the input list is empty, we choose to throw an exception instead. This is a somewhat subjective choice. In our experience taking the tail of an empty list is often a bug, and silently returning a value just means this bug will be discovered later, further from the place where it was introduced. 
  
  It's generally good practice when pattern matching to use '_' for any variables you don't intend to use on the right hand side of a pattern. It makes it clear the value isn't relevant.  
  */
  def tail[A](l: List[A]): List[A] = 
    l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_,t) => t
    }

  /* 
  Again, it is somewhat subjective whether to throw an exception when asked to drop more elements than the list contains. The usual default for `drop` is not to throw an exception, since it is typically used in cases where this is not indicative of a programming error. If you pay attention to how you use `drop`, it is often in cases where the length of the input list is unknown, and the number of elements to be dropped is being computed from something else. If `drop` threw an exception, we'd have to first compute or check the length and only drop up to that many elements.  
  */
  def drop[A](l: List[A])(n: Int): List[A] = 
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t)(n-1) 
    }

  /* 
  Somewhat overkill, but to illustrate the feature we are using a _pattern guard_, to only match a `Cons` whose head satisfies our predicate, `f`. The syntax is simply to add `if <cond>` after the pattern, before the `=>`, where `<cond>` can use any of the variables introduced by the pattern.
  */
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = 
    l match {
      case Cons(h,t) if f(h) => dropWhile(t)(f) 
      case _ => l
    }

  /*
  If a function body consists solely of a match expression, we'll often put the 
  match on the same line as the function signature, rather than introducing another
  level of nesting.
  */
  def setHead[A](l: List[A])(h: A): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_,t) => Cons(h,t)
  }

  /*
  Notice we are copying the entire list up until the last element. Besides being inefficient, the natural recursive solution will stack overflow for large lists (can you see why?). With strict lists, it's more typical to use a temporary, mutable buffer internal to the function. So long as the buffer is allocated internal to the function, the mutation is not observable and RT is preserved.
  */
  def init[A](l: List[A]): List[A] = 
    l match { 
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }
  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => List(buf.toList: _*)
      case Cons(h,t) => buf += h; go(t)
    }
    go(l)
  }

  /* 
  Notice that we traverse all the way to the end of the list (pushing frames onto the call stack as we go) before we can begin collapsing it.
  
  foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)(_ + _)
  1 + foldRight(Cons(2, Cons(3, Nil)), 0)(_ + _)
  1 + (2 + foldRight(Cons(3, Nil), 0)(_ + _))
  1 + (2 + (3 + (foldRight(Nil, 0)(_ + _))))
  1 + (2 + (3 + (0)))
  6
  */

  /* 
  No, this is not possible! The reason is that _before_ we ever call our function, `f`, we evaluate its argument, which in the case of `foldRight` means traversing the list all the way to the end. We need _non-strict_ evaluation to support early termination---we discuss this in a later chapter.  
  */

  /* 
  We get back the original list! Why is that? One way of thinking about what `foldRight` "does" is it replaces the `Nil` constructor of the list with the `z` argument, and it replaces the `Cons` constructor with the given function, `f`. If we just supply `Nil` for `z` and `Cons` for `f`, then we get back the input list. 
  
  foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil:List[Int])(Cons(_,_))
  Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil:List[Int])(Cons(_,_)))
  Cons(1, Cons(2, foldRight(Cons(3, Nil), Nil:List[Int])(Cons(_,_))))
  Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil:List[Int])(Cons(_,_)))))
  Cons(1, Cons(2, Cons(3, Nil))) 
  */

  def length[A](l: List[A]): Int = 
    foldRight(l, 0)((_,acc) => acc + 1)

  /* 
  It's common practice to annotate functions you expect to be tail-recursive with the `tailrec` annotation. If the function is not tail-recursive, it will yield a compile error, rather than silently compiling the code and resulting in greater stack space usage at runtime. 
  
  Aside: we usually use one-letter variable names when everything there is to say about a value is implied by its type. Since functions are usually quite short in FP, many functional programmers feel this makes the code easier to read, since it makes the structure of the code easier to see. 
  */
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match { 
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc,h) => acc + 1)

  def reverse[A](list: List[A]): List[A] = foldLeft(list, List[A]())((acc,h) => Cons(h,acc))

  /*
  The implementation of `foldRight` in terms of `reverse` and `foldLeft` is a common trick for avoiding stack overflows when implementing a strict `foldRight` function as we've done in this chapter. (We will revisit this in a later chapter, when we discuss laziness).
  
  The other implementations build up a chain of functions which, when called, results in the operations being performed with the correct associativity.
  */
  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B = 
    foldLeft(reverse(l), z)((b,a) => f(a,b))
  
  def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B = 
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)
  
  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B = 
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  /*
  `append` simply replaces the `Nil` constructor of the first list with the second list, which is exactly the operation performed by `foldRight`.
  */
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = 
    foldRight(l, r)(Cons(_,_))

  /*
  Since `append` takes time proportional to its first argument, and this first argument never grows because of the right-associativity of `foldRight`, this function is linear in the total length of all lists. You may want to try tracing the execution of the implementation on paper to convince yourself that this works. 
  
  Notice we are simply referencing the `append` function, without writing something like `(x,y) => append(x,y)` or `append(_,_)`. In Scala there is a rather arbitrary distinction between functions defined as _methods_, which are introduced with the `def` keyword, and function values, which are the first-class objects we can pass to other functions, put in collections, and so on. This is a case where Scala lets us pretend the distinction doesn't exist. In other cases, you'll be forced to write `append _` (to convert a `def` to a function value) or even `(x: List[A], y: List[A]) => append(x,y)` if the function is polymorphic and the type arguments are not known. 
  */
  def concat[A](l: List[List[A]]): List[A] = 
    foldRight(l, Nil:List[A])(append)

  def add1(l: List[Int]): List[Int] = 
    foldRight(l, Nil:List[Int])((h,t) => Cons(h+1,t))

  def doubleToString(l: List[Double]): List[String] = 
    foldRight(l, Nil:List[String])((h,t) => Cons(h.toString,t))

  /* 
  A natural solution is using `foldRight`, however this will stack overflow for large lists. We can use `foldRightViaFoldLeft` to avoid the stack overflow (variation 1), but more commonly, with our current implementation of `List`, `map` will just be implemented using local mutation (variation 2). Again, notice that the mutation is not observable outside the function, since we are only mutating a buffer that we have allocated. 
  */
  def map[A,B](l: List[A])(f: A => B): List[B] = 
    foldRight(l, Nil:List[B])((h,t) => Cons(f(h),t))
  
  def map_1[A,B](l: List[A])(f: A => B): List[B] = 
    foldRightViaFoldLeft(l, Nil:List[B])((h,t) => Cons(f(h),t))
  
  def map_2[A,B](l: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h,t) => buf += f(h); go(t)
    }
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }

  /* 
  The discussion about `map` also applies here.
  */
  def filter[A](l: List[A])(f: A => Boolean): List[A] = 
    foldRight(l, Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t)
  
  def filter_1[A](l: List[A])(f: A => Boolean): List[A] = 
    foldRightViaFoldLeft(l, Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t)
  
  def filter_2[A](l: List[A])(f: A => Boolean): List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h,t) => if (f(h)) buf += h; go(t)
    }
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }

  /* 
  This could also be implemented directly using `foldRight`.
  */
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = 
    concat(map(l)(f))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  /* 
  To match on multiple values, we can put the values into a pair and match on the pair, as shown below, and the same syntax extends to matching on N values (see sidebar "Pairs and tuples in Scala" for more about pair and tuple objects). You can also (somewhat less conveniently, but a bit more efficient) nest pattern matches: on the right hand side of the `=>`, simply begin another `match` expression. The inner `match` will have access to all the variables introduced in the outer `match`. 
  
  The discussion about stack usage from the explanation of `map` also applies here.
  */
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))
  }

  /* 
  This function is usually called `zipWith`. The discussion about stack usage from the explanation of `map` also applies here. By putting the `f` in the second argument list, Scala can infer its type from the previous argument list. 
  */
  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }

  /*
  There's nothing particularly bad about this implementation, except that it's somewhat monolithic and easy to get wrong. (Without pattern matching and pattern guards, the implementation would be even more finnicky.) Where possible, we prefer to assemble functions like this using combinations of other functions. It makes the code more obviously correct and easier to read and understand. Notice that in this implementation we need special purpose logic to break out of our loops early. In Chapter 5 we'll discuss ways of composing functions like this from simpler components, without giving up the efficiency of having the resulting functions work in one pass over the data.
  */
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Nil,_) => false
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    def go[A](l: List[A]): Boolean = l match {
      case Nil => false
      case Cons(h,t) if startsWith(l, sub) => true
      case Cons(h,t) => go(t)  
    }
    go(l)
  }
}