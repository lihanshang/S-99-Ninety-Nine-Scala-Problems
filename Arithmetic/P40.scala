/*
P40 (**) Goldbach's conjecture.
Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. E.g. 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers (much larger than Scala's Int can represent). Write a function to find the two prime numbers that sum up to a given even integer.
scala> 28.goldbach
res0: (Int, Int) = (5,23)
*/

object Solution {


    val primes = 2 #:: Stream.from(3, 2).filter(isPrime)
    def isPrime(x: Int): Boolean = primes.takeWhile(c => c * c <= x).forall(x % _ != 0)


    def goldbach(x: Int): (Int, Int) = primes.takeWhile(_ < x).find(c => isPrime(x - c)) match {
            case None => throw new IllegalArgumentException
            case Some(p) => (p, x - p)
        }

}
/*
def find(p: (A) ⇒ Boolean): Option[A]
Finds the first element of the sequence satisfying a predicate, if any.
 */
