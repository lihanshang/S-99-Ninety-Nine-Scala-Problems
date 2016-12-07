/*
P39 (*) A list of prime numbers.
Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
scala> listPrimesinRange(7 to 31)
res0: List[Int] = List(7, 11, 13, 17, 19, 23, 29, 31)
*/

object Solution {


    val primes = 2 #:: Stream.from(3, 2).filter(isPrime)
    def isPrime(x: Int): Boolean = primes.takeWhile(c => c * c <= x).forall(x % _ != 0)
    def listPrimesinRange(x: Int, y: Int): List[Int] = {
        primes.filter(c => c > x).takeWhile(c => c <= y).toList
        //primes.dropWhile(c => c > x).takeWhile(c => c <= y).toList
    }

}
/*
def dropWhile(p: (A) ⇒ Boolean): Stream[A]
Returns the a Stream representing the longest suffix of this iterable whose first element does not satisfy the predicate p.
 */
