/*
P31 (**) Determine whether a given integer number is prime.
scala> 7.isPrime
res0: Boolean = true
*/

object Solution {

    lazy val primes = 2 #:: Stream.from(3, 2).filter(isPrime)
    def isPrime(n: Int): Boolean = primes.takeWhile(p => p * p <= n).forall(n % _ != 0)

}
/*
def takeWhile(p: (A) ⇒ Boolean): Stream[A]
Returns the longest prefix of this Stream whose elements satisfy the predicate p.

def from(start: Int, step: Int): Stream[Int]
Create an infinite stream starting at start and incrementing by step step.
 */
