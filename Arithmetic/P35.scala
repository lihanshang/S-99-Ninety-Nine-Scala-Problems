/*
P35 (**) Determine the prime factors of a given positive integer.
Construct a flat list containing the prime factors in ascending order.
scala> 315.primeFactors
res0: List[Int] = List(3, 3, 5, 7)
*/

object Solution {

    val primes = 2 #:: Stream.from(3, 2).filter(isPrime)
    def isPrime(x: Int): Boolean = primes.takeWhile(c => c * c <= x).forall(x % _ != 0)
    def primeFactors(n: Int): List[Int] = {
        def primeFactorsR(n: Int, ps: Stream[Int]): List[Int] = {
            if (isPrime(n)) List(n)
            else if (n % ps.head == 0) ps.head :: primeFactorsR(n / ps.head, ps)
            else primeFactorsR(n, ps.tail)
        }
        primeFactorsR(n, primes)
    }

}
/*
def takeWhile(p: (A) ⇒ Boolean): Stream[A]
Returns the longest prefix of this Stream whose elements satisfy the predicate p.

def forall(p: (A) ⇒ Boolean): Boolean
Tests whether a predicate holds for all elements of this sequence.
 */
