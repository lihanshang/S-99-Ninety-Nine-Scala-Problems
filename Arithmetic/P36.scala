/*
P36 (**) Determine the prime factors of a given positive integer (2).
Construct a list containing the prime factors and their multiplicity.
scala> 315.primeFactorMultiplicity
res0: List[(Int, Int)] = List((3,2), (5,1), (7,1))
Alternately, use a Map for the result.

scala> 315.primeFactorMultiplicity
res0: Map[Int,Int] = Map(3 -> 2, 5 -> 1, 7 -> 1)
*/

object Solution {


    def pack(ls: List[Int]): List[List[Int]] = {
        if(ls.isEmpty) List(List())
        else{
            val (packed, next) = ls span(_ == ls.head)
            if (next == Nil) List(packed)
            else packed :: pack(next)
        }
    }
    def encode(ls: List[List[Int]]): List[(Int, Int)] = ls.map(x => (x.head, x.length))
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
    def primeFactorMultiplicity(x: Int): List[(Int, Int)] ={
        encode(pack(primeFactors(x)))
    }

}
