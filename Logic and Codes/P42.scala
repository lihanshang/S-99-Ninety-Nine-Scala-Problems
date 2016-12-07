import java.lang
import java.lang.System

/*
P46 (**) Truth tables for logical expressions.
Define functions and, or, nand, nor, xor, impl, and equ (for logical equivalence) which return true or false according to the result of their respective operations; e.g. and(A, B) is true if and only if both A and B are true.
scala> and(true, true)
res0: Boolean = true

scala> xor(true. true)
res1: Boolean = false
A logical expression in two variables can then be written as an function of two variables, e.g: (a: Boolean, b: Boolean) => and(or(a, b), nand(a, b))

Now, write a function called table2 which prints the truth table of a given logical expression in two variables.

scala> table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
A     B     result
true  true  true
true  false true
false true  false
false false false
*/

object Solution {


    val primes = 2 #:: Stream.from(3, 2).filter(isPrime)
    def isPrime(x: Int): Boolean = primes.takeWhile(c => c * c <= x).forall(x % _ != 0)


    def goldbach(x: Int): (Int, Int) = primes.takeWhile(_ < x).find(c => isPrime(x - c)) match {
            case None => throw new IllegalArgumentException
            case Some(p) => (p, x - p)
        }

    def printGoldbachList(x: Int, y: Int) = {
        (x to y).filter(c => c % 2 == 0).map(c =>(c,goldbach(c))).foreach(_ match{case (c, (p1, p2)) => println(c + " = " + p1 + " + " + p2) })
    }


    def main(args: Array[String]): Unit = {
        print(printGoldbachList(9, 20))
    }
}
/*
def find(p: (A) ⇒ Boolean): Option[A]
Finds the first element of the sequence satisfying a predicate, if any.
 */