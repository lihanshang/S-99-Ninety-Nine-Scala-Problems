/*
P32 (**) Determine the greatest common divisor of two positive integer numbers.
Use Euclid's algorithm.
scala> gcd(36, 63)
res0: Int = 9
*/

object Solution {


    def gcd(x: Int, y: Int): Int = {
        if(y == 0) x else gcd(y, x % y)
    }

}
