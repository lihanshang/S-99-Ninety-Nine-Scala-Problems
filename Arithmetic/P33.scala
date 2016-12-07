/*
P33 (*) Determine whether two positive integer numbers are coprime.
Two numbers are coprime if their greatest common divisor equals 1.
scala> 35.isCoprimeTo(64)
res0: Boolean = true
*/

object Solution {

    def gcd(x: Int, y: Int): Int = {
        if(y == 0) x else gcd(y, x % y)
    }
    class S99Int(val start: Int){
        def isCoprime(x: Int) = gcd(start, x) == 1
    }

}
