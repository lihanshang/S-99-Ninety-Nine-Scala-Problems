/*
P34 (**) Calculate Euler's totient function phi(m).
Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r <= m) that are coprime to m.
scala> 10.totient
res0: Int = 4
*/

object Solution {


    def gcd(x: Int, y: Int): Int = {
        if(y == 0) x else gcd(y, x % y)
    }

    def isCoprime(x: Int, y: Int): Boolean = gcd(x, y) == 1

    def totient(x: Int) = (1 to x).filter(isCoprime(x, _)).length

}
