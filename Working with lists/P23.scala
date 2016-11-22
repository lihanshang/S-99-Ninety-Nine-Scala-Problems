/*
P23 (**) Extract a given number of randomly selected elements from a list.
Example:
scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
res0: List[Symbol] = List('e, 'd, 'a)
*/

object Solution {

    def randomSelect2[A](n: Int, ls: List[A]): List[A] =
        if(n <= 0) Nil
        else{
            val e = (new util.Random).nextInt(ls.length)
            val l = ls.take(e) ::: ls.takeRight(ls.length - e - 1)
            ls(e) :: randomSelect2(n - 1, l)
        }
        
    def randomSelect1[A](n: Int, ls: List[A]): List[A] =
        if (n <= 0) Nil
        else {
            val (rest, e) = removeAt((new util.Random).nextInt(ls.length), ls)
            e :: randomSelect1(n - 1, rest)
        }

    // It can be expensive to create a new Random instance every time, so let's
    // only do it once.
    def randomSelect[A](n: Int, ls: List[A]): List[A] = {
        def randomSelectR(n: Int, ls: List[A], r: util.Random): List[A] =
            if (n <= 0) Nil
            else {
                val (rest, e) = removeAt(r.nextInt(ls.length), ls)
                e :: randomSelectR(n - 1, rest, r)
            }
        randomSelectR(n, ls, new util.Random)
    }

}
/*
def nextInt(): Int
Returns the next pseudorandom, uniformly distributed int value from this random number generator's sequence.

def nextInt(n: Int): Int
Returns a pseudorandom, uniformly distributed int value between 0 (inclusive) and the specified value (exclusive), drawn from this random number generator's sequence.
 */
