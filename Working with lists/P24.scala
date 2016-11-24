/*
P24 (*) Lotto: Draw N different random numbers from the set 1..M.
Example:
scala> lotto(6, 49)
res0: List[Int] = List(23, 1, 17, 33, 21, 37)
*/

object Solution {

    def randomSelect(n: Int, ls: List[Int]): List[Int] = {
        if(n <= 0) Nil
        else{
            val e = (new util.Random).nextInt(ls.length)
            val l = ls.take(e) ::: ls.takeRight(ls.length - e - 1)
            ls(e) :: randomSelect(n - 1, l)
        }
    }

    def lotto(count: Int, max: Int): List[Int] = randomSelect(count, List.range(1, max + 1))

}
