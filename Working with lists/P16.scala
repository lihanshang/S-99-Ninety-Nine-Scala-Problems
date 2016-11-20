/*
P16 (**) Drop every Nth element from a list.
Example:
scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
*/

object Solution {

    // Simple recursion.
    def dropRecursive[A](n: Int, ls: List[A]): List[A] = {
        def dropR(c: Int, curList: List[A]): List[A] = (c, curList) match {
            case (_, Nil)       => Nil
            case (1, _ :: tail) => dropR(n, tail)
            case (_, h :: tail) => h :: dropR(c - 1, tail)
        }
        dropR(n, ls)
    }

    // Tail recursive.
    def dropTailRecursive[A](n: Int, ls: List[A]): List[A] = {
        def dropR(c: Int, curList: List[A], result: List[A]): List[A] = (c, curList) match {
            case (_, Nil)       => result.reverse
            case (1, _ :: tail) => dropR(n, tail, result)
            case (_, h :: tail) => dropR(c - 1, tail, h :: result)
        }
        dropR(n, ls, Nil)
    }

    // Functional.
    def dropFunctional[A](n: Int, ls: List[A]): List[A] =
    ls.zipWithIndex filter { v => (v._2 + 1) % n != 0 } map { _._1 }

}
/*
def zipWithIndex: List[(A, Int)]
[use case]
Zips this list with its indices.
returns
A new list containing pairs consisting of all elements of this list paired with their index. Indices start at 0.
 */
/*
def filter(p: (A) ⇒ Boolean): List[A]
Selects all elements of this traversable collection which satisfy a predicate.
 */
