/*
P04 (*) Find the number of elements of a list.
Example:
scala> length(List(1, 1, 2, 3, 5, 8))
res0: Int = 6
*/
object Solution {

    // Builtins.
    def lengthBuiltin[A](ls: List[A]): Int = ls.length

    // Simple recursive solution.
    def lengthRecursive[A](ls: List[A]): Int = ls match {
        case Nil => 0;
        case _ :: tail => lengthRecursive(tail) + 1
    }
    //Tail recursion usually takes less stacks
    def lengthTailRecursive[A](ls: List[A]): Int = {
        def lengthR(result: Int, curList: List[A]): Int = curList match{
            case Nil => result
            case _ :: tail => lengthR(result + 1, tail)
        }
        lengthR(0, ls)
    }

    // More pure functional solution, with folds.
    def lengthFunctional[A](ls: List[A]): Int = ls.foldLeft(0) { (c, _) => c + 1 }
}
/*
def foldLeft[B](z: B)(op: (B, A) ⇒ B): B
Applies a binary operator to a start value and all elements of this sequence, going left to right.
 */
