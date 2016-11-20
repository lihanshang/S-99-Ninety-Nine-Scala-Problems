/*
P07 (**) Flatten a nested list structure.
Example:
scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
res0: List[Any] = List(1, 1, 2, 3, 5, 8)
*/
object Solution {

    def flatten(ls: List[Any]): List[Any] = ls flatMap {
        case ms: List[_] => flatten(ms)
        case e => List(e)
    }
}
/*
final def flatMap[B](f: (A) ⇒ GenTraversableOnce[B]): List[B]
[use case] Builds a new collection by applying a function to all elements of this list and using the elements of the resulting collections.
 */
