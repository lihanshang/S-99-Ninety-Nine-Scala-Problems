/*
P09 (**) Pack consecutive duplicates of list elements into sublists.
If a list contains repeated elements they should be placed in separate sublists.
Example:

scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
*/
object Solution {

    def pack[A](ls: List[A]): List[List[A]] = {
        if(ls.isEmpty) List(ls)
        else{
            val (packed, next) = ls span(_ == ls.head)
            if(next == Nil) List(packed)
            else packed :: pack(next)
        }
    }

}
/*
final def span(p: (A) ⇒ Boolean): (List[A], List[A])
Splits this list into a prefix/suffix pair according to a predicate.

Note: c span p is equivalent to (but possibly more efficient than) (c takeWhile p, c dropWhile p), provided the evaluation of the predicate p does not cause any side-effects.
 */
