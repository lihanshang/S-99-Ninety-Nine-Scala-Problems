/*
P08 (**) Eliminate consecutive duplicates of list elements.
If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
Example:

scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
*/
object Solution {

    // Standard recursive.
    def compressRecursive[A](ls: List[A]): List[A] = ls match {
        case Nil => Nil
        case h :: tail => h :: compressRecursive(tail.dropWhile(_ == h))
    }

    def compressRecursive_pack[A](ls: List[A]): List[A] = {
        if(ls.isEmpty) ls
        else{
            val (packed, next) = ls span(_ == ls.head)
            if(next == Nil) List(packed.head)
            else packed.head :: compressRecursive_pack(next)
        }
    }
    
    // Tail recursive.
    def compressTailRecursive[A](ls: List[A]): List[A] = {
        def compressR(result: List[A], curList: List[A]): List[A] = curList match {
            case h :: tail => compressR(h :: result, tail.dropWhile(_ == h))
            case Nil       => result.reverse
        }
        compressR(Nil, ls)
    }

    // Functional.

    def compressFunctional[A](ls: List[A]): List[A] =
        ls.foldRight(List[A]()) { (h, r) =>
        if (r.isEmpty || r.head != h) h :: r
        else r
    }

    // You can not add an element on the current list(immutable), however, you can add one to the head to build a new list
    def compressFunctionalL[A](ls: List[A]): List[A] = {
        var t = ls.foldLeft(List[A]()) { (r, h) =>
            if (r.isEmpty || r.head != h) h :: r
            else r
        }
        t.reverse
    }

}
/*
def foldRight[B](z: B)(op: (A, B) ⇒ B): B
Applies a binary operator to all elements of this list and a start value, going right to left.
 */
