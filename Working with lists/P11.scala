/*
P11 (*) Modified run-length encoding.
Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
Example:

scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
*/
object Solution {


    def encode_a[A](ls: List[A]): List[Any] = {
        if(ls.isEmpty) List()
        else{
            val (packed, next) = ls span(_ == ls.head)
            if(next == Nil){
                if(packed.length != 1)
                    List((packed.length, packed.head))
                else List(packed.head)
            }
            else {
                if(packed.length != 1)
                    (packed.length, packed.head) :: encode_a(next)
                else packed.head :: encode_a(next)
            }
        }
    }

    def pack[A](ls: List[A]): List[List[A]] = {
        if(ls.isEmpty) List(ls)
        else{
            val (packed, next) = ls span(_ == ls.head)
            if(next == Nil) List(packed)
            else packed :: pack(next)
        }
    }

    def encode[A](ls: List[A]): List[(Int, A)] =
        pack(ls) map { e => (e.length, e.head) }

    def encodeModified[A](ls: List[A]): List[Any] =
        encode(ls) map { t => if (t._1 == 1) t._2 else t }

    // Just for fun, here's a more typesafe version.
    def encodeModified2[A](ls: List[A]): List[Either[A, (Int, A)]] =
    encode(ls) map { t => if (t._1 == 1) Left(t._2) else Right(t) }

}
/*
final case class Left[+A, +B](value: A) extends Either[A, B] with Product with Serializable
 */
