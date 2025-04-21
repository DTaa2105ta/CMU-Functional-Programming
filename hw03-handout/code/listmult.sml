
fun mult([] : int list) : int = 1
  | mult(x::xs) = x * mult(xs)
fun tmult([] : int list, acc : int) : int = acc
  | tmult(x::xs, acc) = tmult(xs, x * acc)
fun multTail(l) = tmult(l, 1)

(* Test cases for multTail *)
(*
val test1 = multTail([1, 2, 3]) = 6
val test2 = multTail([1, 2, 3, 4]) = 24
val test3 = multTail([1, 2, 0, 4, 5]) = 0
val test4 = multTail([1, 2, 3, 4, 5]) = mult([1, 2, 3, 4, 5])
*)