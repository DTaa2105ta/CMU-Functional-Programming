fun member(x : int, [] : int list) : bool = false
  | member(x, y::ys) = (x = y) orelse member(x, ys)
(* Test for member(x, l) *)
val test1 = member(1, [1, 2, 3]) = true
val test2 = member(4, [1, 2, 3]) = false
val test3 = member(1, [1, 1, 1]) = true
val test4 = member(0, [~1, ~2, ~3, 0]) = true

(*remove (x, l) returns the list l' obtained by removing the first occurrence of x from l *)
(*
remove(x, l) == > l'
REQUIRES: member(x, l) = true
ENSURES: l' is obtained by removing the first occurrence of x from l 
*)
fun remove (x : int, y::ys : int list) : int list = 
    case (x=y)
      of true => ys
       | false => y::remove(x, ys)
(* Test for remove(x, l) *)
val test5 = remove(1, [1, 2, 3]) = [2, 3]
val test6 = remove(3, [1, 2, 3]) = [1, 2]
val test7 = remove(1, [1, 1, 1]) = [1, 1]
val test8 = remove(0, [~1, 0, ~3, 0]) = [~1, ~3, 0]
val test9 = remove(1, [2, 3, 4, 1, 1, 5]) = [2, 3, 4, 1, 5]
val test10 = remove(1, [1]) = []


fun isPermutation ([] : int list, [] : int list) : bool = true
  | isPermutation ([], _) = false
  | isPermutation (_, []) = false
  | isPermutation (x::xs, l2) = if member(x, l2) then isPermutation(xs, remove(x, l2)) else false 
(* Test for isPermutation(l1, l2) *)
val test11 = isPermutation([1, 2, 3], [3, 2, 1]) = true
val test12 = isPermutation([1, 2, 3], [3, 2, 2]) = false
val test13 = isPermutation([2, 2, 3], [3, 2]) = false
val test14 = isPermutation([1, 2, 3], [1, 2, 3, 1]) = false
val test15 = isPermutation([2, 2, 2], [2, 2, 2]) = true
val test16 = isPermutation([1, 2, 3, 1, 2, 1], [1, 1, 2, 1, 2, 3]) = true