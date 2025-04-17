Control.Print.printLength := 100;
Control.Print.printDepth := 100;
(* ================================================================================== *)
(* lookAndSay.sml *)

(* Helper function to count consecutive elements in a list *)
fun lasHelp (l : int list, x : int, c : int) : (int list * int) = 
  case l
    of [] => (l, c)
     | (y::ys) => case y = x
                    of true => lasHelp (ys, x, c + 1)
                     | false => (l, c)
(*
val test1 = lasHelp ([1, 2, 3, 4, 4], 4, 1) = ([1, 2, 3, 4, 4], 1)
val test2 = lasHelp ([2, 2, 6, 2, 3], 2, 3) = ([6, 2, 3], 5)
*)

fun t_lookAndSay ([] : int list, c : int) : int list = []
  | t_lookAndSay (x :: xs, c) = 
    let
      val (l, x_count) = lasHelp (x::xs, x, c) 
    in
      (x_count :: x :: []) @ t_lookAndSay (l, c)
    end

fun lookAndSay (l : int list) : int list = t_lookAndSay(l, 0)
(*
(* Test cases for lookAndSay *)
val test1 = lookAndSay([1, 2, 3]) = [1, 1, 1, 2, 1, 3]
val test2 = lookAndSay([1, 1, 2, 2, 3]) = [2, 1, 2, 2, 1, 3]
val test3 = lookAndSay([1, 2, 2, 3]) = [1, 1, 2, 2, 1, 3]
val test4 = lookAndSay([2 ,2 ,2]) = [3 ,2]
val test5 = lookAndSay([1, 1, 1, 1]) = [4, 1]
*)
(*
(* Basic Edge Cases *)
val test1 = lookAndSay([]) = []  (* Empty list *)
val test2 = lookAndSay([1]) = [1, 1]  (* Single element *)
val test3 = lookAndSay([5, 5]) = [2, 5]  (* Two identical elements *)

(* Sequences with Multiple Groups *)
val test4 = lookAndSay([1, 1, 1, 2, 2, 3, 3, 3, 3]) = [3, 1, 2, 2, 4, 3]
val test5 = lookAndSay([9, 9, 9, 9, 9, 9, 9, 9, 9]) = [9, 9]  (* Long sequence of same digit *)
val test6 = lookAndSay([1, 2, 3, 4, 5, 5, 5, 4, 3, 2, 1]) = [1, 1, 1, 2, 1, 3, 1, 4, 3, 5, 1, 4, 1, 3, 1, 2, 1, 1]

(* Alternating Patterns *)
val test7 = lookAndSay([1, 2, 1, 2, 1, 2]) = [1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1, 2]
val test8 = lookAndSay([0, 1, 0, 1, 0, 1, 0]) = [1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0]

(* Edge Cases with Numbers *)
val test9 = lookAndSay([0, 0, 0]) = [3, 0]  (* Multiple zeros *)
val test10 = lookAndSay([~1, ~1, ~1, ~1]) = [4, ~1]  (* Negative numbers *)
val test11 = lookAndSay([999, 999, 999]) = [3, 999]  (* Large numbers *)

(* Complex Patterns *)
val test12 = lookAndSay([1, 1, 2, 2, 2, 1, 1, 1, 1, 2]) = [2, 1, 3, 2, 4, 1, 1, 2]
val test13 = lookAndSay([3, 3, 3, 3, 2, 2, 1, 1, 1]) = [4, 3, 2, 2, 3, 1]
val test14 = lookAndSay([1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4]) = [1, 1, 3, 2, 4, 3, 5, 4]

(* Mixed Sequences *)
val test17 = lookAndSay([7, 7, 7, 1, 2, 2, 2, 2, 3, 3]) = [3, 7, 1, 1, 4, 2, 2, 3]
val test18 = lookAndSay([0, 0, 1, 1, 2, 2, 3, 3, 3]) = [2, 0, 2, 1, 2, 2, 3, 3]

(* Boundary Tests *)
val test19 = lookAndSay([1, 1, 1, 1, 2]) = [4, 1, 1, 2]  (* Multiple at start *)
val test20 = lookAndSay([1, 2, 2, 2, 2]) = [1, 1, 4, 2]  (* Multiple at end *)
*)


fun t_lookAndSay' ([] : int list, c : int) : int list = []
  | t_lookAndSay' (x :: [], c) = c :: x :: []
  | t_lookAndSay' (x :: y :: ys, c) = 
    case x = y
      of true => t_lookAndSay' (y :: ys, c + 1)
       | false => (c :: x :: []) @ t_lookAndSay' (y :: ys, 1)


fun lookAndSay' (l : int list) : int list = t_lookAndSay' (l, 1)
val test0 = lookAndSay'([1, 1, 1, 2]) = [3, 1, 1, 2]  (* Basic case with multiple elements *)

(* Test cases for lookAndSay' *)
(* Basic Edge Cases *)
val test1 = lookAndSay'([]) = []  (* Empty list *)
val test2 = lookAndSay'([1]) = [1, 1]  (* Single element *)
val test3 = lookAndSay'([5, 5]) = [2, 5]  (* Two identical elements *)

(* Sequences with Multiple Groups *)
val test4 = lookAndSay'([1, 1, 1, 2, 2, 3, 3, 3, 3]) = [3, 1, 2, 2, 4, 3]
val test5 = lookAndSay'([9, 9, 9, 9, 9, 9, 9, 9, 9]) = [9, 9]  (* Long sequence of same digit *)
val test6 = lookAndSay'([1, 2, 3, 4, 5, 5, 5, 4, 3, 2, 1]) = [1, 1, 1, 2, 1, 3, 1, 4, 3, 5, 1, 4, 1, 3, 1, 2, 1, 1]

(* Alternating Patterns *)
val test7 = lookAndSay'([1, 2, 1, 2, 1, 2]) = [1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1, 2]
val test8 = lookAndSay'([0, 1, 0, 1, 0, 1, 0]) = [1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0]

(* Edge Cases with Numbers *)
val test9 = lookAndSay'([0, 0, 0]) = [3, 0]  (* Multiple zeros *)
val test10 = lookAndSay'([~1, ~1, ~1, ~1]) = [4, ~1]  (* Negative numbers *)
val test11 = lookAndSay'([999, 999, 999]) = [3, 999]  (* Large numbers *)

(* Complex Patterns *)
val test12 = lookAndSay'([1, 1, 2, 2, 2, 1, 1, 1, 1, 2]) = [2, 1, 3, 2, 4, 1, 1, 2]
val test13 = lookAndSay'([3, 3, 3, 3, 2, 2, 1, 1, 1]) = [4, 3, 2, 2, 3, 1]
val test14 = lookAndSay'([1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4]) = [1, 1, 3, 2, 4, 3, 5, 4]
val test17 = lookAndSay'([7, 7, 7, 1, 2, 2, 2, 2, 3, 3]) = [3, 7, 1, 1, 4, 2, 2, 3]
val test18 = lookAndSay'([0, 0, 1, 1, 2, 2, 3, 3, 3]) = [2, 0, 2, 1, 2, 2, 3, 3]

(* Boundary Tests *)
val test19 = lookAndSay'([1, 1, 1, 1, 2]) = [4, 1, 1, 2]  (* Multiple at start *)
val test20 = lookAndSay'([1, 2, 2, 2, 2]) = [1, 1, 4, 2]  (* Multiple at end *)
