use "factorize.sml";
use "factorization.sml";

(* 
  valid(l) => bool
  REQUIRES: l : int list
  ENSURES: returns true if the product of the elements in l is equal to the product of the prime factors of the product of l
           returns false otherwise
*)
fun tmult([] : int list, acc : int) : int = acc
  | tmult(x::xs, acc) = tmult(xs, x * acc)
fun mult(l : int list) : int = tmult(l, 1)

fun valid ([] : int list) : bool = false
  | valid ([1]) = false
  | valid (l) = isPermutation(l, factorize (mult l))

(* Basic Edge Cases *)
val test1 = valid([]) = false             (* Empty list *)
val test2 = valid([1]) = false            (* Single non-prime *)
val test3 = valid([2]) = true             (* Single prime *)
val fag1 = valid([1,1,1]) = false
val fag2 = valid([1,1,3]) = false
(* Complex Prime Factorizations *)
val test4 = valid([2,2,2,2,2,2,2,2]) = true    (* 2^8 = 256 *)
val test5 = valid([2,2,3,3,5,5,7,7]) = true    (* Multiple pairs of primes *)
val test6 = valid([2,3,5,7,11,13,17,19]) = true (* Product of first 8 primes *)

(* Invalid Orderings and Numbers *)
val test7 = valid([4,3,2]) = false        (* Contains non-prime *)
val test8 = valid([2,2,4]) = false        (* Mixed prime and non-prime *)
val test9 = valid([1,2,3]) = false        (* Contains 1 *)


val test11 = valid([97,89,83,79]) = true         (* Product of large primes *)

(* Tricky Cases *)
val test12 = valid([2,3,2,3,2,3]) = true  (* Mixed order but valid *)
val test13 = valid([3,3,3,2,2,2]) = true  (* Unsorted but valid *)

(* Edge Cases with Repeated Primes *)
val test15 = valid([2,2,2,3,3,3]) = true  (* Equal powers of different primes *)
val test16 = valid([7,7,7,7,7]) = true        (* High power of single prime *)

(* Invalid but Tricky Cases *)
val test17 = valid([2,3,6]) = false           (* Sum equals product *)
val test18 = valid([2,3,5,30]) = false        (* Contains their product *)
val test19 = valid([2,4,8,16]) = false        (* Powers of 2, but includes non-primes *)

(* Mixed Complex Cases *)
val test24 = valid([2,3,5,2,3,5]) = true (* Repeated pattern *)