use "/home/dtaa/CS/CMU/15150/Lab & HW/lab03-handout/code/is-permutation.sml";
(*
upto(n, N) => l
REQUIRES: n : int, N : int and n and N > 0
ENSURES: returns the lsit of all prime factors of n that are smaller than or equal to N
	     returns the empty list if n is 1
*)
fun oddupto (1 : int, odd : int, N : int) : int list = []
  | oddupto (_, _, 1) = []
  | oddupto (n, odd, N) = 
    if n mod odd = 0 then odd :: oddupto(n div odd, odd, N)
    else oddupto (n, odd+2, N-2)

fun upto' (1 : int, N : int) : int list  = []
  | upto' (_, 1) = []
  | upto' (n, N) = 
    if n mod 2 = 0 
      then 2 :: upto' (n div 2, N)
    else
      if (N mod 2 = 0) then oddupto(n, 3, N-1) else oddupto(n, 3, N)

(* Tail reccursion for upto() *)
fun t_oddupto (1 : int, odd : int, N : int, acc : int list) : int list = acc
  | t_oddupto (_, _, 1, acc) = acc
  | t_oddupto (n, odd, N, acc) =
    case n mod odd
      of 0 => t_oddupto (n div odd, odd, N, odd :: acc)
       | _ => t_oddupto (n, odd+2, N-2, acc)

fun t_upto (1 : int, N : int, acc : int list) : int list  = acc
  | t_upto (_, 1, acc) = acc
  | t_upto (n, N, acc) = 
   case (n mod 2, N mod 2)
     of (0, 0) => t_upto (n div 2, N, 2 :: acc)
      | (0, _) => t_upto (n div 2, N, 2 :: acc)
      | (_, 0) => t_oddupto (n, 3, N-1, acc)
      | (_, _) => t_oddupto (n, 3, N, acc)

fun upto(n : int, N : int) : int list = t_upto(n, N, [])

(* ================================================================================== *)
(* Test cases for upto *)
(* Base cases *)
(*
val test1 = upto(1, 10) = []  (* n = 1 should always return empty list *)
val test2 = upto(7, 1) = []   (* When N = 1, should return empty list *)

(* Edge cases with prime numbers *)
val test3 = upto(17, 16) = [] (* Prime number where N is less than the prime *)
val test4 = upto(23, 23) = [23] (* Prime number equal to N *)

(* Multiple of same prime factor *)
val test5 = upto(32, 5) = [2,2,2,2,2] (* Power of 2 *)
val test6 = upto(243, 5) = [3,3,3,3,3] (* Power of 3 *)

(* Complex numbers with multiple prime factors *)
val test7 = isPermutation(upto(2310, 11), [2,3,5,7,11]) (* Product of first 4 primes *)
val test8 = isPermutation(upto(1001, 13), [7,11,13]) (* Product of consecutive primes *)

(* Large numbers with restricted N *)
val test9 = upto(65536, 4) = [2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2] (* 2^16 *)
val test10 = upto(10007, 100) = [] (* Prime number larger than N *)

(* Numbers with mixed factors *)
val test11 = isPermutation(upto(2940, 7), [2,2,3,5,7,7]) (* Complex factorization with limit *)
val test12 = isPermutation(upto(9999, 11), [3,3,11]) (* Multiple occurrences of primes *)
val test12' = isPermutation(upto(9999, 9999), [3,3,11,101])
(* Stress test with large numbers *)
val test13 = isPermutation(upto(123456, 5), [2,2,2,2,2,2,3]) 
val test14 = isPermutation(upto(999999, 7), [3,3,3,7])

(* Edge cases with consecutive primes *)
val test15 = isPermutation(upto(30030, 13), [2,3,5,7,11,13]) (* Product of first 6 primes *)
val test16 = isPermutation(upto(510510, 11), [2,3,5,7,11]) (* Product of first 7 primes, limited *)

(* Tricky cases with almost-prime numbers *)
val test17 = upto(961, 40) = [31,31] (* Square of prime *)
val test18 = upto(841, 29) = [29,29] 
(* Performance test with highly composite numbers *)
val test19 = isPermutation(upto(7560, 11), [2,2,2,3,3,3,5,7]) (* Highly composite number *)
val test20 = isPermutation(upto(83160, 13), [2,2,2,3,3,3,5,7,11]) (* Large highly composite *)
*)
(* ================================================================================== *)

