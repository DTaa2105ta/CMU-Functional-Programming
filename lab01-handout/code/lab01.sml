(*ource and freely distributable*)
(*~*)
(*~                           Sponsor Vim development!*)
(* ---------------------------------------------------------------------- *)
(* For Sec 7.1 *)
fun intToString (x : int) : string = Int.toString x

fun factorial (0: int) : int = 1
  | factorial n =  n * factorial(n - 1)
(* ---------------------------------------------------------------------- *)

(* ---------------------------------------------------------------------- *)

(* Code from class *)

(* sum : int list -> int *)
(* REQUIRES: true *)
(* ENSURES: sum(L) evaluates to the sum of the integers in L. *)
fun sum (L : int list): int =
  case L of
    [] => 0
  | x::xs => x + (sum xs)

(* count : int list list -> int *)
(* REQUIRES: true *)
(* ENSURES: count(R) evaluates to the sum of the integers in R. *)
fun count (L : int list list): int =
  case L of
    [] => 0
  | r::rs => (sum r) + (count rs)

(* Test cases *)
val test1 = sum [1, 2, 3] (* Expected output: 6 *)
val test2 = count [[1, 2], [3, 4]] (* Expected output: 10 *)
val test3 = intToString 123 (* Expected output: "123" *)

(* function 4 *)
(* summorial n ==> m
   REQUIRES: n >= 0
   ENSURES: m is the sum of all integers between 0 and n
*)
fun summorial (0 : int) : int = 0
  | summorial n = n + summorial(n - 1)

(* function 5 *)
(* summorial2 (n,a) ==> m
   REQUIRES: n >= 0
   ENSURES: result m is the sum of a and all integers from 0 to n
            (i.e., m = a + 0 + 1 + ... + n)
*)
fun summorial2 (0: int, a: int): int = a
  | summorial2 (n, a) = summorial2 ((n-1), n+a)


