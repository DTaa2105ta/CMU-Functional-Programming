fun intToString (x: int): string = Int.toString x

(* Example *)
(* factorial n ==> r
 * REQUIRES: n >= 1
 * ENSURES:  r is the product of all integers between 1 and n
 *)
fun factorial (1: int): int = 1
  |  factorial n = n * factorial (n-1)

(* summorial n ==> r
 * REQUIRES: n >= 0
 * ENSURES:  r is the sum of all integers between 0 and n
 *)
fun summorial (0: int): int = raise Fail "Implement me"
  | summorial n = raise Fail "Implement me"

(* summorial' (n, a) ==> r
 * REQUIRES: n >= 0
 * ENSURES:  FILL ME IN
 *)
fun summorial' (0: int, a: int): int = a
  | summorial' (n, a) = summorial' ((n-1), n+a)
