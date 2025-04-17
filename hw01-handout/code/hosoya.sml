(* (n, k) => H(n, k)
 * REQUIRES: n >= k >= 0 
 * ENSURES:  H(n, k) = Fib(k) x Fib(n - k)
 *) 
(* fun hosoya _ = raise Fail "Unimplemented" *)

fun hosoya (0 : int,0 : int): int = 1
  | hosoya (1, 0) = 1
  | hosoya (1, 1) = 1
  | hosoya (2, 1) = 1
  | hosoya (n, k) = 
    if (n >= (k + 2)) then (hosoya (n-1, k) + hosoya (n-2, k))
    else hosoya(n-2, k-2) + hosoya(n-1, k-1);

(* Testing *)
val 1 = hosoya(0, 0);
val 2 = hosoya(2, 0);
val 8 = hosoya(6, 5);
val 13 = hosoya(6, 0);
val 13 = hosoya(6, 6);
val 9 = hosoya(6, 3);