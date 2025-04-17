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
(*
factorize n -> l
REQUIRES: n : int, n > 0
ENSURES: l : int list, a list of all the prime factors of n is some order
*)
(*
fun t_apprx_int_squareRoot(n : int, acc : int) : int = 
    case ((acc * acc - n ) < 0)
      of true => t_apprx_int_squareRoot(n, acc + 1)
       | false => acc
fun apprx_int_squareRoot(n : int) : int = t_apprx_int_squareRoot(n, 1)
*)

fun factorize (n : int) : int list =  upto(n, n)

(* Test cases for factorize *)
(*
val test1 = factorize(1) = [] (* 1 has no prime factors *)
val test2 = factorize(2) = [2] (* 2 is prime *)
val test3 = factorize(3) = [3] (* 3 is prime *)
val test4 = factorize(4) = [2, 2] (* 4 = 2^2 *)
val test5 = factorize(9999) = [101, 11, 3, 3]
val test6 = factorize(53) = [53] (* 53 is prime *)
*)

(*
notPrime (n) -> bool
REQUIRES: n : int, n > 0
ENSURES: returns true if n is not prime
         returns false if n is prime
*)

fun notPrime (1 : int) : bool = true
  | notPrime (n) = 
    let 
     val x::xs = factorize n 
    in
     if xs = [] then false else true
    end  
(*
(* Basic Edge Cases *)
val test1 = notPrime(1) = true      (* 1 is not prime by definition *)
val test2 = notPrime(2) = false     (* Smallest prime *)
val test3 = notPrime(3) = false     (* Smallest odd prime *)

(* Perfect Powers *)
val test4 = notPrime(8) = true      (* 2^3 *)
val test5 = notPrime(27) = true     (* 3^3 *)
val test6 = notPrime(625) = true    (* 5^4 *)

(* Carmichael Numbers - Non-prime numbers that pass some primality tests *)
val test7 = notPrime(561) = true    (* Smallest Carmichael number *)
val test8 = notPrime(1105) = true   (* Another Carmichael number *)
val test9 = notPrime(2465) = true   (* Carmichael number = 5 × 17 × 29 *)

(* Large Prime Numbers *)
val test10 = notPrime(7919) = false (* Large prime *)
val test11 = notPrime(9973) = false (* Prime close to 10000 *)

(* Products of Large Primes *)
val test12 = notPrime(9991 * 9973) = true (* Product of two large primes *)
val test13 = notPrime(7919 * 7907) = true (* Product of consecutive primes *)

(* Numbers Near Powers of 2 *)
val test14 = notPrime(4095) = true  (* 2^12 - 1, highly composite *)
val test15 = notPrime(4097) = true  (* 2^12 + 1, composite *)

(* Mersenne Numbers (2^n - 1) *)
val test16 = notPrime(127) = false  (* 2^7 - 1, Mersenne prime *)
val test17 = notPrime(511) = true   (* 2^9 - 1, not prime *)

(* Fermat Numbers (2^(2^n) + 1) *)
val test18 = notPrime(257) = false  (* F3, Fermat prime *)
val test19 = notPrime(65537) = false (* F4, largest known Fermat prime *)

(* Numbers with Special Properties *)
val test20 = notPrime(5040) = true  (* Highly composite number *)
val test21 = notPrime(6601) = true  (* Pseudoprime base 2 *)
val test22 = notPrime(8911) = true (* Composite that looks prime *)

(* Edge Cases Near Powers *)
val test23 = notPrime(10001) = true (* Just after 10000 *)
val test24 = notPrime(9999) = true  (* Just before 10000 *)

(* Performance Tests *)
val test25 = notPrime(999983) = false (* Large prime near 1000000 *)
val test26 = notPrime(999981) = true  (* Large composite near 1000000 *)

(* Pairs of Numbers *)
val test27 = notPrime(91) = true    (* Product of small primes 7×13 *)
val test28 = notPrime(89) = false   (* Prime between composite numbers *)
*)

fun notPrime_cert (1 : int) : bool * int list = (true, [])
  | notPrime_cert (n) = 
    let 
     val x::xs = factorize n 
    in
     if xs = [] then (false, []) else (true, x::xs)
    end
(*
(* Test cases for notPrime_cert *)
(* Base Cases *)
val test1 = notPrime_cert(1) = (true, [])       (* Special case: 1 *)
val test2 = notPrime_cert(2) = (false, [])      (* Smallest prime *)
val test3 = notPrime_cert(3) = (false, [])      (* Smallest odd prime *)

(* Perfect Powers *)
val test4 = notPrime_cert(16) = (true, [2,2,2,2])    (* 2^4 *)
val test5 = notPrime_cert(81) = (true, [3,3,3,3])    (* 3^4 *)
val test6 = notPrime_cert(625) = (true, [5,5,5,5])   (* 5^4 *)

(* Carmichael Numbers *)
val test7 = notPrime_cert(561) = (true, [17,11,3])   (* Smallest Carmichael *)
val test8 = notPrime_cert(1105) = (true, [17,13,5])  (* Another Carmichael *)
val test9 = notPrime_cert(2821) = (true, [31,13,7])  (* Large Carmichael *)

(* Products of Large Primes *)
val test10 = notPrime_cert(91) = (true, [17,13])      (* Small semi-prime *)
(* Complex Factorizations *)
val test13 = notPrime_cert(2047) = (true, [89,23])   (* 2^11 - 1, not Mersenne prime *)
val test14 = notPrime_cert(2048) = (true, [2,2,2,2,2,2,2,2,2,2,2]) (* 2^11 *)
val test15 = notPrime_cert(5040) = (true, [7,5,3,3,2,2,2,2]) (* Highly composite *)

(* Special Number Types *)
val test16 = notPrime_cert(127) = (false, [])        (* Mersenne prime *)
val test20 = notPrime_cert(729) = (true, [3,3,3,3,3,3])  (* 3^6 *)
*)

fun notPrime_check (n : int) : bool = 
  let 
   val (isNotPrime, _) = notPrime_cert n
  in
   if isNotPrime then true else false
  end
(*
(* Test cases for notPrime_check *)

(* Base Cases - Verify special values *)
val test1 = notPrime_check(1) = true        (* Special case: 1 is not prime *)
val test2 = notPrime_check(2) = false       (* Smallest prime *)
val test3 = notPrime_check(3) = false       (* Smallest odd prime *)

(* Perfect Powers - Verify factorizations of powers *)
val test4 = notPrime_check(16) = true       (* 2^4 *)
val test5 = notPrime_check(81) = true       (* 3^4 *)
val test6 = notPrime_check(625) = true      (* 5^4 *)

(* Carmichael Numbers - Numbers that can fool some primality tests *)
val test7 = notPrime_check(561) = true      (* 561 = 3 × 11 × 17 *)
val test8 = notPrime_check(1105) = true     (* 1105 = 5 × 13 × 17 *)
val test9 = notPrime_check(2821) = true     (* 2821 = 7 × 13 × 31 *)

(* Edge Cases - Verify handling of special numbers *)
val test10 = notPrime_check(4) = true       (* Smallest composite *)
val test11 = notPrime_check(25) = true      (* Square of prime *)
val test12 = notPrime_check(127) = false    (* Mersenne prime *)

(* Large Numbers - Test performance and accuracy *)
val test13 = notPrime_check(997 * 991) = true    (* Product of large primes *)
val test14 = notPrime_check(997) = false         (* Large prime *)
val test15 = notPrime_check(1000000007) = false  (* Large prime near million *)

(* Mixed Properties - Various mathematical properties *)
val test16 = notPrime_check(2047) = true    (* 2^11 - 1, not Mersenne prime *)
val test17 = notPrime_check(2048) = true    (* Pure power of 2 *)
val test18 = notPrime_check(5040) = true    (* Highly composite number *)

(* Certificate Validation - Check if factors multiply to n *)
val test19 = notPrime_check(100) = true     (* 2^2 × 5^2 *)
val test20 = notPrime_check(729) = true     (* 3^6 *)
val test21 = notPrime_check(901) = true     (* 17 × 53 *)

(* False Positives Check - Verify against incorrect certificates *)
val test22 = notPrime_check(17) = false     (* Prime but might get wrong cert *)
val test23 = notPrime_check(23) = false     (* Prime but might get wrong cert *)
val test24 = notPrime_check(29) = false     (* Prime but might get wrong cert *)

(* Boundary Cases - Test near powers of 2 *)
val test25 = notPrime_check(32767) = true   (* 2^15 - 1 *)
val test26 = notPrime_check(32768) = true   (* 2^15 *)
val test27 = notPrime_check(32769) = true   (* 2^15 + 1 *)

(* Product Verification - Check multiplication of factors *)
val test28 = notPrime_check(1001) = true    (* 7 × 11 × 13 *)
val test29 = notPrime_check(1024) = true    (* 2^10 *)
val test30 = notPrime_check(1029) = true    (* 3 × 7 × 7 × 7 *)
*)
