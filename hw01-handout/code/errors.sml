
(* Rectangle *)

fun perimeter (a: real, b: real): real = 2.0 * (a + b);

fun area (a: real, b: real) : real = a * b;

fun sqrt (a : real) : real = Math.sqrt(a);
fun diagonal (a: real, b: real): real = sqrt (a*a + b*b);

fun isSquare (a: int, b: int): bool = a = b;

fun isValid (a: real, b: real): bool = a > 0.0 andalso b > 0.0;

fun isDegenerate (0: int, b: int): bool = true
  | isDegenerate (a, 0) = true
  | isDegenerate (a, b) = false;

