fun defaultToThree (NONE : int option):int = 3
  | defaultToThree (SOME x) = x;

val 2 = defaultToThree(SOME 2);
val 3 = defaultToThree(NONE);

fun searchForEven [] = NONE
  | searchForEven (x::xs) = if (x mod 2)=0 then SOME(x) else searchForEven xs;

val (SOME _) = searchForEven [1,2,3,4];
val NONE = searchForEven [1,3,5];