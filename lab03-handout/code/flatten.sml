fun oldFlat [] = []
  | oldFlat (L::LS) = L @ (oldFlat LS)

fun flatten (LL : int list list) : int list = raise Fail "Unimplemented"

fun flatten ([] : int list list) : int list = []
  | flatten ([]::[]) = []
  | flatten ((xs::[])::remain) = xs::flatten(remain)  
