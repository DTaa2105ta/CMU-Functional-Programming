(* octal n ==> r *)
fun octal (n : int) : int = 
    if n < 8
        then n
        else (n mod 8) + 10 * octal (n div 8)
        