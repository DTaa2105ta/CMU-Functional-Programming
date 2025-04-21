datatype strlist = Nil | Cons of string * strlist;

fun concat (Nil : strlist) : string = ""
  | concat (Cons (firststr, strX)) = firststr ^ concat strX;

val test = Cons ("hello", Cons (" world", Nil));
concat test;  (* Returns "hello world" *)