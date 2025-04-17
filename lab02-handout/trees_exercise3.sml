datatype stree = Empty | Node of stree * string * stree;

(* fun insert (w: string, Empty: stree): stree = Node(Empty, w, Empty)
  | insert (w, Node(tl, s, tr)) = if w < s then Node(insert(w, tl), s, tr) else Node(Node(tl, s, insert(w, tr))); *)

fun inorder ( Empty : stree ) : string list = []
  | inorder ( Node (tl, s, tr) ) = inorder tl @ (s :: inorder tr);

(* sorted : string list -> bool *)
fun get_first_of_list ([] : string list ) : string = ""
  | get_first_of_list (x :: xs) = x;

fun sorted ([] : string list) : bool = true
  | sorted (x :: []) = true
  | sorted (x :: xs) = (x < get_first_of_list (xs)) andalso sorted(xs);
(* Test for sorted *)
val true = sorted ["a"]
val true = sorted ["a", "b", "c"];
val false = sorted ["c", "b", "a"];
val true = sorted ["a", "d", "f", "h"];

(* in_stree: string * stree -> bool*)
fun in_stree (str : string, Empty : stree) : bool = false
  | in_stree (str, Node (tl, s, tr)) = (str = s) orelse (in_stree (str, tl)) orelse (in_stree (str, tr));
(* Test for sorted *)
val false = in_stree ("a", Empty);
val true = in_stree ("a", Node (Empty, "a", Node (Empty, "b", Node (Empty, "c", Empty))) );
val true = in_stree ("b", Node (Node (Node (Empty, "e", Empty), "b", Node (Empty, "d", Empty)), "a", Node (Node (Empty, "f", Empty), "c", Node (Empty, "g", Empty))));

(* insert (s, t) ==> t' 
 * REQUIRES t is a sorted tree (or sorted (inorder t))
 * ENSURES sorted(inorder t') andalso in(s, t')
 *)
fun insert (w : string, Empty : stree) : stree = Node (Empty, w, Empty)
  | insert (w, Node (tl, s, tr)) = 
    case String.compare (w, s)
      of EQUAL => Node (tl, s, tr)
       | GREATER => Node (tl, s, insert (w, tr))
       | LESS => Node (insert (w, tl), s, tr)