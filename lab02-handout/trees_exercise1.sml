datatype stree = Empty | Node of stree * string *stree;
(* size : stree -> int, size computes the number of nodes (size) *)
fun size (Empty : stree) : int = 0
  | size (Node (L, content, R)) = 1 + size(L) + size(R);
(* Test for size of stree *)
val 0 = size(Empty);
val 3 = size(Node (Node (Empty, "b", Empty), "a", Node (Empty, "c", Empty)));
val 4 = size(Node (Node (Node (Empty, "c", Empty), "b", Empty), "a", Node (Empty, "c", Empty)));
val 3 = size(Node (Node (Empty, "b", Empty), "a", Node (Empty, "f", Empty)));

fun max (x : int, y : int) : int = if x < y then y else x;
fun height (Empty : stree) : int = 0
  | height (Node (Empty, _, Empty)) = 0
  | height (Node (L, _, R)) = 1 + max (height L, height R);
(* Test for height of stree *)
val 0 = height(Empty);
val 0 = height(Node (Empty, "a", Empty));
val 1 = height(Node (Node (Empty, "b", Empty), "a", Empty));
val 2 = height(Node (Node (Node (Empty, "c", Empty), "b", Empty), "a", Node (Empty, "c", Empty)));
