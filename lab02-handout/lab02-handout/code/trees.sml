datatype tree = empty | node of tree * int * tree;

fun append ([]: int list, l2: int list) : int list = l2
  | append (x :: xs, l2) =  x :: append (xs, l2);

fun inorder (empty: tree): int list = []
  | inorder (node(t1, x, t2)) = append (inorder t1, (x :: inorder t2));

fun size (empty: tree) : int = 0
  | size (node(t1, x, t2)) = 1 + size t1 + size t2;

(* Test *)
val [1, 2, 3, 4, 5, 6, 7, 8] = append ([1, 2, 3], [4, 5, 6, 7, 8]);
val [1, 2] = append ([], [1, 2]);
val [1, 2, 3] = inorder (node(node(empty, 1, empty), 2, node(empty, 3, empty)));
val [1, 2, 3, 4, 5] = inorder (node(node(node(empty, 1, empty), 2, node(empty, 3, empty)), 4, node(empty, 5, empty)));
val [] = inorder empty;

val 3 = size (node (node (empty, 1, empty), 2, node (empty, 3, empty)));
val 2 = size (node (node (empty, 1, empty), 2, empty));