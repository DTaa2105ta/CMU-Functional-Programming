
datatype tree = Empty
              | Leaf of string
              | Node of tree * tree

datatype itree = iEmpty
               | iLeaf of string
               | iNode of itree * int * itree


(* fun rightorder (temp: tree, tail: int) : tree = *)
fun size (Empty : tree) : int = 0
  | size (Leaf(s)) = 1
  | size (Node(t_l, t_r)) = 1 + size t_l + size t_r

fun instrument (Empty : tree) : itree = iEmpty
  | instrument (Leaf(s)) = iLeaf (s)
  | instrument (Node(t_l, t_r)) = iNode (instrument(t_l), size(t_r) - size(t_l), instrument(t_r));
(*
(* **Tests for instrument()** *)
val test_empty = instrument(Empty);
(* Expected: iEmpty *)
val test_leaf = instrument(Leaf "a");
(* Expected: iLeaf "a" *)
val balanced_tree = Node(Node(Leaf "a", Leaf "b"), Node(Leaf "c", Leaf "d"));
val test_balanced = instrument(balanced_tree);
(* Expected: iNode(iNode(iLeaf "a", 0, iLeaf "b"), 0, iNode(iLeaf "c", 0, iLeaf "d")) *)
val left_skewed = Node(Node(Leaf "a", Empty), Empty);
val test_left_skewed = instrument(left_skewed);
(* Expected: iNode(iNode(iLeaf "a", ~1, iEmpty), ~2, iEmpty) *)
val right_skewed = Node(Empty, Node(Empty, Leaf "b"));
val test_right_skewed = instrument(right_skewed);
(* Expected: iNode(iEmpty, 2, iNode(iEmpty, 1, iLeaf "b")) *)
val nested = Node(Node(Leaf "a", Node(Leaf "b", Leaf "c")), Leaf "d");
val test_nested = instrument(nested);
(* Expected: iNode(iNode(iLeaf "a", 2, iNode(iLeaf "b", 0, iLeaf "c")), ~4, iLeaf "d") *)
fun make_large_tree(depth: int) : tree =
  if depth = 0 then Leaf "x"
  else Node(make_large_tree(depth-1), make_large_tree(depth-1));

val large_tree = make_large_tree(20);  (* 2^20 leaves *)
val test_large = instrument(large_tree);  (* Should not stack overflow *)
*)
(* ---------------------------------------------------------------- *)
fun iSize (iEmpty : itree) = 0
  | iSize (iLeaf(s)) = 1
  | iSize (iNode(t_l, _, t_r)) = 1 + iSize t_l + iSize t_r;

fun validate (iEmpty : itree) : bool = true
  | validate (iLeaf(s)) = true 
  | validate (iNode(t_l, i, t_r)) = (i = iSize(t_r) - iSize(t_l)) andalso validate t_l andalso validate t_r;

(* **Tests for instrument()** *)
val test_leaf = validate(iLeaf "x");
(* Expected: true *)
val balanced = iNode(iNode(iLeaf "a", 0, iLeaf "b"), 0, iNode(iLeaf "c", 0, iLeaf "d"));
val test_balanced = validate(balanced);
(* Expected: true *)
val invalid_imbalance = iNode(iLeaf "a", 1, iLeaf "b");  (* True imbalance = 0 *)
val test_invalid = validate(invalid_imbalance);
(* Expected: false *)
val partial_valid = iNode(iNode(iLeaf "a", 1, iLeaf "b"), 0, iLeaf "c");  (* Left subtree invalid *)
val test_partial = validate(partial_valid);
(* Expected: false *)
val negative_imb = iNode(iNode(iLeaf "a", ~1, iEmpty), ~1, iLeaf "b");
val test_negative = validate(negative_imb);
(* Expected: true *)
val hidden_invalid = iNode(
  iNode(iLeaf "a", 0, iLeaf "b"), 
  0, 
  iNode(iNode(iLeaf "c", 2, iLeaf "d"), 0, iLeaf "e")  (* Inner node has invalid imbalance *)
);
val test_hidden = validate(hidden_invalid);
(* Expected: false *)
val size_mismatch = iNode(
  iNode(iLeaf "a", 0, iNode(iLeaf "b", 0, iLeaf "c")), 
  0,  (* True imbalance = 1 (right subtree larger) *)
  iLeaf "d"
);
val test_mismatch = validate(size_mismatch);
(* Expected: false *)

fun iSize' (iEmpty : itree) : int = 0
  | iSize' (iLeaf(s)) = 1
  | iSize' (iNode(t_l, i, t_r)) = if validate(iNode(t_l, i, t_r)) then (1 + i + 2 * iSize' t_l) else raise Fail "Invalid";

(* Tests for iSize'*)
val nested_balanced = 
  iNode(
    iNode(iLeaf "a", 0, iLeaf "b"),  (* Left subtree *)
    0,                                (* Imbalance = 0 *)
    iNode(iLeaf "c", 0, iLeaf "d")    (* Right subtree *)
  );
(* Structure:
      [0]
     /   \
  [0]     [0]
  / \     / \
a   b   c   d
*)
val test1 = (iSize' nested_balanced = 7, validate nested_balanced = true);
(* Expected: (true, true) *)
val nested_left_heavy =
  iNode(
    iNode(iLeaf "a", ~1, iEmpty),  (* Left subtree: size 2 (1 leaf + 1 empty) *)
    ~1,                            (* Imbalance = size(right) - size(left) = 1 - 2 = -1 *)
    iLeaf "b"                      (* Right subtree: size 1 *)
  );
(* Structure:
     [-1]
     /   \
  [~1]    b
  / \
a   E
*)
val test2 = (iSize' nested_left_heavy = 4, validate nested_left_heavy = true);
(* Expected: (true, true) *)
(*
val nested_invalid =
  iNode(
    iNode(iLeaf "a", 0, iLeaf "b"),
    1,  (* Incorrect: Actual imbalance = size(right) - size(left) = 1 - 2 = -1 *)
    iLeaf "c"
  );
(* Structure:
      [1]  (* Invalid! *)
     /   \
  [0]     c
  / \
a   b
*)
val test4 = (validate nested_invalid = false);
(* Expected: true (should fail validation) *)
*)
val deep_nested_empty =
  iNode(
    iNode(iEmpty, 2, iNode(iEmpty, 1, iLeaf "a")),
    ~2,
    iLeaf "b"
  );
(* Structure:
      [~2]
     /   \
  [2]     b
  / \
E   [1]
     / \
    E   a
*)
val test6 = (iSize' deep_nested_empty = 5, validate deep_nested_empty = true);
(* Expected: (true, true) *)

fun tiltLeft (iEmpty : itree) : itree = iEmpty
  | tiltLeft (iLeaf s) = iLeaf s
  | tiltLeft (iNode(t_l, i, t_r)) = if i > 0 then iNode(tiltLeft(t_r), ~i, tiltLeft(t_l)) 
                                    else iNode(tiltLeft(t_l), i, tiltLeft(t_r))

(* Tests for tiltLeft *)
val deep_right = iNode(iLeaf "a", 3, iNode(iLeaf "b", 2, iNode(iLeaf "c", 0, iLeaf "d")));
(* Original: false one
         [3]
        /   \
      a    [2]
          / \
       b   [0]
           / \
         c   d
*)
val validate_result_for_deepRight = validate deep_right; (* false *)
val tilted5 = tiltLeft deep_right;
(* (iSize'(deep_right) = iSize(deep_right)) = false; (* Invalid *) *)
val validate_result_for_tiltLeft_deepRight = validate tilted5;
(* Expected:
         [~4]
        /   \
     [~2]    a
     / \
 [0]   b
 / \
d   c
*)
val with_empty = iNode(iEmpty, 2, iNode(iLeaf "a", ~1, iEmpty));
(* Original:
     [2]
    /   \
 E     [~1]
       / \
      a   E
*)
val validate_result_for_withEmpty = validate with_empty; (* true *)
val tilted6 = tiltLeft with_empty; 
(iSize'(with_empty) = iSize(with_empty)) = true; (* Expected: true *)
val validate_result_for_tiltLeft_withEmpty = validate tilted6;
(* Expected:
     [~2]
    /   \
 [1]    E
 / \
E   a
*)
