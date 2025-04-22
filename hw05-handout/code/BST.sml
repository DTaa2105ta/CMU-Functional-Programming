type key = int 
type 'a entry = key * 'a

datatype 'a bst = Empty
		| Node of 'a bst * 'a entry * 'a bst
(*

*)
fun allLess (Empty: 'a bst, _: int): bool = true
  | allLess (Node (left, (k', _), right), k) = 
    k' < k andalso 
    allLess (left, k) andalso 
    allLess (right, k)
  
fun allGreater (Empty: 'a bst, _: int): bool = true
  | allGreater (Node (left, (k', _), right), k) =
    k' > k andalso 
    allGreater (left, k) 
    andalso allGreater (right, k)
  
fun is_bst (Empty: 'a bst): bool = true
  | is_bst (Node (left, (k, _), right)) =
    allLess (left, k) andalso 
    allGreater (right, k) andalso
    is_bst left andalso 
    is_bst right
(* Test cases for is_bst *)

(* Base cases *)
val test_bst_empty = is_bst Empty = true
val test_bst_single = is_bst (Node(Empty, (5, "five"), Empty)) = true

(* Simple BSTs - testing boundary conditions *)
val test_bst_consecutive = 
    is_bst (Node(Node(Empty, (1, "one"), Empty), 
                 (2, "two"), 
                 Node(Empty, (3, "three"), Empty))) = true

val test_bst_wide_range = 
    is_bst (Node(Node(Empty, (1, "one"), Empty), 
                 (10, "ten"), 
                 Node(Empty, (20, "twenty"), Empty))) = true

(* Invalid BSTs - testing violation of BST property *)
val test_bst_invalid_left_equal = 
    is_bst (Node(Node(Empty, (5, "five"), Empty), (5, "five"), Empty)) = false

val test_bst_invalid_right_equal = 
    is_bst (Node(Empty, (5, "five"), Node(Empty, (5, "five"), Empty))) = false

val test_bst_invalid_left_greater = 
    is_bst (Node(Node(Empty, (6, "six"), Empty), (5, "five"), Empty)) = false

val test_bst_invalid_right_less = 
    is_bst (Node(Empty, (5, "five"), Node(Empty, (4, "four"), Empty))) = false

(* Complex valid BST - testing nested comparisons *)
val valid_complex_bst = 
    Node(
        Node(Node(Empty, (10, "ten"), Empty), 
             (20, "twenty"), 
             Node(Empty, (25, "twenty-five"), Empty)),
        (30, "thirty"),
        Node(Node(Empty, (35, "thirty-five"), Empty), 
             (40, "forty"), 
             Node(Empty, (45, "forty-five"), Empty))
    )
val test_bst_complex_valid = is_bst valid_complex_bst = true

(* Complex invalid BST - testing subtle violations *)
val invalid_subtle_bst = 
    Node(
        Node(Node(Empty, (10, "ten"), Empty), 
             (20, "twenty"), 
             Node(Empty, (31, "thirty-one"), Empty)), (* Violates: 31 > 30 *)
        (30, "thirty"),
        Node(Node(Empty, (29, "twenty-nine"), Empty), (* Violates: 29 < 30 *)
             (40, "forty"), 
             Node(Empty, (45, "forty-five"), Empty))
    )
val test_bst_subtle_invalid = is_bst invalid_subtle_bst = false

(* Testing deep violation *)
val deep_violation_bst = 
    Node(
        Node(
            Node(
                Node(Empty, (5, "five"), Empty),
                (10, "ten"),
                Node(Empty, (25, "twenty-five"), Empty) (* Violates: 25 > 20 *)
            ),
            (20, "twenty"),
            Empty
        ),
        (30, "thirty"),
        Node(Empty, (40, "forty"), Empty)
    )
val test_bst_deep_violation = is_bst deep_violation_bst = false

(* Invalid BST - 7 violates the left-subtree rule *)
val invalidBST= 
    Node(
        Node(
            Node(Empty, (2, #"a"), Empty),
            (3, #"b"),
            Node(Empty, (7, #"e"), Empty)),  (* 7 > 5 violates BST property *)
        (5, #"d"),
        Node(Empty, (6, #"i"), Empty))
val invalidBSTResult = is_bst invalidBST = false