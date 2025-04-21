Control.Print.printLength := 100;
Control.Print.printDepth := 100;
datatype 'a tree = Empty
                 | Node of 'a tree * 'a * 'a tree

(* inorder t ==> l
 * ENSURES: l is the in-order traversal of t
 *)
fun inorder (Empty: 'a tree): 'a list = nil
  | inorder (Node(tL,x,tR)) = (inorder tL) @ (x :: inorder tR)

fun rightmost (Node (tL, a, tR): 'a tree): 'a * 'a tree = 
  let
    fun rightmost' (Node (tL, a, Empty): 'a tree, fn_acc: 'a tree -> 'a tree): 'a * 'a tree = (a, fn_acc tL)
      | rightmost' (Node (tL, a, tR), fn_acc) = rightmost' (tR, fn newRight => fn_acc (Node (tL, a, newRight))) 
  in
    rightmost' (Node(tL, a, tR), fn x => x)
  end

(*
merge (t1, t2) ==> t
ENSURES: (inorder t1) @ (inorder t2) \cong (inorder t)
*) 
fun merge (Empty: 'a tree, t2: 'a tree): 'a tree = t2
  | merge (t1, t2) = 
  let
    val (t1_rightmostNode, t1') = rightmost t1
  in
    Node (t1', t1_rightmostNode, t2)
  end
(*
(* Degenerate left and right spines *)
val degenerate_left = Node(Node(Node(Empty, 1, Empty), 2, Empty), 3, Empty)
val degenerate_right = Node(Empty, 4, Node(Empty, 5, Node(Empty, 6, Empty)))
val merge_degenerate = merge(degenerate_left, degenerate_right)

(* Complex zigzag pattern *)
val zigzag_tree = Node(
    Node(Empty, 2, Node(Node(Empty, 1, Empty), 3, Empty)),
    4,
    Node(Node(Empty, 6, Empty), 5, Node(Empty, 7, Empty))
)
val simple_tree = Node(Node(Empty, 8, Empty), 9, Node(Empty, 10, Empty))
val merge_zigzag = merge(zigzag_tree, simple_tree)

(* Balanced but with gaps *)
val gap_tree = Node(
    Node(Node(Empty, 1, Empty), 3, Empty),
    5,
    Node(Empty, 7, Node(Empty, 9, Empty))
)
val fill_tree = Node(
    Node(Empty, 2, Empty),
    4,
    Node(Empty, 6, Empty)
)
val merge_gaps = merge(gap_tree, fill_tree)

(* Alternating pattern *)
val alternating_left = Node(
    Node(Empty, 1, Node(Empty, 3, Empty)),
    2,
    Node(Empty, 5, Node(Empty, 4, Empty))
)
val alternating_right = Node(
    Node(Empty, 7, Node(Empty, 6, Empty)),
    8,
    Node(Empty, 9, Node(Empty, 10, Empty))
)
val merge_alternating = merge(alternating_left, alternating_right)

(* Deep nested structure *)
val nested_tree = Node(
    Node(
        Node(Node(Empty, 1, Empty), 2, Node(Empty, 3, Empty)),
        4,
        Node(Empty, 5, Empty)
    ),
    6,
    Node(Node(Empty, 7, Empty), 8, Node(Empty, 9, Empty))
)
val small_complex = Node(
    Node(Node(Empty, 10, Empty), 11, Empty),
    12,
    Node(Empty, 13, Node(Empty, 14, Empty))
)
val merge_nested = merge(nested_tree, small_complex)

(* Mixed patterns with duplicate values *)
val mixed_tree1 = Node(
    Node(Node(Empty, 1, Empty), 1, Node(Empty, 2, Empty)),
    2,
    Node(Empty, 3, Node(Empty, 3, Empty))
)
val mixed_tree2 = Node(
    Node(Empty, 1, Node(Empty, 2, Empty)),
    2,
    Node(Empty, 3, Empty)
)
val merge_mixed = merge(mixed_tree1, mixed_tree2)
*)

fun member (x: ''a, []: ''a list): bool = false
  | member (x, y::ys) = (x = y) orelse member(x, ys)
(*
(* Test cases for member function *)

(* Basic cases with integers *)
val test_member_empty = member(1, []) = false
val test_member_single = member(1, [1]) = true
val test_member_single_false = member(2, [1]) = false

(* Multiple elements *)
val test_member_first = member(1, [1,2,3,4,5]) = true
val test_member_middle = member(3, [1,2,3,4,5]) = true
val test_member_last = member(5, [1,2,3,4,5]) = true
val test_member_missing = member(6, [1,2,3,4,5]) = false

(* Duplicate elements *)
val test_member_duplicates = member(2, [1,2,2,2,3]) = true
val test_member_after_duplicates = member(3, [1,2,2,2,3]) = true

(* Characters - testing polymorphic equality *)
val test_member_char = member(#"a", [#"a",#"b",#"c"]) = true
val test_member_char_missing = member(#"d", [#"a",#"b",#"c"]) = false

(* Boolean values *)
val test_member_bool = member(true, [false, true, false]) = true
val test_member_bool_missing = member(true, [false, false, false]) = false

(* Mixed-type tuples *)
val test_member_tuple = 
    member((1,#"a"), [(2,#"b"), (1,#"a"), (3,#"c")]) = true
val test_member_tuple_missing = 
    member((4,#"d"), [(2,#"b"), (1,#"a"), (3,#"c")]) = false

(* Edge cases *)
val test_member_large = 
    let
        val large_list = List.tabulate(1000, fn x => x)
    in
        member(999, large_list) = true andalso
        member(1000, large_list) = false
    end

(* Nested tuples *)
val test_member_nested = 
    member((1,(2,#"a")), [(3,(4,#"b")), (1,(2,#"a")), (5,(6,#"c"))]) = true

(* List elements *)
val test_member_lists = 
    member([1,2], [[3,4], [1,2], [5,6]]) = true

(* Complex combinations *)
val test_member_complex = 
    member((1,[#"a",#"b"]), 
           [(2,[#"c",#"d"]), (1,[#"a",#"b"]), (3,[#"e",#"f"])]) = true

(* Performance test with repeated elements *)
val test_member_performance = 
    let
        val repeat_list = List.tabulate(1000, fn x => x mod 2)
    in
        member(1, repeat_list) = true andalso
        member(2, repeat_list) = false
    end
*)
fun indexOf (x: ''a, l: ''a list): int option =
  	let
  	  fun indexOf' (x: ''a, []: ''a list, acc: int): int option = NONE
  	    | indexOf' (x, y::ys, acc) = if (x = y) then SOME acc else indexOf' (x, ys, acc + 1)
  	in
  	  indexOf' (x, l, 0)
  	end
(*
(* Test cases for indexOf function *)

(* Basic cases *)
val test_indexOf_empty = indexOf(1, []) = NONE
val test_indexOf_single = indexOf(1, [1]) = SOME 0
val test_indexOf_single_missing = indexOf(2, [1]) = NONE

(* Position testing *)
val test_indexOf_first = indexOf(1, [1,2,3,4,5]) = SOME 0
val test_indexOf_middle = indexOf(3, [1,2,3,4,5]) = SOME 2
val test_indexOf_last = indexOf(5, [1,2,3,4,5]) = SOME 4
val test_indexOf_missing = indexOf(6, [1,2,3,4,5]) = NONE

(* Duplicate elements - should return first occurrence *)
val test_indexOf_duplicates = indexOf(2, [1,2,2,2,3]) = SOME 1
val test_indexOf_after_duplicates = indexOf(3, [1,2,2,2,3]) = SOME 4

(* Different types *)
val test_indexOf_char = indexOf(#"a", [#"a",#"b",#"c"]) = SOME 0
val test_indexOf_bool = indexOf(true, [false,true,false]) = SOME 1

(* Composite types *)
val test_indexOf_tuple = 
    indexOf((1,#"a"), [(2,#"b"), (1,#"a"), (3,#"c")]) = SOME 1

val test_indexOf_nested_tuple = 
    indexOf((1,(2,#"a")), [(3,(4,#"b")), (1,(2,#"a")), (5,(6,#"c"))]) = SOME 1

(* Lists as elements *)
val test_indexOf_list = 
    indexOf([1,2], [[3,4], [1,2], [5,6]]) = SOME 1

(* Edge cases *)
val test_indexOf_large = 
    let
        val large_list = List.tabulate(1000, fn x => x)
    in
        indexOf(999, large_list) = SOME 999 andalso
        indexOf(1000, large_list) = NONE
    end

(* Complex combinations *)
val test_indexOf_complex = 
    indexOf((1,[#"a",#"b"]), 
           [(2,[#"c",#"d"]), (1,[#"a",#"b"]), (3,[#"e",#"f"])]) = SOME 1

(* Performance cases *)
val test_indexOf_performance = 
    let
        val repeat_list = List.tabulate(1000, fn x => x mod 2)
    in
        indexOf(1, repeat_list) = SOME 1 andalso
        indexOf(2, repeat_list) = NONE
    end
*)
(*
fun remove (x: ''a, []: ''a list): ''a list = []
  | remove (x, y::ys) = if (x = y) then remove (x, ys) else y :: remove (x, ys)
*) 
fun remove (x: ''a, l: ''a list): ''a list = 
    let 
      fun tremove (x: ''a, []: ''a list, accL: ''a list) = accL
        | tremove (x, y::ys, accL) =
          case x = y
            of true => tremove (x, ys, accL)
             | false => tremove (x, ys, accL @ [y])
    in
      tremove (x, l, [])
    end

(* Test cases for remove function *)

(* Base cases *)
val test_remove_empty = remove(1, []) = []
val test_remove_single = remove(1, [1]) = []
val test_remove_single_missing = remove(2, [1]) = [1]

(* Multiple elements *)
val test_remove_first = remove(1, [1,2,3,4,5]) = [2,3,4,5]
val test_remove_middle = remove(3, [1,2,3,4,5]) = [1,2,4,5]
val test_remove_last = remove(5, [1,2,3,4,5]) = [1,2,3,4]
val test_remove_missing = remove(6, [1,2,3,4,5]) = [1,2,3,4,5]

(* Multiple occurrences *)
val test_remove_duplicates = remove(2, [1,2,2,2,3]) = [1,3]
val test_remove_alternating = remove(1, [1,2,1,2,1]) = [2,2]

(* Different types *)
val test_remove_char = remove(#"a", [#"a",#"b",#"a",#"c"]) = [#"b",#"c"]
val test_remove_bool = remove(true, [true,false,true,false]) = [false,false]

(* Complex types *)
val test_remove_tuple = 
    remove((1,#"a"), [(2,#"b"), (1,#"a"), (1,#"a"), (3,#"c")]) = 
    [(2,#"b"), (3,#"c")]

val test_remove_nested = 
    remove((1,[2]), [(1,[2]), (2,[3]), (1,[2]), (3,[4])]) = 
    [(2,[3]), (3,[4])]

(* Edge cases *)
val test_remove_all_same = 
    remove(1, List.tabulate(5, fn _ => 1)) = []

(* Performance test *)
val test_remove_large = 
    let
        val large_list = List.tabulate(1000, fn x => x mod 3)
        val result = remove(2, large_list)
    in
        length result = 667 andalso  (* 1000 - 333 occurrences of 2 *)
        not (member(2, result))      (* No 2's remaining *)
    end

(* Mixed patterns *)
val test_remove_mixed = 
    let
        val complex_list = [(1,#"a"), (2,#"a"), (1,#"b"), (1,#"a")]
        val result = remove((1,#"a"), complex_list)
    in
        result = [(2,#"a"), (1,#"b")]
    end

(* Stress test with repeated operations *)
val test_remove_sequential = 
    let
        val list = [1,2,3,1,2,3,1,2,3]
        val step1 = remove(1, list)
        val step2 = remove(2, step1)
        val step3 = remove(3, step2)
    in
        step3 = []
    end