
datatype tree = Leaf of int
              | Node of tree * tree

fun treeMin (Leaf n : tree) : int = n
  | treeMin (Node (leftTree, rightTree)) = 
    let
    	val l = treeMin(leftTree)
    	val r = treeMin(rightTree)
    in
    	if l < r then l else r
    end
    
   (* Test cases for treeMin *)
val test1 = treeMin(Leaf 5) = 5
val test2 = treeMin(Node (Leaf 5, Leaf 10)) = 5
val test3 = treeMin(Node (Leaf 10, Leaf 5)) = 5

(* Nested tree with multiple levels *)
val test4 = treeMin(Node(Node(Leaf 8, Leaf 3), Node(Leaf 4, Leaf 6))) = 3

(* Tree with negative numbers *)
val test5 = treeMin(Node(Node(Leaf ~5, Leaf 0), Leaf 2)) = ~5

(* Unbalanced tree with left-heavy structure *)
val test6 = treeMin(Node(Node(Node(Leaf 7, Leaf 1), Leaf 3), Leaf 9)) = 1

(* Unbalanced tree with right-heavy structure *)
val test7 = treeMin(Node(Leaf 12, Node(Leaf 6, Node(Leaf 4, Leaf 8)))) = 4

(* Tree with duplicate minimum values *)
val test8 = treeMin(Node(Node(Leaf 2, Leaf 2), Node(Leaf 2, Leaf 3))) = 2

(* Tree with large integers *)
val test9 = treeMin(Node(Node(Leaf 99999, Leaf 88888), Node(Leaf 77777, Leaf 66666))) = 66666

(* Complex nested tree with mixed positive/negative values *)
val test10 = treeMin(
    Node(
        Node(Node(Leaf ~2, Leaf 5), Node(Leaf 0, Leaf 3)),
        Node(Node(Leaf 7, Leaf 1), Node(Leaf 4, Leaf ~1))
    )
) = ~2

(* Edge case: deep tree with minimum at the bottom *)
val test11 = treeMin(
    Node(
        Node(Node(Leaf 10, Leaf 9), Node(Leaf 8, Leaf 7)),
        Node(Node(Leaf 6, Leaf 5), Node(Leaf 4, Leaf 1))
    )
) = 1
