Control.Print.printLength := 100;
Control.Print.printDepth := 100;

(*********************** Insertion sort on lists ***********************)

(* insert (x, l) ==> l'
 * REQUIRES: sorted l
 * ENSURES:
    - isPermutation (x::l, l')
    - sorted l'
 *)
fun insert (x: int, []: int list): int list = [x]
  | insert (x, y::l) =
     if x <= y
       then x::y::l
       else y :: insert (x, l)

(* isort l ==> l'
 * ENSURES:
    - isPermutation (l, l')
    - sorted l'
 *)
fun isort ([]: int list): int list = []
  | isort (x::l) = insert (x, isort l)


(*********************** Insertion sort on trees ***********************)

datatype itree = empty
               | node of itree * int * itree


(***** Begin utility functions *****)

(* sorted l ==> b
   ENSURES: b is true iff l is sorted in ascending order
 *)
fun sorted ([]: int list): bool = true
  | sorted [_] = true
  | sorted (x::y::l) = x <= y andalso sorted (y::l)

(* inorder t ==> l
 * ENSURES: l is the inorder traversal of t
 *)
fun inorder (t: itree): int list =
  let
    fun inorder' (empty: itree, l: int list): int list = l
      | inorder' (node(tL, x, tR), l) =
          inorder'(tL, x :: inorder' (tR, l))
  in
    inorder' (t, [])
  end 

(* Insert (x, t) ==> t'
 * REQUIRES: sorted (inorder t)
 * ENSURES:
    - isPermutation (x::inorder t, inorder t')
    - sorted (inorder t')
 *)
 
fun Insert (x: int, empty: itree): itree = node(empty, x, empty)
  | Insert (x, node(tL,y,tR)) =
     if x <= y
       then node (Insert (x, tL), y, tR)
       else node (tL, y, Insert (x, tR))

(****** End utility functions ******)

fun ILsort'([]: int list, aux_tree: itree): itree = aux_tree
  | ILsort'(x::xs, aux_tree) = ILsort'(xs, Insert(x, aux_tree))
  
fun ILsort (iTree: itree): itree = ILsort'(inorder(iTree), empty)
(*
(* Test cases for ILsort *)

(* Test 1: Empty tree *)
val test1 = ILsort(empty) = empty

(* Test 2: Single node tree *)
val test2 = ILsort(node(empty, 5, empty)) = node(empty, 5, empty)

(* Test 3: Small unbalanced tree *)
val ans1 = ILsort(node(node(empty, 3, empty), 1, node(empty, 2, empty)))
val ans2 = ILsort(node(
      node(empty, 5, empty),
      3,
      node(empty, 1, empty)
    ))
val ans3 = ILsort(node(
      node(node(empty, 7, empty), 4, empty),
      2,
      node(empty, 1, node(empty, 5, empty))
    ))
*)
fun Isort' (empty: itree, sorted_aux_tree): itree = sorted_aux_tree
  | Isort' (node(tL, x, tR), sorted_aux_tree) = Isort' (tR, Isort' (tL, Insert (x, sorted_aux_tree))) 

fun Isort (iTree: itree): itree = Isort' (iTree, empty)
(* Cases for Isort *)

(* Test 1: Empty tree *)
val ans4 = Isort(node(node(empty, 3, empty), 1, node(empty, 2, empty)))
val ans5 = Isort(node(
      node(empty, 5, empty),
      3,
      node(empty, 1, empty)
    ))
val ans6 = Isort(node(
      node(node(empty, 7, empty), 4, empty),
      2,
      node(empty, 1, node(empty, 5, empty))
    ))
