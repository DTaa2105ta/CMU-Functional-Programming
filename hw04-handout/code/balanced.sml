Control.Print.printLength := 100;
Control.Print.printDepth := 100;

datatype tree = Empty
              | Node of tree * char * tree

(* inorder t ==> l
   ENSURES: l is the inorder traversal of t
 *)
fun inorder (Empty: tree): char list = []
  | inorder (Node(tL, c, tR)) = (inorder tL) @ (c :: inorder tR)

(* size t ==> n
   ENSURES: n is the number of elements in t
 *)
fun size (Empty: tree): int = 0
  | size (Node(tL, _, tR)) = 1 + size tL + size tR
  
(* height t ==> h
   ENSURES: h is the height of tree t
 *)
fun height (Empty: tree): int = 0
  | height (Node(tL, _, tR)) = 1 + Int.max(height tL, height tR)

(* balanced t ==> b
   ENSURES: b iff the heights of the left and right subtree of every inner
            Node differ by at most 1
 *)
fun balanced (Empty: tree) : bool = true
  | balanced (Node(tL, _, tR)) =
    let
      val hl = height tL
      val hr = height tR
    in
      Int.abs(hl - hr) <= 1 andalso balanced tL andalso balanced tR
    end
(* Old splitN()
fun Insert (x: char, Empty: tree): tree = Node(Empty, x, Empty)
  | Insert (x, Node(tL,y,tR)) =
     if x <= y
       then Node (Insert (x, tL), y, tR)
       else Node (tL, y, Insert (x, tR))	      
	      
fun preorder (t: tree): char list =
  let
    fun preorder' (Empty: tree, l: char list): char list = l
      | preorder' (Node(tL, x, tR), l) =
      	  x :: preorder' (tL, preorder'(tR, l))
  in
    preorder'(t, [])
  end

fun truncateList (l: char list, i: int): char list * char list =
  let
    fun truncateList' ([]: char list, i: int, l1: char list, l2: char list): char list * char list = (l1, l2)
      | truncateList' (l, 0, l1, l2) = (l1, l)
      | truncateList' (x::xs, i, l1, l2) = truncateList'(xs, i-1, l1 @ [x], l2)
  in
    truncateList'(l, i, [], [])
  end

fun construct ([]: char list, t: tree): tree = t
  | construct (c :: cs, t) = construct (cs, Insert(c, t))

fun splitN (ctree: tree, i: int): tree * tree = 
  let 
    val (l1, l2) = truncateList(preorder (ctree), i)
  in
    (construct (l1, Empty), construct (l2, Empty))
  end
*)
(*
val tree1 = Node (Empty,#"a",Node (Node (Empty,#"b",Empty),#"c",Empty)) : tree
val tree2 = Node (Node (Empty,#"a",Empty),#"b",Node (Empty,#"c",Empty)) : tree
val tree3 =
  Node
    (Node (Empty,#"a",Empty),#"b",
     Node (Empty,#"c",Node (Node (Empty,#"d",Empty),#"e",Empty))) : tree
val tree8 = 
  Node(
    Node(
      Node(Empty, #"a", Empty),
      #"b",
      Node(
        Empty,
        #"c",
        Node(Empty, #"d", Empty)
      )
    ),
    #"e",
    Node(
      Node(
        Node(Empty, #"f", Empty),
        #"g",
        Empty
      ),
      #"h",
      Empty
    )
  )

val tree7 = 
  Node(
    Node(
      Empty,
      #"a",
      Node(Empty, #"b", Empty)
    ),
    #"c",
    Node(
      Node(Empty, #"d", Empty),
      #"e",
      Empty
    )
  )
*)
(* Right-heavy tree with multiple levels *)
val tree4 = 
  Node(
    Node(Empty, #"a", Empty),
    #"b",
    Node(
      Node(Empty, #"c", Empty),
      #"d",
      Node(
        Empty,
        #"e",
        Node(Empty, #"f", Empty)
      )
    )
  )
(*
(* Left-heavy tree with multiple levels *)
val tree5 = 
  Node(
    Node(
      Node(
        Node(Empty, #"a", Empty),
        #"b",
        Empty
      ),
      #"c",
      Node(Empty, #"d", Empty)
    ),
    #"e",
    Empty
  )
*)
(*
val pre_ans1 = preorder tree1
val pre_ans2 = preorder tree2
val pre_ans3 = preorder tree3
*)
(*
val ans4 = splitN (tree3, 2)
val ans5 = splitN (tree5, 3)
val ans8 = splitN (tree8, 4)
val ans8' = splitN (tree8, 0)
val ans8''= splitN (tree8, 8)
val test8 = ans8' = ans8''
val ans7 = splitN (tree7, 2)
*)
(* Old leftmost' and Old rightmost' *)
(*
fun leftmost' (Empty: tree): tree = Empty (* Handle empty case *)
  | leftmost' (Node(Empty, _, tR)) = tR   (* Found leftmost! *)
  | leftmost' (Node(tL, c, tR)) = 
      let val newLeft = leftmost' tL
      in Node(newLeft, c, tR)  
      end

fun rightmost' (Empty: tree): tree = Empty (* Handle empty case *)
  | rightmost' (Node(tL, _, Empty)) = tL   (* Found rightmost! *)
  | rightmost' (Node(tL, c, tR)) = 
      let val newRight = rightmost' tR
      in Node(tL, c, newRight)  
      end
*)
(* ====================================================================== *)
(* LEGACY *)
(* leftmost' and rightmost' are implemented in tail recursion with fn accumulator *)
(* REQUIRES: t is non-empty tree *)

fun leftmost' (Empty: tree): tree = Empty
  | leftmost' (t) =
    (* Tail-recursive helper that builds the result tree as it goes *)
    let
      fun helper (Empty: tree, fn_acc: tree -> tree): tree = fn_acc Empty
        | helper (Node (Empty, _, tR), fn_acc) = fn_acc tR
        | helper (Node (tL, c, tR), fn_acc) =
          helper (tL, fn newLeft => fn_acc (Node (newLeft, c, tR)))
    in
      helper (t, fn x => x)
    end

fun rightmost' (Empty: tree): tree = Empty
  | rightmost' (t) =
    (* Tail-recursive helper that builds the result tree as it goes *)
  let
    fun helper (Empty: tree, fn_acc: tree -> tree): tree = fn_acc Empty
      | helper (Node (tL, _, Empty), fn_acc) = fn_acc tL
      | helper (Node (tL, c, tR), fn_acc) = 
      	helper (tR, fn newRight => fn_acc (Node (tL, c, newRight)))
  in
    helper (t, fn x => x)
  end
(* splitN: tree * int -> tree * tree
   REQUIRES: t is a non-empty tree, i <= size t
   ENSURES: splits the tree into two trees at the i-th element (based on leftmost elements)
 *)
fun splitN (Empty, _) = (Empty, Empty)
  | splitN (t, i) =
    let
      val totalSize = size t
      
      fun safeLeftmost (t, 0) = t
        | safeLeftmost (Empty, _) = Empty
        | safeLeftmost (t, n) =
          safeLeftmost (leftmost' t, n-1)
      
      fun safeRightmost (t, 0) = t
        | safeRightmost (Empty, _) = Empty
        | safeRightmost (t, n) =
          safeRightmost (rightmost' t, n-1)
    in
      if i <= 0 then (Empty, t)
      else if i >= totalSize then (t, Empty)
      else (safeRightmost (t, totalSize - i), safeLeftmost (t, i)) (*t1, t2*)
    end
(*
val localTest = splitN (Node  (Node( Node( Node(Empty, #"a", Empty), #"b", Node(Empty, #"c", Empty)), #"d", Empty), #"e", Node(Empty, #"f", Empty)), 3)
(* Base Cases *)
val test_splitN_empty = splitN(Empty, 0) = (Empty, Empty)
val test_splitN_single = splitN(Node(Empty, #"a", Empty), 1) = (Node(Empty, #"a", Empty), Empty)

(* Edge Cases with Split Points *)
val test_splitN_zero = splitN(tree3, 0) = (Empty, tree3)
val test_splitN_full = splitN(tree3, size tree3) = (tree3, Empty)

val test_splitN_oversize = splitN(tree3, size tree3 + 1) = (tree3, Empty)

(* Complex Tree Structures *)
val test_splitN_zigzag = 
  let val zigzagTree = 
    Node(
      Node(Empty, #"b", Node(Node(Empty, #"a", Empty), #"d", Empty)),
      #"e",
      Node(Empty, #"c", Empty)
    )
  in
    splitN(zigzagTree, 2) = 
    (Node(Empty, #"b", Node(Empty, #"a", Empty)),
     Node(Node(Empty, #"d", Empty), #"e", Node(Empty, #"c", Empty)))
  end

(* Deep Unbalanced Trees *)
val test_splitN_deep_left = 
  let val deepLeftTree = 
    Node(
      Node(Node(Node(Empty, #"a", Empty), #"b", Empty), #"c", Empty),
      #"d",
      Node(Empty, #"e", Empty))
  in
    splitN(deepLeftTree, 3) = (Node (Node (Node (Empty,#"a",Empty),#"b",Empty),#"c",Empty),
   Node (Empty,#"d",Node (Empty,#"e",Empty))) 
  end

(* Split at Different Positions *)
val test_splitN_mid = 
  let val balancedTree = 
    Node(
      Node(Empty, #"a", Node(Empty, #"b", Empty)),
      #"c",
      Node(Node(Empty, #"d", Empty), #"e", Empty)
    )
  in
    splitN(balancedTree, 3) =(Node (Node (Empty,#"a",Node (Empty,#"b",Empty)),#"c",Empty),
   Node (Node (Empty,#"d",Empty),#"e",Empty))
  end

(* Sequence of Operations *)
val test_splitN_sequential = 
  let val (t1, t2) = splitN(tree4, 2)
      val (t3, t4) = splitN(t2, 2)
  in
    (size t1 = 2) andalso (size t3 = 2) andalso (size t4 > 0)
  end

(* Complex Pattern Testing *)
val test_splitN_pattern = 
  let val patternTree = 
    Node(
      Node(Node(Empty, #"a", Empty), #"b", Node(Empty, #"c", Empty)),
      #"d",
      Node(Node(Empty, #"e", Empty), #"f", Node(Empty, #"g", Empty))
    )
  in
    splitN(patternTree, 4) = (Node
     (Node (Node (Empty,#"a",Empty),#"b",Node (Empty,#"c",Empty)),#"d",Empty),
   Node (Node (Empty,#"e",Empty),#"f",Node (Empty,#"g",Empty)))
  end

(* Test with Repeated Characters *)
val test_splitN_repeated = 
  let val repeatedTree = 
    Node(
      Node(Empty, #"a", Node(Empty, #"a", Empty)),
      #"a",
      Node(Empty, #"a", Empty)
    )
  in
    splitN (repeatedTree, 2) = (Node (Empty,#"a",Node (Empty,#"a",Empty)),
   Node (Empty,#"a",Node (Empty,#"a",Empty)))
  end

*)

(* Create a left-spine degenerate tree *)
val degenerateLeft = 
  Node(
    Node(
      Node(
        Node(
          Node(Empty, #"a", Empty),
          #"b", 
          Empty
        ),
        #"c",
        Empty
      ),
      #"d",
      Empty
    ),
    #"e",
    Empty
  )

(* Test cases for splitN on degenerate tree *)
val test_splitN_degenerate1 = splitN(degenerateLeft, 2) = 
  (Node(Node(Empty, #"a", Empty), #"b", Empty),
   Node(Node(Node(Empty, #"c", Empty), #"d", Empty), #"e", Empty))

val test_splitN_degenerate2 = splitN(degenerateLeft, 3) = 
  (Node(Node(Node(Empty, #"a", Empty), #"b", Empty), #"c", Empty),
   Node(Node(Empty, #"d", Empty), #"e", Empty))

val test_splitN_degenerate3 = splitN(degenerateLeft, 4) = 
  (Node(Node(Node(Node(Empty, #"a", Empty), #"b", Empty), #"c", Empty), #"d", Empty),
   Node(Empty, #"e", Empty))

(* Verify properties *)
val test_degenerate_size = size degenerateLeft = 5
val test_degenerate_height = height degenerateLeft = 5
val test_degenerate_inorder = inorder degenerateLeft = [#"a", #"b", #"c", #"d", #"e"]
(* ====================================================================== *)
(* LEGACY *)
(*
fun leftmost (Empty: tree): char * tree = (#" ", Empty) (* Handle empty case *)
  | leftmost (Node(Empty, c, tR)) = (c, tR)  (* Found leftmost! *)
  | leftmost (Node(tL, c, tR)) = 
      let val (x, newLeft) = leftmost tL
      in (x, Node(newLeft, c, tR))  
      end
*)
fun leftmost (Empty: tree): char * tree = (#" ", Empty)
  | leftmost (Node(tL, c, tR)) = 
    let
      fun helper (Empty: tree, fn_acc: tree -> tree): char * tree = (#" ", fn_acc Empty)
        | helper (Node(Empty, c, tR), fn_acc) = (c, fn_acc tR)
        | helper (Node(tL, c, tR), fn_acc) =
          helper (tL, fn newLeft => fn_acc (Node(newLeft, c, tR)))
    in
      helper (Node(tL, c, tR), fn x => x)
    end 
(*
val leftmost_ans5 = leftmost tree5
val test = leftmost_ans5 = leftmost tree5
*)
(*
(* Test cases for leftmost *)

(* Empty tree *)
val test1 = leftmost Empty = (#" ", Empty)

(* Single node *)
val test2 = leftmost (Node(Empty, #"a", Empty)) = (#"a", Empty)

(* Deep left path *)
val deepLeft = 
  Node(
    Node(
      Node(
        Node(Empty, #"a", Empty),
        #"b",
        Node(Empty, #"c", Empty)
      ),
      #"d",
      Empty
    ),
    #"e",
    Node(Empty, #"f", Empty)
  )
val test3 = leftmost deepLeft = (#"a", 
  Node(
    Node(
      Node(Empty, #"b", Node(Empty, #"c", Empty)),
      #"d",
      Empty
    ),
    #"e",
    Node(Empty, #"f", Empty)
  ))

(* Zigzag path to leftmost *)
val zigzag = 
  Node(
    Node(
      Empty,
      #"b",
      Node(
        Node(Empty, #"a", Empty),
        #"c",
        Empty
      )
    ),
    #"d",
    Empty
  )
val test4 = leftmost zigzag = (#"b", 
Node (Node (Node (Empty,#"a",Empty),#"c",Empty),#"d",Empty))

(* Right-heavy tree with leftmost in left subtree *)
val rightHeavy = 
  Node(
    Node(Empty, #"a", Empty),
    #"b",
    Node(
      Node(Empty, #"c", Empty),
      #"d",
      Node(
        Node(Empty, #"e", Empty),
        #"f",
        Node(Empty, #"g", Empty)
      )
    )
  )
val test5 = leftmost rightHeavy = (#"a",
  Node(Empty, #"b",
    Node(
      Node(Empty, #"c", Empty),
      #"d",
      Node(
        Node(Empty, #"e", Empty),
        #"f",
        Node(Empty, #"g", Empty)
      )
    )
  ))

(* Tree with multiple possible paths but only one leftmost *)
val multiPath = 
  Node(
    Node(
      Node(Empty, #"b", Empty),
      #"a",
      Node(Empty, #"b", Empty)
    ),
    #"c",
    Node(
      Node(Empty, #"b", Empty),
      #"d",
      Empty
    )
  )
val test6 = leftmost multiPath = (#"b",
  Node(
    Node(Empty, #"a", Node(Empty, #"b", Empty)),
    #"c",
    Node(
      Node(Empty, #"b", Empty),
      #"d",
      Empty
    )
  ))
*)
fun halves (t: tree) : tree * char * tree = 
  let
    val sizeT = size t
    val (t1, t2') = splitN(t, sizeT div 2)
    val (x, t2) = leftmost t2'
  in
    (t1, x, t2)
  end
(*
(* Helper function to verify halves criteria *)
fun verifyHalvesCriteria(t: tree, result: tree * char * tree): bool =
    let
        val (t1, x, t2) = result
        (* Criterion 1: inorder t = (inorder t1) @ [x] @ (inorder t2) *)
        val inorderCorrect = inorder t = (inorder t1) @ [x] @ (inorder t2)
        (* Criterion 2: size t1 = (size t) div 2 *)
        val sizeCorrect = size t1 = (size t) div 2
        (* Criterion 3 & 4: heights of t1 and t2 <= height t *)
        val heightCorrect = height t1 <= height t andalso height t2 <= height t
    in
        inorderCorrect andalso sizeCorrect andalso heightCorrect
    end

(* Test Cases *)

(* Test 1: Minimal non-empty tree *)
val test_halves_minimal = 
    let
        val input = Node(Empty, #"a", Empty)
        val result = halves input
    in
        verifyHalvesCriteria(input, result)
    end

(* Test 2: Perfect balanced binary tree *)
val test_halves_balanced = 
    let
        val input = Node(
            Node(Node(Empty, #"a", Empty), #"b", Node(Empty, #"c", Empty)),
            #"d",
            Node(Node(Empty, #"e", Empty), #"f", Node(Empty, #"g", Empty))
        )
        val result = halves input
    in
        verifyHalvesCriteria(input, result)
    end

(* Test 3: Left-heavy unbalanced tree *)
val test_halves_left_heavy = 
    let
        val input = Node(
            Node(Node(Node(Empty, #"a", Empty), #"b", Empty), #"c", Empty),
            #"d",
            Node(Empty, #"e", Empty)
        )
        val result = halves input
    in
        verifyHalvesCriteria(input, result)
    end

(* Test 4: Right-heavy unbalanced tree *)
val test_halves_right_heavy = 
    let
        val input = Node(
            Node(Empty, #"a", Empty),
            #"b",
            Node(Empty, #"c", Node(Node(Empty, #"d", Empty), #"e", Empty))
        )
        val result = halves input
    in
        verifyHalvesCriteria(input, result)
    end

(* Test 5: Tree with repeated elements *)
val test_halves_repeated = 
    let
        val input = Node(
            Node(Node(Empty, #"a", Empty), #"a", Node(Empty, #"a", Empty)),
            #"a",
            Node(Node(Empty, #"a", Empty), #"a", Empty)
        )
        val result = halves input
    in
        verifyHalvesCriteria(input, result)
    end

(* Test 6: Zigzag pattern tree *)
val test_halves_zigzag = 
    let
        val input = Node(
            Node(Empty, #"b", Node(Node(Empty, #"a", Empty), #"d", Empty)),
            #"e",
            Node(Empty, #"c", Node(Empty, #"f", Empty))
        )
        val result = halves input
    in
        verifyHalvesCriteria(input, result)
    end

(* Test 7: Large tree with mixed patterns *)
val test_halves_complex = 
    let
        val input = Node(
            Node(
                Node(Node(Empty, #"a", Empty), #"b", Node(Empty, #"c", Empty)),
                #"d",
                Node(Empty, #"e", Node(Empty, #"f", Empty))
            ),
            #"g",
            Node(
                Node(Empty, #"h", Empty),
                #"i",
                Node(Empty, #"j", Node(Empty, #"k", Empty))
            )
        )
        val result = halves input
    in
        verifyHalvesCriteria(input, result)
    end
*)
(* ====================================================================== *)
(* rebalance: tree -> tree
   ENSURES: rebalances the tree by splitting it into two halves and 
            reconstructing it with the middle element as root
 *)
fun rebalance (Empty: tree): tree = Empty
  | rebalance t =
    let
      val (t1, c, t2) = halves(t)
    in
      Node (rebalance t1, c, rebalance t2)
    end
(*
(* Helper function to verify rebalance properties *)
fun verifyRebalance(input: tree, result: tree): bool =
    let
        (* Check if result is balanced *)
        val isBalanced = balanced result
        (* Check if elements are preserved in same order *)
        val sameOrder = inorder input = inorder result
        (* Check if sizes match *)
        val sizePreserved = size input = size result
    in
        isBalanced andalso sameOrder andalso sizePreserved
    end

(* Test Cases for rebalance *)

(* Test 1: Already balanced tree *)
val test_rebalance_balanced = 
    let
        val input = Node(
            Node(Empty, #"a", Empty),
            #"b",
            Node(Empty, #"c", Empty)
        )
    in
        verifyRebalance(input, rebalance input)
    end

(* Test 2: Left-heavy unbalanced tree *)
val test_rebalance_left_heavy = 
    let
        val input = Node(
            Node(
                Node(
                    Node(Empty, #"a", Empty),
                    #"b",
                    Empty
                ),
                #"c",
                Empty
            ),
            #"d",
            Empty
        )
    in
        verifyRebalance(input, rebalance input)
    end

(* Test 3: Right-heavy unbalanced tree *)
val test_rebalance_right_heavy = 
    let
        val input = Node(
            Empty,
            #"a",
            Node(
                Empty,
                #"b",
                Node(
                    Empty,
                    #"c",
                    Node(Empty, #"d", Empty)
                )
            )
        )
    in
        verifyRebalance(input, rebalance input)
    end
(* Test 4: Zigzag pattern tree *)
val test_rebalance_zigzag = 
    let
        val input = Node(
            Node(Empty, #"b", 
                Node(Node(Empty, #"a", Empty), 
                    #"d", Empty)
            ),
            #"e",
            Node(Empty, #"c", 
                Node(Empty, #"f", Empty))
        )
    in
        verifyRebalance(input, rebalance input)
    end
(* Test 5: Tree with repeated elements (order critical) *)
val test_rebalance_repeated = 
    let
        val input = Node(
            Node(Node(Empty, #"a", Empty), 
                 #"a", 
                 Node(Empty, #"a", Empty)),
            #"a",
            Node(Empty, #"a", Empty)
        )
    in
        verifyRebalance(input, rebalance input)
    end

(* Test 6: Complex mixed pattern tree *)
val test_rebalance_complex = 
    let
        val input = Node(
            Node(
                Node(Empty, #"a", Empty),
                #"b",
                Node(Empty, #"c", 
                    Node(Empty, #"d", Empty))
            ),
            #"e",
            Node(
                Node(Empty, #"f", Empty),
                #"g",
                Node(
                    Node(Empty, #"h", Empty),
                    #"i",
                    Empty
                )
            )
        )
    in
        verifyRebalance(input, rebalance input)
    end

(* Test 7: Sequential operations *)
val test_rebalance_sequential = 
    let
        val step1 = rebalance tree4
        val step2 = rebalance step1
    in
        verifyRebalance(tree4, step1) andalso
        verifyRebalance(step1, step2) andalso
        inorder tree4 = inorder step2
    end

(* Test 8: Extreme imbalance *)
val test_rebalance_extreme = 
    let
        (* Create a highly right-skewed tree *)
        fun createSkewed(n: int): tree =
            if n = 0 then Empty
            else Node(Empty, chr(ord #"a" + n), createSkewed(n-1))
        
        val input = createSkewed 5  (* a->b->c->d->e->f *)
        val result = rebalance input
    in
        verifyRebalance(input, result)
    end
*)