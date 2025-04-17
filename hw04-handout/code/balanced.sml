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
(*
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
(* We use leftmostCurrent() and counter i so that when i=0 we return the t' *)
(* Other thing, we strictly save leftmost nodes and build it for t1, and we do recursion on t2*)

fun Insert (x: char, Empty: tree): tree = Node(Empty, x, Empty)
  | Insert (x, Node(tL,y,tR)) =
     if x <= y
       then Node (Insert (x, tL), y, tR)
       else Node (tL, y, Insert (x, tR))

fun leftmost' (Empty: tree): char * tree = (#" ", Empty) (* Handle empty case *)
  | leftmost' (Node(Empty, c, tR)) = (c, tR)  (* Found leftmost! *)
  | leftmost' (Node(tL, c, tR)) = 
      let val (x, newLeft) = leftmost' tL
      in (x, Node(newLeft, c, tR))  
      end

fun splitN (Empty: tree, _: int): tree * tree = (Empty, Empty)
  | splitN (Node(_, c, _), i) =
    let
      fun splitN' (ctree: tree, t1_memo: tree, 0: int): tree * tree = (t1_memo, ctree)
        | splitN' (ctree, t1_memo, i) =
          let
            val (c_t1, t2) = leftmost' (ctree)
          in 
            splitN'(t2, Insert(c_t1, t1_memo), i-1)
          end
    in
      splitN' (Node(_, c, _), Node(Empty, c, Empty), i)
    end

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
      Node(Empty, #"e", Empty)
    )
  in
    splitN(deepLeftTree, 3) = 
    (Node(Node(Empty, #"a", Node(Empty, #"b", Empty)), #"c", Empty),
     Node(Empty, #"d", Node(Empty, #"e", Empty)))
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
    splitN(balancedTree, 3)
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
    splitN(patternTree, 4)
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
    splitN(repeatedTree, 2)
  end
(* ================= *)
(* LEGACY *)
(*
fun leftmost (Empty: tree): char * tree = (#" ", Empty) (* Handle empty case *)
  | leftmost (Node(Empty, c, tR)) = (c, tR)  (* Found leftmost! *)
  | leftmost (Node(tL, c, tR)) = 
      let val (x, newLeft) = leftmost tL
      in (x, Node(newLeft, c, tR))  
      end
*)

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
(* fun halves    _ = *)
fun rebalance _ = raise Fail "Unimplemented"

