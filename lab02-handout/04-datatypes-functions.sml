(****************************************
 *                                      *
 * Datatypes and functions - lecture 04 *
 *                                      *
 ****************************************)


(* Commands for increasing the print length and depth in the repl *)
Control.Print.printLength := 100;
Control.Print.printDepth := 100;


(****************************
 *  Arithmetic expressions  *
 ****************************)

(* 'datatype' is used to declare a new inductive domain.
   Each case is a constructor. *)
datatype exp = Num of int
	     | Plus of exp * exp
	     | Minus of exp * exp
	     | Times of exp * exp
	     | Div of exp * exp;

(* (5 * 8) + 2 *)
val e: exp = Plus(Times(Num 5, Num 8), Num 2);

(* solve e ==> n
 * REQUIRES true
 * ENSURES n is the result of expression e
 *)
fun solve (Num x: exp): int = x
  | solve (Plus (e1, e2)) = solve e1 + solve e2
  | solve (Minus (e1, e2)) = solve e1 - solve e2
  | solve (Times (e1, e2)) = solve e1 * solve e2
  | solve (Div (e1, e2)) = solve e1 div solve e2;



(***********
 *  Lists  *
 ***********)

datatype strlist = Nil
		 | Cons of string * strlist;

(* length l ==> n
 * REQUIRES true
 * ENSURES n is the number of elements in l
 *)
fun length (Nil: strlist): int = 0
  | length (Cons (s, l)) = 1 + length l;
  (* since we do not use s, we can use the wildcard in its place *) 
  (* | length (Cons (_, l)) = 1 + length l; *)

val l: strlist = Cons("f", Cons("u", Cons("n", Nil)));

(* append (l1, l2) ==> l
 * REQUIRES true
 * ENSURES l is the list formed by l1 followed by l2
 *)
fun append (Nil: strlist, l2: strlist): strlist = l2
  | append (Cons (s, l1), l2) = Cons (s, append (l1, l2));

val l1: strlist = Cons("f", Cons("u", Cons("n", Nil)));
val l2: strlist = Cons("c", Cons("t", Cons("i", Cons("o", Cons("n", Nil)))));

(* concat l ==> s
 * REQUIRES true
 * ENSURES s is the concatenation of all elements in l
 *)
fun concat (Nil: strlist): string = ""
  | concat (Cons (s, l)) = s ^ (concat l);

val s: string = concat (append (l1, l2));


(**** Lists in SML *)

(* 'list' is a built-in type. Use nil or [] for the empty list and infix :: for cons *)
val l3: int list = (1 :: (2 :: (3 :: nil)));
val l4: int list = [1, 2, 3];  (* syntactic sugar for the representation above *)
(* append is @ *)
val l5: int list = l3 @ l4;

(* Pattern matching on (built-in) lists *)

(* length l ==> n
 * REQUIRES true
 * ENSURES n is the length of l
 *)
fun length (nil: int list): int = 0
  | length (_ :: l) = 1 + length l;

(* append (l1, l2) ==> l
 * REQUIRES true
 * ENSURES l is the list formed by l1 followed by l2
 *)
fun append ([]: int list, l2: int list): int list = l2
  | append (s :: l1, l2) = s :: append (l1, l2);


(* sum l ==> n
 * REQUIRES true
 * ENSURES n is the sum of elements in l
 *)
fun sum (nil: int list): int = 0
  | sum (x :: l) = x + sum l;



(***********
 *  Trees  *
 ***********)

datatype inttree = Leaf of int
		 | Node of inttree * inttree;

val t1: inttree = Node(Leaf 1, Node (Leaf 2, Leaf 3));

(* size t ==> n
 * REQUIRES true
 * ENSURES n is the number of nodes and leaves in t
 *)
fun size (Leaf _: inttree): int = 1
  | size (Node (tl, tr)) = 1 + (size tl) + (size tr);

(* mirror t ==> t'
 * REQUIRES true
 * ENSURES t' is the tree which is the mirror of t
 *)
fun mirror (Leaf x: inttree): inttree = Leaf x
  | mirror (Node (tl, tr)) = Node (mirror tr, mirror tl);

(* height t ==> n
 * REQUIRES true
 * ENSURES n is the length of the longest path in t
 *)
fun height (Leaf _: inttree): int = 1
  | height (Node (tl, tr)) = 1 + Int.max (height tl, height tr);


(* A tree that holds strings in all nodes *)
(* sEmpty:
   - needed to represent an empty tree
   - using Leaf and Empty in the same datatype would allow us to 
     represent the same tree in two different ways, which is not ideal
*)
datatype stree = Empty
	       | Node of stree * string * stree;

(* size t ==> n
 * REQUIRES true
 * ENSURES n is the number of nodes in t
 *)
fun size (Empty: stree): int = 0
  | size (Node (tl, _, tr)) = 1 + (size tl) + (size tr);

(* height t ==> n
 * REQUIRES true
 * ENSURES n is the length of the longest path in t
 *)
fun height (Empty: stree): int = 0
  | height (Node (tl, _, tr)) = 1 + Int.max (height tl, height tr);

val t1: stree = Node (Node (Node (Empty, "a", Empty),
                            "is",
			    Node (Empty, "tree", Empty)),
                      "this",
                      Node (Empty, "!", Empty));

val t2: stree = Node (Node (Node (Empty, "C", Empty),
                            "B",
			    Node (Empty, "D", Empty)),
                      "A",
		      Node (Empty, "E", Empty));

(* preorder t ==> l
 * REQUIRES true
 * ENSURES l contains the elements of t in a pre-order traversal
 *)
fun preorder (Empty: stree): string list = []
  | preorder (Node (tl, s, tr)) = (s :: preorder tl) @ preorder tr;

(* inorder t ==> l
 * REQUIRES true
 * ENSURES l contains the elements of t in an in-order traversal
 *)
fun inorder (Empty: stree): string list = []
  | inorder (Node (tl, s, tr)) = inorder tl @ (s :: inorder tr);

(* postorder t ==> l
 * REQUIRES true
 * ENSURES l contains the elements of t in a post-order traversal
 *)
fun postorder (Empty: stree): string list = []
  | postorder (Node (tl, s, tr)) = postorder tl @ postorder tr @ [s];


(* Binary search tree: 
 * Invariant: 
   for all nodes Node (tl, d, tr). 
   for all data dl in tl and dr in tr. 
   dl < d < dr
 *)

val words: string list = ["functioning",
			  "functor",
			  "function",
			  "functions",
			  "functional",
			  "funky",
			  "functioned",
			  "funkiest"];

(* insert (s, t) ==> t'
 * REQUIRES t is a sorted tree (or sorted(inorder t))
 * ENSURES sorted(inorder t') andalso in(s, t')
 *
 * As an extra exercise, implement:
 * sorted : string list -> bool
 * in : string * stree -> bool
 *)
fun insert (w: string, Empty: stree): stree = Node (Empty, w, Empty)
  | insert (w, Node (tl, s, tr)) =
    case String.compare (w, s)
     of EQUAL   => Node (tl, s, tr)
      | GREATER => Node (tl, s, insert (w, tr))
      | LESS    => Node (insert (w, tl), s, tr)

(* insertAll (l, t) ==> t'
 * REQUIRES sorted(inorder t)
 * ENSURES sorted(inorder t') andalso in(w, t') for all w in l
 *)
fun insertAll (nil: string list, t: stree): stree = t
  | insertAll (w :: l, t) = insertAll (l, insert (w, t));

val tw: stree = insertAll (words, Empty);

							   
(* For practicing (check the list part!): 
   https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems 
 *)

