datatype stree = Empty | Node of stree * string *stree;
(*  *)
fun preorder ( Empty : stree ) : string list = []
  | preorder ( Node (tl, s, tr) ) = 
    let
      val left_elts = preorder tl
      val right_elts = preorder tr
    in
      (s :: left_elts ) @ right_elts
    end
(* preorder ( Node (tl , s, tr) ) = (x :: preorder tl) @ ( preorder tr) *)

fun inorder ( Empty : stree ) : string list = []
  | inorder ( Node (tl, s, tr) ) = inorder tl @ (s :: inorder tr);

fun postorder (Empty : stree ) : string list = []
  | postorder ( Node (tl, s, tr) ) = postorder tl @ postorder tr @ (s :: [])

(* Test *)
(* 
Depth-3 Example Tree:
         "a"
        /   \
     "b"     "c"
    /       /   \
 "d"     "e"    "f"
*)
val sampleTree =
  Node(
    Node(
      Node(Empty, "d", Empty),
      "b",
      Empty
    ),
    "a",
    Node(
      Node(Empty, "e", Empty),
      "c",
      Node(Empty, "f", Empty)
    )
  );

val ["a","b","d","c","e","f"] = preorder(sampleTree);
val ["d","b","a","e","c","f"] = inorder(sampleTree);
val ["d","b","e","f","c","a"] = postorder(sampleTree);
