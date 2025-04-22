(*
Control.Print.printLength := 100;
Control.Print.printDepth := 100;
*)
type key = int 
type 'a entry = key * 'a

datatype 'a rbt = Empty
        | Red of 'a rbt * 'a entry * 'a rbt
        | Black of 'a rbt * 'a entry * 'a rbt

(* Helper function to extract components from nodes *)
fun getComponents (Red (l, kv, r)) = (l, kv, r)
  | getComponents (Black (l, kv, r)) = (l, kv, r)
  | getComponents Empty = raise Fail "Cannot get components of Empty"

fun allLess (Empty: 'a rbt, _: int): bool = true
  | allLess (node, k) =
      let val (left, (k', _), right) = getComponents node
      in
          k' < k andalso 
          allLess (left, k) andalso 
          allLess (right, k)
      end
fun allGreater (Empty: 'a rbt, _: int): bool = true
  | allGreater (node, k) =
      let val (left, (k', _), right) = getComponents node
      in
          k' > k andalso 
          allGreater (left, k) andalso 
          allGreater (right, k)
      end
fun is_bst (Empty: 'a rbt): bool = true
  | is_bst node =
      let val (left, (k, _), right) = getComponents node
      in
          allLess (left, k) andalso 
          allGreater (right, k) andalso
          is_bst left andalso 
          is_bst right
      end
(* Red-Black Tree properties:
   1. Each node is either red or black.
   2. The root is always black.
   3. Red nodes cannot have red children (no two reds in a row).
   4. Every path from a node to its descendant Empty must have the same number of black nodes. *)

fun is_rbt (t: 'a rbt): bool = 
  let
    fun is_red (Red _) = true
      | is_red _ = false
    
    (* Helper function that returns SOME black_height if valid, NONE otherwise *)
    fun check (Empty) = SOME 0  (* Empty nodes are considered black *)
      | check (Red(left, _, right)) =
            let
                val left_bh = check left
                val right_bh = check right
            in
                case (left_bh, right_bh) 
				  of
                    (SOME lbh, SOME rbh) =>
                        if lbh = rbh then
                            (* Check for red-red violation in children *)
                            if (is_red left orelse is_red right) then NONE
                            else SOME lbh  (* Red nodes don't increase black height *)
                        else NONE
                  | _ => NONE
            end
      | check (Black(left, _, right)) =
            let
                val left_bh = check left
                val right_bh = check right
            in
                case (left_bh, right_bh) 
				  of
                    (SOME lbh, SOME rbh) =>
                        if lbh = rbh then SOME (lbh + 1)  (* Black nodes increase height *)
                        else NONE
                  | _ => NONE
            end
  in
    (* First ensure root is Black *)
    case t of
        Empty => true  (* Empty tree is technically valid *)
      | Red _ => false  (* Root cannot be Red *)
      | Black _ =>
            case check t of
                NONE => false
              | SOME _ => is_bst t  (* Only check BST if black_height is valid *)
  end
(* Test cases for is_rbt *)

(* Base cases - testing fundamental RBT properties *)
val test_rbt_empty = is_rbt Empty = true
val test_rbt_single_black = is_rbt (Black(Empty, (1, "one"), Empty)) = true
val test_rbt_single_red = is_rbt (Red(Empty, (1, "one"), Empty)) = false  (* Root must be black *)

(* Property tests - valid structures *)
val valid_small_rbt = Black(
    Red(Black(Empty, (1, "one"), Empty), 
        (2, "two"),
        Black(Empty, (3, "three"), Empty)),
    (4, "four"),
    Red(Black(Empty, (5, "five"), Empty),
        (6, "six"),
        Black(Empty, (7, "seven"), Empty))
)
val test_valid_small = is_rbt valid_small_rbt = true

(* Invalid red-red relationship tests *)
val invalid_red_red = Black(
    Red(Red(Empty, (1, "one"), Empty),  (* Violation: red child has red parent *)
        (2, "two"),
        Empty),
    (3, "three"),
    Empty
)
val test_invalid_red_red = is_rbt invalid_red_red = false

(* Black height violation tests *)
val invalid_black_height = Black(
    Black(Black(Empty, (1, "one"), Empty),  (* Path has 3 black nodes *)
          (2, "two"),
          Empty),
    (3, "three"),
    Black(Empty, (4, "four"), Empty)        (* Path has 2 black nodes *)
)
val test_invalid_black_height = is_rbt invalid_black_height = false

(* Complex valid RBT with maximum allowed red nodes *)
val complex_valid_rbt = Black(
    Red(
        Black(
            Red(Empty, (1, "one"), Empty),
            (2, "two"),
            Red(Empty, (3, "three"), Empty)
        ),
        (4, "four"),
        Black(
            Red(Empty, (5, "five"), Empty),
            (6, "six"),
            Empty
        )
    ),
    (7, "seven"),
    Red(
        Black(Empty, (8, "eight"), Empty),
        (9, "nine"),
        Black(
            Red(Empty, (10, "ten"), Empty),
            (11, "eleven"),
            Empty
        )
    )
)
val test_complex_valid = is_rbt complex_valid_rbt = true

(* BST property violation in valid RB structure *)
val valid_rb_invalid_bst = Black(
    Red(
        Black(Empty, (5, "five"), Empty),  (* Violates BST: 5 > 4 *)
        (4, "four"),
        Black(Empty, (3, "three"), Empty)
    ),
    (6, "six"),
    Red(
        Black(Empty, (7, "seven"), Empty),
        (8, "eight"),
        Black(Empty, (9, "nine"), Empty)
    )
)
val test_valid_rb_invalid_bst = is_rbt valid_rb_invalid_bst = false

(* Edge case - Maximum red nodes without violation *)
val edge_case_reds = Black(
    Red(
        Black(
            Red(Empty, (1, "one"), Empty),
            (2, "two"),
            Empty
        ),
        (3, "three"),
        Black(
            Empty,
            (4, "four"),
            Red(Empty, (5, "five"), Empty)
        )
    ),
    (6, "six"),
    Red(
        Black(Empty, (7, "seven"), Empty),
        (8, "eight"),
        Black(
            Red(Empty, (9, "nine"), Empty),
            (10, "ten"),
            Empty
        )
    )
)
val test_edge_case = is_rbt edge_case_reds = true