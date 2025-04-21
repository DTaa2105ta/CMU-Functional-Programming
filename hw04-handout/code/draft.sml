fun leftmost' (Empty: tree, i: int): tree = Empty
  | leftmost' (t, i) =
    (* Tail-recursive helper that builds the result tree as it goes *)
    let
      fun helper (Empty: tree, _: int , _: tree -> tree): tree = Empty
        | helper (t, 0, fn_acc) = fn_acc t
        | helper (Node (Empty, _, tR): tree, fn_acc) = fn_acc tR
        | helper (Node (tL, c, tR), i, fn_acc) =
          helper (tL, i-1, fn newLeft => fn_acc (Node (newLeft, c, tR)))
    in
      helper (t, i, fn x => x)
    end

val zigzag = Node(
            Node(Empty, #"b", 
                Node(Node(Empty, #"a", Empty), 
                    #"d", Empty)
            ),
            #"e",
            Node(Empty, #"c", 
                Node(Empty, #"f", Empty)))

val ok = leftmost' (zigzag, 2)
(* Expected output: Node(Empty, #"a", Node(Empty, #"d", Empty)) *)

(* Test cases for leftmost' *)