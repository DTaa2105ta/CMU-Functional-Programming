val ans1 = Node (Empty,"a",Node (Node (Empty,"b",Empty),"c",Empty)) : itree
val ans2 = Node (Node (Empty,"a",Empty),"b",Node (Empty,"c",Empty)) : tree
val ans3 =
  Node
    (Node (Empty,"a",Empty),"b",
     Node (Empty,"c",Node (Node (Empty,"d",Empty),"e",Empty))) : itree
