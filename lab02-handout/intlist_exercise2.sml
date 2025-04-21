fun sumlist ([] : int list) : int = 0
  | sumlist (x :: xs) = x + sumlist(xs);
(* Test *)
val sampleIntList = [1, 2, 3];
val 6 = sumlist(sampleIntList);
(* val 7 = sumlist(sampleIntList); *)