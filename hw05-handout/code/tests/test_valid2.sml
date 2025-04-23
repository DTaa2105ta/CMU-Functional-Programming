Control.Print.printLength := 100;
Control.Print.printDepth := 100;
use "../delimiters.sml";

(* Test cases for valid2 function *)
val test_valid2 = 
  let
    (* Edge Cases *)
    val test1 = valid2 [] = true                          (* Empty input *)
    val test2 = valid2 [LPAR] = false                     (* Single unmatched left *)
    val test3 = valid2 [RPAR] = false                     (* Single unmatched right *)
    
    (* Basic Matching *)
    val test4 = valid2 [LPAR, RPAR] = true               (* Simple pair *)
    val test5 = valid2 [LPAR, LPAR, RPAR, RPAR] = true   (* Two pairs *)
    
    (* Complex Nesting *)
    val test6 = valid2 [LPAR, LPAR, LPAR, RPAR, RPAR, RPAR] = true  (* Deep nesting *)
    val test7 = valid2 [LPAR, RPAR, LPAR, RPAR, LPAR, RPAR] = true  (* Multiple pairs *)
    
    (* Tricky Cases *)
    val test8 = valid2 [RPAR, LPAR] = false              (* Wrong order *)
    val test9 = valid2 [LPAR, RPAR, RPAR, LPAR] = false  (* Mismatched pairs *)
    val test10 = valid2 [RPAR, RPAR, LPAR, LPAR] = false (* Reversed pairs *)
    
    (* Stress Tests *)
    val test11 = valid2 (List.tabulate(1000, fn x => if x mod 2 = 0 then LPAR else RPAR)) = true  (* Large balanced input *)
    val test12 = valid2 (List.tabulate(999, fn x => if x mod 2 = 0 then LPAR else RPAR)) = false  (* Large unbalanced input *)
    
    (* Pattern Recognition *)
    val test13 = valid2 [LPAR, LPAR, RPAR, LPAR, RPAR, RPAR] = true (* Mixed nesting *)
    val test14 = valid2 [RPAR, RPAR, LPAR, LPAR] = false           (* Invalid prefix *)
    
    (* Boundary Conditions *)
    val test15 = valid2 [LPAR, RPAR, LPAR, RPAR, RPAR] = false    (* Valid prefix with invalid suffix *)
  in
    List.all (fn x => x) [test1, test2, test3, test4, test5, test6, test7, 
                         test8, test9, test10, test11, test12, test13, test14, test15]
  end

val _ = print ("All valid2 tests passed: " ^ Bool.toString test_valid2 ^ "\n")