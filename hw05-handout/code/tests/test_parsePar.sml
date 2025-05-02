Control.Print.printLength := 100;
Control.Print.printDepth := 100;
use "../delimiters.sml";

(* Helper function to compare pTrees for structural equality *)
fun equalPTrees (empty, empty) = true
  | equalPTrees (nested t1, nested t2) = equalPTrees(t1, t2)
  | equalPTrees (sbs(t1a, t1b), sbs(t2a, t2b)) = 
    equalPTrees(t1a, t2a) andalso equalPTrees(t1b, t2b)
  | equalPTrees (_, _) = false

(* Test runner with detailed output *)
fun runTest (testName: string, input: string, expected: pTree) =
    let
        val result = parsePar(pList_fromString input)
        val passed = equalPTrees(result, expected)
        val message = testName ^ ":\n" ^
                     "  Input: " ^ input ^ "\n" ^
                     "  Expected: " ^ pTree_toString expected ^ "\n" ^
                     "  Got: " ^ pTree_toString result ^ "\n" ^
                     "  Result: " ^ (if passed then "PASSED" else "FAILED") ^ "\n\n"
    in
        print message;
        passed
    end

val test_parsePar = 
  let
    (* Basic Cases *)
    val test1 = runTest("Empty string", "", empty)
    
    val test2 = runTest("Single pair", "()", nested empty)
    
    val test3 = runTest("Double nested", "(())", 
                       nested(nested empty))

    (* Multiple Independent Pairs *)
    val test4 = runTest("Two adjacent pairs", "()()", 
                       sbs(nested empty, nested empty))
    
    val test5 = runTest("Three adjacent pairs", "()()()", 
                       sbs(nested empty, sbs(nested empty, nested empty)))

    (* Complex Nesting *)
    val test6 = runTest("Deep nesting", "((((((()))))))", 
                       nested(nested(nested(nested(nested(nested(nested empty)))))))
    
    val test7 = runTest("Mixed nesting and adjacency", "(())((()))", 
                       sbs(nested(nested empty), nested(nested(nested empty))))

    (* Complex Patterns *)
    val test8 = runTest("Alternating pattern", "(()())", 
                       nested(sbs(nested empty, nested empty)))
    
    val test9 = runTest("Complex mixed pattern", "(()(())())", 
                       nested(sbs(nested empty, sbs(nested(nested empty), nested empty))))

    (* Left/Right Heavy *)
    val test10 = runTest("Left-heavy nesting", "((()())())", 
                        nested(sbs(nested(sbs(nested empty, nested empty)), nested empty)))
    
    val test11 = runTest("Right-heavy nesting", "(()((())))", 
                        nested(sbs(nested empty, nested(nested(nested empty)))))

    (* Boundary Cases - These test valid but tricky inputs *)
    val test12 = runTest("Nested with multiple pairs", "((())(()))", 
                        nested(sbs(nested (nested empty), nested (nested empty))))
    
    val test13 = runTest("Complex right nesting", "(()((()())))", 
                        nested(sbs(nested empty, 
                                 nested (nested(sbs(nested empty, nested empty))))))
    
    val test14 = runTest("Mixed deep nesting", "((()())((())))", 
                        nested(sbs(nested(sbs(nested empty, nested empty)), 
                                 nested (nested(nested empty)))))

  in
    print("\nOverall Test Results:\n");
    print("Total Tests: 14\n");
    print("Passed: " ^ Int.toString(List.length(List.filter (fn x => x) 
          [test1, test2, test3, test4, test5, test6, test7, test8, test9, 
           test10, test11, test12, test13, test14])) ^ "\n");
    List.all (fn x => x) 
          [test1, test2, test3, test4, test5, test6, test7, test8, test9, 
           test10, test11, test12, test13, test14]
  end;

val _ = test_parsePar;

