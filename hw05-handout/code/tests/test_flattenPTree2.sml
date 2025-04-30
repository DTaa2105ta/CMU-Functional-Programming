Control.Print.printLength := 100;
Control.Print.printDepth := 100;
use "../delimiters.sml";

(* Helper function to check if flattenPTree maintains string equality *)
fun checkStringEquality (t: pTree): bool =
    pList_toString(flattenPTree t) = pTree_toString t

(* Helper function to run test and print result with string representations *)
fun runTest (testName: string, tree: pTree) =
    let
        val result = checkStringEquality tree
        val pListStr = pList_toString(flattenPTree tree)
        val pTreeStr = pTree_toString tree
        val message = testName ^ ":\n" ^
                     "  pList_toString: " ^ pListStr ^ "\n" ^
                     "  pTree_toString: " ^ pTreeStr ^ "\n" ^
                     "  Result: " ^ (if result then "PASSED" else "FAILED") ^ "\n"
    in
        print message;
        result
    end

val test_flattenPTree_string_equality = 
  let
    (* Test 1: Empty tree *)
    val test1 = runTest("Empty tree", empty)

    (* Test 2: Single nested empty *)
    val test2 = runTest("Single nested", nested empty)

    (* Test 3: Double nested empty *)
    val test3 = runTest("Double nested", nested(nested empty))

    (* Test 4: Side-by-side empty trees *)
    val test4 = runTest("Side-by-side empty",
                       sbs(nested empty, nested empty))

    (* Test 5: Tricky - nested empty with side-by-side *)
    val test5 = runTest("Nested with side-by-side",
                       nested(sbs(nested empty, nested empty)))

    (* Test 6: Multiple side-by-side with empty *)
    val test6 = runTest("Multiple side-by-side with empty",
                       sbs(sbs(empty, nested empty), 
                           sbs(nested empty, empty)))

    (* Test 7: Deep nesting with empty branches *)
    val test7 = runTest("Deep nesting with empty",
                       nested(nested(sbs(empty, nested(nested empty)))))

    (* Test 8: Asymmetric tree *)
    val test8 = runTest("Asymmetric tree",
                       sbs(nested(nested(nested empty)),
                           nested empty))

    (* Test 9: Complex mixed structure *)
    val test9 = runTest("Complex mixed",
                       sbs(nested(sbs(nested empty, empty)),
                           nested(nested(sbs(empty, nested empty)))))

    (* Test 10: Edge case - empty between nested *)
    val test10 = runTest("Empty between nested",
                        sbs(nested empty, sbs(empty, nested empty)))

  in
    print("\nOverall Test Results:\n");
    print("Total Tests: 10\n");
    print("Passed: " ^ Int.toString(List.length(List.filter (fn x => x) 
          [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10])) ^ "\n");
    List.all (fn x => x) 
          [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10]
  end;

(* Run all tests *)
val _ = test_flattenPTree_string_equality;