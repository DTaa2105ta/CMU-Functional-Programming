(* Test file: test_delimiters.sml *)

Control.Print.printLength := 100;
Control.Print.printDepth := 100;
use "../delimiters.sml";

(* Helper function to compare pLists *)
fun comparePList ([], []) = true
  | comparePList (x::xs, y::ys) = x = y andalso comparePList(xs, ys)
  | comparePList _ = false;

(* Helper function to run test and print result *)
fun runTest (testName: string, test: bool) =
    let
        val result = if test then "PASSED" else "FAILED"
        val message = testName ^ ": " ^ result ^ "\n"
    in
        print message;
        test
    end;

val test_flattenPTree = 
  let
    (* Base Cases *)
    val test1 = runTest("Test 1 (Empty tree)", 
                       flattenPTree empty = [])

    val test2 = runTest("Test 2 (Simple nested)", 
                       comparePList(flattenPTree(nested empty), [LPAR, RPAR]))

    val test3 = runTest("Test 3 (Double nested)", 
                       comparePList(flattenPTree(nested (nested empty)), 
                                  [LPAR, LPAR, RPAR, RPAR]))

    (* Side-by-Side Cases *)
    val test4 = runTest("Test 4 (Side-by-Side empty)", 
                       comparePList(
                         flattenPTree(sbs(nested empty, nested empty)),
                         [LPAR, RPAR, LPAR, RPAR]
                       ))

    (* Complex Nested Structures *)
    val test5 = runTest("Test 5 (Nested with Side-by-Side)", 
                       comparePList(
                         flattenPTree(nested(sbs(nested empty, nested empty))),
                         [LPAR, LPAR, RPAR, LPAR, RPAR, RPAR]
                       ))

    (* Mixed Complex Cases *)
    val test6 = runTest("Test 6 (Mixed nesting)", 
                       comparePList(
                         flattenPTree(
                           sbs(
                             nested(nested empty),
                             nested(sbs(nested empty, nested empty))
                           )
                         ),
                         [LPAR, LPAR, RPAR, RPAR, LPAR, LPAR, RPAR, LPAR, RPAR, RPAR]
                       ))

    (* Deeply Nested Structure *)
    val test7 = runTest("Test 7 (Deeply nested)",
                       comparePList(
                         flattenPTree(
                           nested(nested(nested(nested(nested empty))))
                         ),
                         [LPAR, LPAR, LPAR, LPAR, LPAR, RPAR, RPAR, RPAR, RPAR, RPAR]
                       ))

    (* Complex Side-by-Side with Multiple Nestings *)
    val test8 = runTest("Test 8 (Complex Side-by-Side)",
                       comparePList(
                         flattenPTree(
                           sbs(
                             sbs(nested empty, nested(nested empty)),
                             sbs(nested(nested empty), nested empty)
                           )
                         ),
                         [LPAR, RPAR, LPAR, LPAR, RPAR, RPAR, LPAR, LPAR, RPAR, RPAR, LPAR, RPAR]
                       ))

    (* Empty Trees in Complex Structures *)
    val test9 = runTest("Test 9 (Empty with component)",
                       comparePList(
                         flattenPTree(sbs(empty, nested empty)),
                         [LPAR, RPAR]
                       ))

    (* Multiple Empty Combinations *)
    val test10 = runTest("Test 10 (Multiple empties)",
                        comparePList(
                          flattenPTree(sbs(sbs(empty, empty), sbs(empty, nested empty))),
                          [LPAR, RPAR]
                        ))

    val test11 = runTest("Test 11 (Complex nesting)",
                        comparePList(
                          flattenPTree(
                            sbs(
                              nested(nested(sbs(nested empty, nested empty))),
                              nested(sbs(nested(nested empty), nested empty))
                            )
                          ),
                          [LPAR,LPAR,LPAR,RPAR,LPAR,RPAR,RPAR,RPAR,LPAR,LPAR,LPAR,RPAR,RPAR,LPAR,RPAR,RPAR]
                        ))

    (* Test 12: Alternating Nested and Side-by-Side *)
    val test12 = runTest("Test 12 (Alt Nested and Side-by-Side)",
                        comparePList(
                          flattenPTree(
                            sbs(
                              nested(sbs(nested empty, nested(nested empty))),
                              nested(sbs(nested(nested empty), nested empty))
                            )
                          ),
                          [LPAR, LPAR, RPAR, LPAR, LPAR, RPAR, RPAR, RPAR, LPAR, LPAR, LPAR, RPAR, RPAR, LPAR, RPAR, RPAR]
                        ))

    (* Test 13: Maximum Nesting Depth Test *)
    val test13 = runTest("Test 13 (Max Nesting Depth)",
                        comparePList(
                          flattenPTree(
                            nested(nested(nested(nested(nested(nested(nested(nested empty)))))))
                          ),
                          [LPAR, LPAR, LPAR, LPAR, LPAR, LPAR, LPAR, LPAR, RPAR, RPAR, RPAR, RPAR, RPAR, RPAR, RPAR, RPAR]
                        ))

    (* Test 14: Complex Tree with Multiple Empty Nodes *)
    val test14 = runTest("Test 14 (Complex with empties)",
                        comparePList(
                          flattenPTree(
                            sbs(
                              sbs(empty, nested(sbs(empty, nested empty))),
                              sbs(nested(sbs(empty, nested empty)), empty)
                            )
                          ),
                          [LPAR, LPAR, RPAR, RPAR, LPAR, LPAR, RPAR, RPAR]
                        ))

    (* Test 15: Balanced Complex Structure *)
    val test15 = runTest("Test 15 (Balanced Complex)",
                        comparePList(
                          flattenPTree(
                            sbs(
                              nested(sbs(nested(nested empty), nested(nested empty))),
                              nested(sbs(nested(nested empty), nested(nested empty)))
                            )
                          ),
                          [LPAR, LPAR, LPAR, RPAR, RPAR, LPAR, LPAR, RPAR, RPAR, RPAR,
                           LPAR, LPAR, LPAR, RPAR, RPAR, LPAR, LPAR, RPAR, RPAR, RPAR]
                        ))

    (* Test 16: Mixed Depth with Multiple Side-by-Side *)
    val test16 = runTest("Test 16 (Mixed Depth)",
                        comparePList(
                          flattenPTree(
                            sbs(
                              nested(sbs(nested empty, empty)),
                              sbs(
                                nested(nested(sbs(nested empty, nested empty))),
                                nested(empty)
                              )
                            )
                          ),
                          [LPAR, LPAR, RPAR, RPAR, LPAR, LPAR, LPAR, RPAR, LPAR, RPAR, RPAR, RPAR, LPAR, RPAR]
                        ))

    (* Advanced Test Cases *)
    val test17 = runTest("Test 17 (Asymmetric nesting)",
                        comparePList(
                          flattenPTree(
                            sbs(
                              nested(nested(nested empty)),
                              nested empty
                            )
                          ),
                          [LPAR, LPAR, LPAR, RPAR, RPAR, RPAR, LPAR, RPAR]
                        ))

    val test18 = runTest("Test 18 (Empty between nested)",
                        comparePList(
                          flattenPTree(
                            sbs(nested empty, sbs(empty, nested empty))
                          ),
                          [LPAR, RPAR, LPAR, RPAR]
                        ))

  in
    print("\nOverall Test Results:\n");
    print("Total Tests: 18\n");
    print("Passed: " ^ Int.toString(List.length(List.filter (fn x => x) 
          [test1, test2, test3, test4, test5, test6, test7, test8, test9, 
           test10, test11, test12, test13, test14, test15, test16, test17, test18])) ^ "\n");
    List.all (fn x => x) 
          [test1, test2, test3, test4, test5, test6, test7, test8, test9, 
           test10, test11, test12, test13, test14, test15, test16, test17, test18]
  end;

(* Helper functions for creating test trees *)
fun createDeepNestedTree (0: int): pTree = empty
  | createDeepNestedTree n = nested(createDeepNestedTree(n-1))

fun createWideSideBySideTree (0: int): pTree = nested empty
  | createWideSideBySideTree n = sbs(nested empty, createWideSideBySideTree(n-1))

(* Performance Tests with Detailed Output *)
fun timeTest f x =
    let
        val timer = Timer.startRealTimer()
        val result = f x
        val time = Timer.checkRealTimer timer
    in
        (result, Time.toReal time)
    end;

val performanceTest = 
    let
        val _ = print("\nPerformance Tests:\n")
        
        (* Test deep nesting *)
        val (deepResult, deepTime) = timeTest (flattenPTree o createDeepNestedTree) 100
        val _ = print("Deep Tree (depth 100):\n")
        val _ = print("  Length: " ^ Int.toString(length(deepResult)) ^ "\n")
        val _ = print("  Time: " ^ Real.toString(deepTime) ^ " seconds\n")

        (* Test wide structure *)
        val (wideResult, wideTime) = timeTest (flattenPTree o createWideSideBySideTree) 100
        val _ = print("Wide Tree (width 100):\n")
        val _ = print("  Length: " ^ Int.toString(length(wideResult)) ^ "\n")
        val _ = print("  Time: " ^ Real.toString(wideTime) ^ " seconds\n")
    in
        true
    end;

(* Exception Tests with Detailed Output *)
fun test_exceptions () =
    let
        val _ = print("\nException Tests:\n")
        
        val test1 = runTest("Exception Test 1 (Nested)", 
                           (flattenPTree(nested (nested empty)); true) 
                           handle Fail _ => false)
        
        val test2 = runTest("Exception Test 2 (Complex)", 
                           (flattenPTree(sbs(nested empty, nested(nested empty))); true)
                           handle Fail _ => false)
    in
        List.all (fn x => x) [test1, test2]
    end;

val _ = test_exceptions();
val _ = performanceTest;