Control.Print.printLength := 100;
Control.Print.printDepth := 100;
use "../delimiters.sml";

(* Helper function for comparing pTree2s *)
fun equalPTree2s (empty2, empty2) = true
  | equalPTree2s (nested2(s1, t1), nested2(s2, t2)) = 
    s1 = s2 andalso equalPTree2s (t1, t2)
  | equalPTree2s (sbs2(t1a, t1b), sbs2(t2a, t2b)) = 
    equalPTree2s (t1a, t2a) andalso equalPTree2s (t1b, t2b)
  | equalPTree2s _ = false

(* Test runner with detailed output *)
fun runTest (testName: string, input: pList2, stack: stack2, expected: pTree2) =
    let
        val result = pp2(input, stack)
        val passed = equalPTree2s (result, expected)
        val message = testName ^ ":\n" ^
                     "  Input list: " ^ pList2_toString input ^ "\n" ^
                     "  Stack: " ^ stack2_toString stack ^ "\n" ^
                     "  Expected tree: " ^ pTree2_toString expected ^ "\n" ^
                     "  Got tree: " ^ pTree2_toString result ^ "\n" ^
                     "  Result: " ^ (if passed then "PASSED" else "FAILED") ^ "\n\n"
    in
        print message;
        passed
    end

val test_pp2 = 
  let
    (* Basic Cases *)
    val test1 = runTest("Empty input with empty stack",
                       [], 
                       [],
                       empty2)

    val test2 = runTest("Single closing tag with matching open",
                       pList2_fromString ")tag",
                       [OPEN2 "tag"],
                       nested2("tag", empty2))

    (* LaTeX-style Cases *)
    val test3 = runTest("LaTeX environment closing",
                       pList2_fromString ")center",
                       [T2(nested2("text", empty2)), OPEN2 "center"],
                       nested2("center", nested2("text", empty2)))

    (* Nested Cases *)
    val test4 = runTest("Nested closing tags",
                       pList2_fromString ")inner)outer",
                       [OPEN2 "inner", OPEN2 "outer"],
                       nested2("outer", nested2("inner", empty2)))

    (* Side-by-side with valid nesting *)
    val test5 = runTest("Side-by-side environments",
                       [],
                       [T2(nested2("p2", empty2)), 
                        T2(nested2("p1", empty2))],
                       sbs2(nested2("p1", empty2),
                            nested2("p2", empty2)))

    (* Complex LaTeX-style *)
    val test6 = runTest("Complex LaTeX structure",
                       pList2_fromString ")equation)align",
                       [T2(nested2("math", empty2)), 
                        OPEN2 "equation",
                        OPEN2 "align"],
                       nested2("align", 
                             nested2("equation",
                                    nested2("math", empty2))))

    (* HTML-style structure *)
    val test7 = runTest("HTML nested structure",
                       pList2_fromString ")p)div",
                       [T2(nested2("text", empty2)),
                        OPEN2 "p",
                        OPEN2 "div"],
                       nested2("div",
                             nested2("p",
                                    nested2("text", empty2))))

    (* Complex document structure *)
    val test8 = runTest("Document structure",
                       pList2_fromString ")section)chapter",
                       [T2(nested2("para", empty2)),
                        OPEN2 "section",
                        OPEN2 "chapter"],
                       nested2("chapter",
                             nested2("section",
                                    nested2("para", empty2))))

    (* Mixed environments *)
    val test9 = runTest("Mixed environments",
                       pList2_fromString ")proof)theorem",
                       [T2(nested2("lemma", empty2)),
                        OPEN2 "proof",
                        OPEN2 "theorem"],
                       nested2("theorem",
                             nested2("proof",
                                    nested2("lemma", empty2))))

    (* Complex nesting with side-by-side *)
    val test10 = runTest("Complex valid structure",
                       pList2_fromString ")inner",
                       [T2(sbs2(nested2("a", empty2),
                               nested2("b", empty2))),
                        OPEN2 "inner"],
                       nested2("inner",
                             sbs2(nested2("a", empty2),
                                  nested2("b", empty2))))

  in
    print("\nOverall Test Results:\n");
    print("Total Tests: 10\n");
    print("Passed: " ^ Int.toString(List.length(List.filter (fn x => x) 
          [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10])) ^ "\n");
    List.all (fn x => x) 
          [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10]
  end

val _ = test_pp2;